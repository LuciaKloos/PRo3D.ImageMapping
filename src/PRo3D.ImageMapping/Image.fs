namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO
open System.Runtime.InteropServices

open HDF.PInvoke

open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE

open System.Text.Json
open System.Collections.Concurrent

module Shaders = 
    open FShade
    open Aardvark.Rendering.Effects

    let instrumentSampler = 
        sampler2d {
            texture uniform?InstrumentImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let colormapTextureSampler =
        sampler2d {
            texture uniform?ColormapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let rgbCompositeSampler =
        sampler2d {
            texture uniform?RgbCompositeTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue
        member x.UseFalseColor : bool = uniform?UseFalseColor
        member x.DataType : int = uniform?DataType
        member x.OverlayMax : V2d = uniform?OverlayMax
        member x.OverlayMin : V2d = uniform?OverlayMin

    let hshColors (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X 
            let remappedClampedNormalizedXInt16 =
                ((min uniform.MaxValue (max uniform.MinValue (hshValueX * 65000.0))) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remappedClampedNormalizedXFloat =
                (hshValueX - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remapClampNormalize =
                if uniform.UseFalseColor then
                    V4d(
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        1.0
                    )
                else 
                    colormapTextureSampler.Sample(V2d ((if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16), 0.0))
            return remapClampNormalize
        }

    // only displays the finished RGB texture. Only samples at current texture coordinate
    let displayRgbComposite (v : Vertex) =
        fragment {
            return rgbCompositeSampler.Sample(v.tc)
        }

module Image =

    let initialPath = ""

    let minValue = {
        value   = 0.0
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }

    let maxValue = {
        value   = 0.0
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }

    type private MbiBandInfo =
        {
            index      : int
            filePath   : string
            label      : Option<string>
            wavelength : Option<float>
            exposure   : Option<float>
        }


    type private NcProductKind =
        | Reflectance
        | ReflectanceUncertainty
        | Mask

    type private NcDatasetInfo =
        {
            path        : string
            datasetPath : string
            width       : int
            height      : int
            bands       : int
            productKind : NcProductKind
        }

    // caches to keep already-read metadata and bands in memory so changing one RGB ratio band
    // does not reread the other five bands from disk
    let private ncDatasetInfoCache =
        ConcurrentDictionary<string, NcDatasetInfo>()

    let private ncRawBandCache =
        ConcurrentDictionary<string, int * int * int * float[]>()

    let private normalizedPath (path : string) =
        Path.GetFullPath(path).ToLowerInvariant()

    let private ncBandCacheKey (path : string) (datasetPath : string) (bandIndex : int) = 
        sprintf "%s|%s|%d" (normalizedPath path) datasetPath bandIndex

    let private emitMaskBandNames =
        [
            "Cloud Flag"
            "Cirrus Flag"
            "Water Flag"
            "Spacecraft Flag"
            "Dilated Cloud Flag"
            "AOD550"
            "H2O (g cm-2)"
            "Aggregate Flag"
            "SpecTf-Cloud Probability"
            "SpecTf-Cloud Flag"
            "SpecTf-Buffer Distance"
        ]

    let private isNcFile (path : string) =
        String.Equals(Path.GetExtension(path), ".nc", StringComparison.OrdinalIgnoreCase)

    let private isReflectanceNcFileName (path : string) =
        let name = Path.GetFileName(path).ToUpperInvariant()
        (name.Contains("_L2A_RFL_") || name.Contains("_RFL_"))
        && not (name.Contains("_RFLUNCERT_"))
        && not (name.Contains("_MASK_"))

    let private tryResolveNcPathToLoad (path : string) : Option<string> =
        let fullPath = Path.GetFullPath path

        if File.Exists fullPath && isNcFile fullPath then
            Some fullPath

        elif Directory.Exists fullPath then
            let ncFiles =
                Directory.GetFiles(fullPath, "*.nc", SearchOption.TopDirectoryOnly)
                |> Array.toList

            match ncFiles |> List.tryFind isReflectanceNcFileName with
            | Some rflPath ->
                Log.warn "Selected a directory, not a NetCDF file. Loading EMIT reflectance file found in that directory: %s" rflPath
                Some rflPath

            | None ->
                match ncFiles with
                | firstNc :: _ ->
                    Log.warn "Selected a directory, not a NetCDF file. No EMIT RFL file was found, so loading first .nc file instead: %s" firstNc
                    Some firstNc
                | [] ->
                    None

        else
            None

    let private closeHdf5 (close : int64 -> int) (id : int64) =
        if id >= 0L then close id |> ignore

    let private ncProductKindFromPath (path : string) =
        let fileName = Path.GetFileName(path).ToUpperInvariant()

        if fileName.Contains("_L2A_RFLUNCERT_") || fileName.Contains("_RFLUNCERT_") then
            ReflectanceUncertainty
        elif fileName.Contains("_L2A_MASK_") || fileName.Contains("_MASK_") then
            Mask
        else
            // EMIT_L2A_RFL_*.nc is the product that contains the hyperspectral cube.
            Reflectance

    let private ncDatasetPathForKind kind =
        match kind with
        | Reflectance -> "reflectance"
        | ReflectanceUncertainty -> "reflectance_uncertainty"
        | Mask -> "mask"

    let private trySaveDebugBmp
        (width : int)
        (height : int)
        (rgbBytes : byte[])
        (path : string) =

        try
            let directory =
                Path.GetDirectoryName path

            if not (String.IsNullOrWhiteSpace directory) then
                Directory.CreateDirectory directory |> ignore

            use stream =
                File.Create path

            use writer =
                new BinaryWriter(stream)

            let rowStride =
                ((width * 3 + 3) / 4) * 4

            let padding =
                Array.zeroCreate<byte> (rowStride - width * 3)

            let pixelDataSize =
                rowStride * height

            let fileSize =
                54 + pixelDataSize

            // BMP header
            writer.Write(byte 0x42) // B
            writer.Write(byte 0x4D) // M
            writer.Write(int32 fileSize)
            writer.Write(int16 0)
            writer.Write(int16 0)
            writer.Write(int32 54)

            // DIB header
            writer.Write(int32 40)
            writer.Write(int32 width)
            writer.Write(int32 height)
            writer.Write(int16 1)
            writer.Write(int16 24)
            writer.Write(int32 0)
            writer.Write(int32 pixelDataSize)
            writer.Write(int32 2835)
            writer.Write(int32 2835)
            writer.Write(int32 0)
            writer.Write(int32 0)

            // BMP stores rows bottom-up and pixels as BGR
            for y = height - 1 downto 0 do
                for x = 0 to width - 1 do
                    let index =
                        (y * width + x) * 3

                    let r =
                        rgbBytes.[index + 0]

                    let g =
                        rgbBytes.[index + 1]

                    let b =
                        rgbBytes.[index + 2]

                    writer.Write(b)
                    writer.Write(g)
                    writer.Write(r)

                if padding.Length > 0 then
                    writer.Write(padding)

            Log.warn "Saved debug RGB composite to %s" path

        with error ->
            Log.warn "Could not save debug RGB composite: %s" error.Message

    let private tryOpenHdf5Dataset (path : string) (datasetPath : string) : Result<int64 * int64, string> =
        try
            let fileId = H5F.``open``(path, H5F.ACC_RDONLY)

            if fileId < 0L then
                Result.Error (sprintf "Could not open NetCDF/HDF5 file: %s" path)
            else
                let datasetId = H5D.``open``(fileId, datasetPath)

                if datasetId < 0L then
                    closeHdf5 H5F.close fileId
                    Result.Error (sprintf "The NetCDF file does not contain a dataset named '%s'." datasetPath)
                else
                    Result.Ok (fileId, datasetId)
        with error ->
            Result.Error error.Message

    let private tryReadNcDatasetInfoUncached (path : string) : Result<NcDatasetInfo, string> =
        let fullPath = Path.GetFullPath path
        let kind = ncProductKindFromPath fullPath
        let datasetPath = ncDatasetPathForKind kind

        match tryOpenHdf5Dataset fullPath datasetPath with
        | Result.Error error ->
            Result.Error error

        | Result.Ok (fileId, datasetId) ->
            let mutable spaceId = -1L

            try
                spaceId <- H5D.get_space datasetId

                if spaceId < 0L then
                    Result.Error (sprintf "Could not read the dataspace of NetCDF dataset '%s'." datasetPath)
                else
                    let dims = Array.zeroCreate<uint64> 3   // expects shape: height x width x bands
                    let maxDims = Array.zeroCreate<uint64> 3
                    let rank = H5S.get_simple_extent_dims(spaceId, dims, maxDims)

                    if rank <> 3 then
                        Result.Error (sprintf "Expected NetCDF dataset '%s' to have rank 3, but it has rank %d." datasetPath rank)
                    else
                        Result.Ok
                            {
                                path = fullPath
                                datasetPath = datasetPath
                                height = int dims.[0]
                                width = int dims.[1]
                                bands = int dims.[2]
                                productKind = kind
                            }
            finally
                closeHdf5 H5S.close spaceId
                closeHdf5 H5D.close datasetId
                closeHdf5 H5F.close fileId

    let private readNcBandAsFloat
        (path : string)
        (datasetPath : string)
        (bandIndex : int)
        : Result<int * int * int * float[], string> =

        match tryReadNcDatasetInfoUncached path with
        | Result.Error error ->
            Result.Error error

        | Result.Ok info ->
            if info.datasetPath <> datasetPath then
                Result.Error (sprintf "NetCDF product %s uses dataset '%s', not '%s'." path info.datasetPath datasetPath)
            elif bandIndex < 0 || bandIndex >= info.bands then
                Result.Error (sprintf "NetCDF band index %d is out of range. Available range is 0..%d." bandIndex (info.bands - 1))
            else
                let cacheKey = ncBandCacheKey info.path info.datasetPath bandIndex

                match ncRawBandCache.TryGetValue cacheKey with
                | true, cached ->
                    Result.Ok cached

                | false, _ ->
                    match tryOpenHdf5Dataset info.path info.datasetPath with
                    | Result.Error error ->
                        Result.Error error

                    | Result.Ok (fileId, datasetId) ->
                        let mutable fileSpaceId = -1L
                        let mutable memSpaceId = -1L
                        let values = Array.zeroCreate<float32> (info.width * info.height)
                        let handle = GCHandle.Alloc(values, GCHandleType.Pinned)

                        try
                            fileSpaceId <- H5D.get_space datasetId

                            if fileSpaceId < 0L then
                                Result.Error (sprintf "Could not read dataspace for '%s'." info.datasetPath)
                            else
                                let start = [| 0UL; 0UL; uint64 bandIndex |]
                                let count = [| uint64 info.height; uint64 info.width; 1UL |]
                                let memDims = [| uint64 info.height; uint64 info.width |]

                                let selectStatus =
                                    H5S.select_hyperslab(
                                        fileSpaceId,
                                        H5S.seloper_t.SET,
                                        start,
                                        null,
                                        count,
                                        null
                                    )

                                if selectStatus < 0 then
                                    Result.Error (sprintf "Could not select band %d from '%s'." bandIndex info.datasetPath)
                                else
                                    memSpaceId <- H5S.create_simple(2, memDims, null)

                                    if memSpaceId < 0L then
                                        Result.Error "Could not create HDF5 memory dataspace."
                                    else
                                        let readStatus =
                                            H5D.read(
                                                datasetId,
                                                H5T.NATIVE_FLOAT,
                                                memSpaceId,
                                                fileSpaceId,
                                                H5P.DEFAULT,
                                                handle.AddrOfPinnedObject()
                                            )

                                        if readStatus < 0 then
                                            Result.Error (sprintf "Could not read band %d from '%s'." bandIndex info.datasetPath)
                                        else
                                            let bandValues =
                                                values
                                                |> Array.map (fun value ->
                                                    let v = float value
                                                    if v <= -9998.0 then Double.NaN else v
                                                )

                                            let result = (info.width, info.height, info.bands, bandValues)
                                            ncRawBandCache.[cacheKey] <- result
                                            Result.Ok result
                        finally
                            handle.Free()
                            closeHdf5 H5S.close memSpaceId
                            closeHdf5 H5S.close fileSpaceId
                            closeHdf5 H5D.close datasetId
                            closeHdf5 H5F.close fileId

    let private tryReadNcVectorFloat (path : string) (datasetPath : string) : Option<float list> =
        try
            match tryOpenHdf5Dataset path datasetPath with
            | Result.Error _ ->
                None

            | Result.Ok (fileId, datasetId) ->
                let mutable spaceId = -1L
                let mutable memSpaceId = -1L

                try
                    spaceId <- H5D.get_space datasetId
                    let dims = Array.zeroCreate<uint64> 1
                    let maxDims = Array.zeroCreate<uint64> 1
                    let rank = H5S.get_simple_extent_dims(spaceId, dims, maxDims)

                    if rank <> 1 then
                        None
                    else
                        let count = int dims.[0]
                        let values = Array.zeroCreate<float32> count
                        let handle = GCHandle.Alloc(values, GCHandleType.Pinned)

                        try
                            let status =
                                H5D.read(
                                    datasetId,
                                    H5T.NATIVE_FLOAT,
                                    H5S.ALL,
                                    H5S.ALL,
                                    H5P.DEFAULT,
                                    handle.AddrOfPinnedObject()
                                )

                            if status < 0 then None else values |> Array.map float |> Array.toList |> Some
                        finally
                            handle.Free()
                finally
                    closeHdf5 H5S.close memSpaceId
                    closeHdf5 H5S.close spaceId
                    closeHdf5 H5D.close datasetId
                    closeHdf5 H5F.close fileId
        with _ ->
            None

    let private tryReadNcVectorInt (path : string) (datasetPath : string) : Option<int list> =
        try
            match tryOpenHdf5Dataset path datasetPath with
            | Result.Error _ ->
                None

            | Result.Ok (fileId, datasetId) ->
                let mutable spaceId = -1L

                try
                    spaceId <- H5D.get_space datasetId
                    let dims = Array.zeroCreate<uint64> 1
                    let maxDims = Array.zeroCreate<uint64> 1
                    let rank = H5S.get_simple_extent_dims(spaceId, dims, maxDims)

                    if rank <> 1 then
                        None
                    else
                        let count = int dims.[0]
                        let values = Array.zeroCreate<int> count
                        let handle = GCHandle.Alloc(values, GCHandleType.Pinned)

                        try
                            let status =
                                H5D.read(
                                    datasetId,
                                    H5T.NATIVE_INT,
                                    H5S.ALL,
                                    H5S.ALL,
                                    H5P.DEFAULT,
                                    handle.AddrOfPinnedObject()
                                )

                            if status < 0 then None else values |> Array.toList |> Some
                        finally
                            handle.Free()
                finally
                    closeHdf5 H5S.close spaceId
                    closeHdf5 H5D.close datasetId
                    closeHdf5 H5F.close fileId
        with _ ->
            None

    let private applyEmitAuxiliaryMasks
        (sourcePath : string)
        (bandIndex : int)
        (values : float[])
        : float[] =

        let result = Array.copy values

        // EMIT uses good_wavelengths=0 for noisy atmospheric absorption regions.
        match tryReadNcVectorInt sourcePath "sensor_band_parameters/good_wavelengths" with
        | Some goodWavelengths ->
            match goodWavelengths |> List.tryItem bandIndex with
            | Some good when good = 0 ->
                for i in 0 .. result.Length - 1 do
                    result.[i] <- Double.NaN
            | _ ->
                ()
        | None ->
            ()

        result

    let private tryGetProperty (name : string) (element : JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>
        if element.TryGetProperty(name, &property) then Some property else None

    let private tryGetString (name : string) (element : JsonElement) =
        match tryGetProperty name element with
        | Some property when property.ValueKind = JsonValueKind.String ->
            property.GetString() |> Option.ofObj
        | _ ->
            None

    let private tryGetInt (name : string) (element : JsonElement) =
        match tryGetProperty name element with
        | Some property when property.ValueKind = JsonValueKind.Number ->
            let mutable value = 0
            if property.TryGetInt32(&value) then Some value else None
        | _ ->
            None

    let private tryGetDouble (name : string) (element : JsonElement) =
        match tryGetProperty name element with
        | Some property when property.ValueKind = JsonValueKind.Number ->
            let mutable value = 0.0
            if property.TryGetDouble(&value) then Some value else None
        | _ ->
            None

    let private tryReadMbiBands (mbiPath : string) =
        try
            let fullMbiPath = Path.GetFullPath mbiPath
            let baseDirectory = Path.GetDirectoryName fullMbiPath

            use document =
                JsonDocument.Parse(File.ReadAllText fullMbiPath)

            match tryGetProperty "mbi_bands" document.RootElement with
            | Some bandsElement when bandsElement.ValueKind = JsonValueKind.Array ->

                bandsElement.EnumerateArray()
                |> Seq.choose (fun bandElement ->
                    match tryGetInt "index" bandElement, tryGetString "file_path" bandElement with
                    | Some index, Some relativePath ->

                        let resolvedPath =
                            if Path.IsPathRooted relativePath then
                                relativePath
                            else
                                Path.Combine(baseDirectory, relativePath)

                        Some
                            {
                                index = index
                                filePath = Path.GetFullPath resolvedPath
                                label = tryGetString "label" bandElement
                                wavelength = tryGetDouble "wavelength" bandElement
                                exposure = tryGetDouble "exposure" bandElement
                            }

                    | _ ->
                        None
                )
                |> Seq.sortBy (fun band -> band.index)
                |> Seq.toList
                |> function
                    | [] -> None
                    | bands -> Some bands

            | _ ->
                None

        with error ->
            Log.warn "Could not read MBI manifest %s: %s" mbiPath error.Message
            None

    let initial = { 
        colorMap = ColorMap.Magma;
        useFalseColor = true;
        selectedChannel = { idx = 0; name = None }
        channelOptions = [];
        dataType = DataType.UInt16;
        defaultMinValues = [minValue.value];
        defaultMaxValues = [maxValue.value];
        inputMinValue = minValue;
        inputMaxValue = maxValue;
        texture = initialPath;

        bandIndex = 0;
        wavelength = None;

        distance = 0;
        time = new DateTime();
    }

    let private getBandAsFloat
        (bandIndex : int)
        (image : TiffReadResult)
        : float[] =
   
        match image.buffers with
        | PixelBuffers.Float32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.UInt16Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.Int16Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.UInt32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.Int32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

    let private percentile
        (fraction : float)
        (sortedValues : float[]) =

        if sortedValues.Length = 0 then
            0.0
        else
            let index =
                fraction * float (sortedValues.Length - 1)
                |> Math.Round
                |> int
                |> max 0
                |> min (sortedValues.Length - 1)

            sortedValues.[index]

    // important, otherwise black
    let private valueToByte
        (gamma : float)
        (minimum : float)
        (maximum : float)
        (value : float) =

        if not (Double.IsFinite value) || maximum <= minimum then
            0uy
        else
            // normalizes & clamps the result
            let normalized = 
                (value - minimum) / (maximum - minimum)
                |> max 0.0
                |> min 1.0

            let safeGamma =
                if Double.IsFinite gamma && gamma > 0.0 then
                    gamma
                else
                    1.0

            // Brightens darker values. Makes dark scientific values more visible.
            // todo make this interactive
            let gammaCorrected = 
                Math.Pow(normalized, safeGamma) // gamma < 1 -> brightens; gamma > 1 -> darkens

            // produces one byte of each of RGB
            gammaCorrected * 255.0
            |> Math.Round
            |> byte

    let private safeRatio
        (minimumSignal : float)
        (numerator : float[])
        (denominator : float[]) =

        if numerator.Length <> denominator.Length then
            invalidArg
                "denominator"
                "Ratio bands must contain the same number of pixels."

        Array.map2
            (fun numeratorValue denominatorValue ->
                if
                    Double.IsFinite numeratorValue &&
                    Double.IsFinite denominatorValue &&
                    Math.Abs denominatorValue > minimumSignal
                then
                    numeratorValue / denominatorValue
                else
                    Double.NaN
            )
            numerator
            denominator

    type private RgbBandSource =
        {
            logicalIndex : int
            filePath     : string
            channelIndex : int
            wavelength   : Option<float>
        }

    type private RgbBandData =
        {
            source : RgbBandSource
            width  : int
            height : int
            values : float[]
        }

    let private availableLogicalBandsMessage (sources : list<RgbBandSource>) =
        sources
        |> List.map (fun source -> source.logicalIndex)
        |> List.distinct
        |> List.sort
        |> List.map string
        |> String.concat ", "

    let private readAdaptiveBandSources
        (images : IndexList<AdaptiveImage>)
        token =

        images
        |> IndexList.toList
        |> List.map (fun image ->
            let selectedChannel =
                image.selectedChannel.GetValue token

            {
                logicalIndex = image.bandIndex.GetValue token
                filePath = image.texture.GetValue token
                channelIndex = selectedChannel.idx
                wavelength = image.wavelength.GetValue token
            }
        )

    let private readBandSourceAsFloat
        (source : RgbBandSource)
        : Result<RgbBandData, string> =

        try
            if String.IsNullOrWhiteSpace source.filePath then
                Result.Error (
                    sprintf
                        "Logical band %d has no TIFF path."
                        source.logicalIndex
                )

            elif not (File.Exists source.filePath) then
                Result.Error (
                    sprintf
                        "Image source for logical band %d does not exist: %s"
                        source.logicalIndex
                        source.filePath
                )

            elif isNcFile source.filePath then
                match tryReadNcDatasetInfoUncached source.filePath with
                | Result.Error error ->
                    Result.Error error

                | Result.Ok info ->
                    match readNcBandAsFloat source.filePath info.datasetPath source.channelIndex with
                    | Result.Error error ->
                        Result.Error error

                    | Result.Ok (width, height, _, values) ->
                        let values =
                            match info.productKind with
                            | Reflectance ->
                                applyEmitAuxiliaryMasks source.filePath source.channelIndex values
                            | ReflectanceUncertainty
                            | Mask ->
                                values

                        Result.Ok
                            {
                                source = source
                                width = width
                                height = height
                                values = values
                            }

            else
                match MultiBandReader.tryReadMultiBandTiff source.filePath false with
                | Result.Error error ->
                    Result.Error error

                | Result.Ok image ->
                    if source.channelIndex < 0 || source.channelIndex >= image.bands then
                        Result.Error (
                            sprintf
                                "Logical band %d points to channel %d in %s, but the available TIFF channel range is 0..%d."
                                source.logicalIndex
                                source.channelIndex
                                source.filePath
                                (image.bands - 1)
                        )
                    else
                        Result.Ok
                            {
                                source = source
                                width = image.width
                                height = image.height
                                values = getBandAsFloat source.channelIndex image
                            }

        with error ->
            Result.Error error.Message


    let private averageBandData
        (bands : list<RgbBandData>)
        : Result<RgbBandData, string> =

        match bands with
        | [] ->
            Result.Error "Cannot average an empty band list."

        | first :: rest ->

            let mismatch =
                rest
                |> List.tryFind (fun band ->
                    band.width <> first.width ||
                    band.height <> first.height ||
                    band.values.Length <> first.values.Length
                )

            match mismatch with
            | Some band ->
                Result.Error (
                    sprintf
                        "Cannot average bands with different dimensions. Band %d is %dx%d, but band %d is %dx%d."
                        band.source.logicalIndex
                        band.width
                        band.height
                        first.source.logicalIndex
                        first.width
                        first.height
                )

            | None ->
                let pixelCount =
                    first.values.Length

                let averaged =
                    Array.init pixelCount (fun i ->
                        let mutable sum = 0.0
                        let mutable count = 0

                        for band in bands do
                            let value = band.values.[i]

                            if Double.IsFinite value then
                                sum <- sum + value
                                count <- count + 1

                        if count > 0 then
                            sum / float count
                        else
                            Double.NaN
                    )

                Result.Ok
                    {
                        first with
                            values = averaged
                    }



    let private readLogicalBand
        (sources : list<RgbBandSource>)
        (logicalBandIndex : int)
        : Result<RgbBandData, string> =

        match sources |> List.tryFind (fun source -> source.logicalIndex = logicalBandIndex) with
        | Some source ->
            readBandSourceAsFloat source

        | None ->
            Result.Error (
                sprintf
                    "Could not find logical RGB band %d. Available logical bands are: %s"
                    logicalBandIndex
                    (availableLogicalBandsMessage sources)
            )

    let private validateSameDimensions
        (bands : list<RgbBandData>)
        : Result<int * int * int, string> =

        match bands with
        | [] ->
            Result.Error "No RGB bands were loaded."

        | first :: rest ->
            let mismatch =
                rest
                |> List.tryFind (fun band ->
                    band.width <> first.width ||
                    band.height <> first.height ||
                    band.values.Length <> first.values.Length
                )

            match mismatch with
            | Some band ->
                Result.Error (
                    sprintf
                        "RGB bands do not have matching dimensions. Band %d is %dx%d, but band %d is %dx%d."
                        band.source.logicalIndex
                        band.width
                        band.height
                        first.source.logicalIndex
                        first.width
                        first.height
                )

            | None ->
                Result.Ok (first.width, first.height, first.values.Length)

    let private readAverageLogicalBand
        (sources : list<RgbBandSource>)
        (averageRadius : int)
        (maxWavelengthDistanceNm : float)
        (centerLogicalIndex : int)
        : Result<RgbBandData, string> =

        match sources |> List.tryFind (fun source -> source.logicalIndex = centerLogicalIndex) with
        | None ->
            Result.Error (sprintf "Could not find logical band %d." centerLogicalIndex)

        | Some centerSource ->

            let candidates =
                match centerSource.wavelength with
                | Some centerWavelength ->

                    sources
                    |> List.filter (fun source ->
                        match source.wavelength with
                        | Some wavelength ->
                            abs (wavelength - centerWavelength) <= maxWavelengthDistanceNm
                        | None ->
                            source.logicalIndex = centerLogicalIndex
                    )
                    |> List.sortBy (fun source ->
                        match source.wavelength with
                        | Some wavelength -> abs (wavelength - centerWavelength)
                        | None -> Double.PositiveInfinity
                    )
                    |> List.truncate (2 * averageRadius + 1)

                | None ->

                    sources
                    |> List.filter (fun source ->
                        abs (source.logicalIndex - centerLogicalIndex) <= averageRadius
                    )
                    |> List.sortBy (fun source ->
                        abs (source.logicalIndex - centerLogicalIndex)
                    )

            let bandsOrErrors =
                candidates
                |> List.map (fun source -> readLogicalBand sources source.logicalIndex)

            let errors =
                bandsOrErrors
                |> List.choose (function
                    | Result.Error error -> Some error
                    | Result.Ok _ -> None
                )

            if not errors.IsEmpty then
                Result.Error (String.concat "\n" errors)
            else
                bandsOrErrors
                |> List.choose (function
                    | Result.Ok band -> Some band
                    | Result.Error _ -> None
                )
                |> averageBandData

    // Constructs a four-channel byte image from the currently loaded logical bands.
    // This works for both dataset layouts:
    // - stacked TIFF: every row points to the same TIFF, but uses another channelIndex
    // - MBI manifest: every row points to its own single-band TIFF, usually channelIndex = 0
    let private createRgbCompositePixImageFromSources
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : int)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : int)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : int)
        (lowerPercentile : float)
        (upperPercentile : float)
        (gamma : float)
        : Result<PixImage<byte>, string> =

        try
            let usesNetCDF =
                sources
                |> List.exists (fun source -> isNcFile source.filePath)

            let averageRadius =
                if usesNetCDF then 0 else 1

            let maxWavelengthDistanceNm =
                if usesNetCDF then 0.0 else 35.0


            match
                readAverageLogicalBand sources averageRadius maxWavelengthDistanceNm redNumeratorIndex,
                readAverageLogicalBand sources averageRadius maxWavelengthDistanceNm redDenominatorIndex,
                readAverageLogicalBand sources averageRadius maxWavelengthDistanceNm greenNumeratorIndex,
                readAverageLogicalBand sources averageRadius maxWavelengthDistanceNm greenDenominatorIndex,
                readAverageLogicalBand sources averageRadius maxWavelengthDistanceNm blueNumeratorIndex,
                readAverageLogicalBand sources averageRadius maxWavelengthDistanceNm blueDenominatorIndex
            with
            | Result.Ok redNumerator,
              Result.Ok redDenominator,
              Result.Ok greenNumerator,
              Result.Ok greenDenominator,
              Result.Ok blueNumerator,
              Result.Ok blueDenominator ->

                let selectedBands =
                    [
                        redNumerator
                        redDenominator
                        greenNumerator
                        greenDenominator
                        blueNumerator
                        blueDenominator
                    ]

                match validateSameDimensions selectedBands with
                | Result.Error error ->
                    Result.Error error

                | Result.Ok (width, height, pixelCount) ->

                    // EMIT reflectance is floating-point data. Keep this threshold very small:
                    // a too-high threshold can make the whole RGB result transparent.
                    let minimumSignal = 1.0e-8

                    let hasSignal value =
                        Double.IsFinite value && value > minimumSignal

                    let validRatio numerator denominator =
                        hasSignal numerator && hasSignal denominator

                    let validForeground =
                        Array.init pixelCount (fun i ->
                            // Do not require all six selected bands to be valid. One bad channel
                            // should not make the whole pixel transparent; valueToByte already maps
                            // invalid channel values to 0.
                            validRatio redNumerator.values.[i] redDenominator.values.[i] ||
                            validRatio greenNumerator.values.[i] greenDenominator.values.[i] ||
                            validRatio blueNumerator.values.[i] blueDenominator.values.[i]
                        )

                    let makeRatio = safeRatio

                    let redBand =
                        makeRatio minimumSignal redNumerator.values redDenominator.values

                    let greenBand =
                        makeRatio minimumSignal greenNumerator.values greenDenominator.values

                    let blueBand =
                        makeRatio minimumSignal blueNumerator.values blueDenominator.values

                    let lowerPercentileFraction =
                        if Double.IsFinite lowerPercentile then
                            lowerPercentile / 100.0
                            |> max 0.0
                            |> min 1.0
                        else
                            0.05

                    let upperPercentileFraction =
                        if Double.IsFinite upperPercentile then
                            upperPercentile / 100.0
                            |> max 0.0
                            |> min 1.0
                        else
                            0.98

                    let lowerPercentileFraction, upperPercentileFraction =
                        if upperPercentileFraction <= lowerPercentileFraction then
                            lowerPercentileFraction, min 1.0 (lowerPercentileFraction + 0.01)
                        else
                            lowerPercentileFraction, upperPercentileFraction

                    let displayRangeForValidPixels values =
                        let validValues =
                            values
                            |> Array.mapi (fun index value ->
                                if validForeground.[index] && Double.IsFinite value then
                                    Some value
                                else
                                    None
                            )
                            |> Array.choose id

                        Array.sortInPlace validValues

                        if validValues.Length = 0 then
                            0.0, 1.0
                        else
                            // Percentiles avoid extreme outlier pixels controlling the whole contrast.
                            let minimum =
                                percentile lowerPercentileFraction validValues

                            let maximum =
                                percentile upperPercentileFraction validValues

                            if maximum <= minimum then
                                minimum, minimum + 1.0
                            else
                                minimum, maximum

                    let redMin, redMax =
                        displayRangeForValidPixels redBand

                    let greenMin, greenMax =
                        displayRangeForValidPixels greenBand

                    let blueMin, blueMax =
                        displayRangeForValidPixels blueBand

                    let rgbImage =
                        PixImage<byte>(
                            Col.Format.RGBA,
                            V2i(width, height)
                        )

                    let debugRgbBytes =
                        if usesNetCDF then
                            Some (Array.zeroCreate<byte> (pixelCount * 3))
                        else
                            None


                    rgbImage
                        .GetMatrix<C4b>()
                        .SetByCoord(fun (position : V2l) ->
                            let x =
                                int position.X

                            let y =
                                int position.Y

                            let index =
                                y * width + x

                            if validForeground.[index] then
                                let r =
                                    valueToByte gamma redMin redMax redBand.[index]

                                let g =
                                    valueToByte gamma greenMin greenMax greenBand.[index]

                                let b =
                                    valueToByte gamma blueMin blueMax blueBand.[index]

                                match debugRgbBytes with
                                | Some bytes ->
                                    let offset =
                                        index * 3

                                    bytes.[offset + 0] <- r
                                    bytes.[offset + 1] <- g
                                    bytes.[offset + 2] <- b

                                | None ->
                                    ()


                                C4b(r, g, b, 255uy)
                            else
                                C4b(0uy, 0uy, 0uy, 0uy)
                        )
                    |> ignore

                    Result.Ok rgbImage


            | Result.Error error, _, _, _, _, _ ->
                Result.Error error

            | _, Result.Error error, _, _, _, _ ->
                Result.Error error

            | _, _, Result.Error error, _, _, _ ->
                Result.Error error

            | _, _, _, Result.Error error, _, _ ->
                Result.Error error

            | _, _, _, _, Result.Error error, _ ->
                Result.Error error

            | _, _, _, _, _, Result.Error error ->
                Result.Error error

        with error ->
            Result.Error error.Message

    let private loadRgbCompositeTextureFromSources
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : int)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : int)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : int)
        (lowerPercentile : float)
        (upperPercentile : float)
        (gamma : float)
        : ITexture =

        match
            createRgbCompositePixImageFromSources
                sources
                redNumeratorIndex
                redDenominatorIndex
                greenNumeratorIndex
                greenDenominatorIndex
                blueNumeratorIndex
                blueDenominatorIndex
                lowerPercentile
                upperPercentile
                gamma
        with
        | Result.Ok image ->
            PixTexture2d(
                PixImageMipMap [|
                    image :> PixImage
                |],
                false
            ) :> ITexture

        | Result.Error error ->
            Log.warn
                "Could not create RGB composite: %s"
                error

            DefaultTextures.checkerboard.GetValue()

    // Makes the RGB texture adaptive. It is recalculated when the loaded image rows,
    // RGB band selections, or contrast/gamma controls change.
    let createRgbCompositeTexture
        (images : alist<AdaptiveImage>)
        (redNumeratorBand : aval<Option<int>>)
        (redDenominatorBand : aval<Option<int>>)
        (greenNumeratorBand : aval<Option<int>>)
        (greenDenominatorBand : aval<Option<int>>)
        (blueNumeratorBand : aval<Option<int>>)
        (blueDenominatorBand : aval<Option<int>>)
        (lowerPercentile : aval<float>)
        (upperPercentile : aval<float>)
        (gamma : aval<float>)
        : aval<ITexture> =

        let adaptiveImages =
            AList.toAVal images

        AVal.custom (fun token ->

            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            let redNumeratorValue =
                redNumeratorBand.GetValue token

            let redDenominatorValue =
                redDenominatorBand.GetValue token

            let greenNumeratorValue =
                greenNumeratorBand.GetValue token

            let greenDenominatorValue =
                greenDenominatorBand.GetValue token

            let blueNumeratorValue =
                blueNumeratorBand.GetValue token

            let blueDenominatorValue =
                blueDenominatorBand.GetValue token

            let lowerPercentileValue =
                lowerPercentile.GetValue token

            let upperPercentileValue =
                upperPercentile.GetValue token

            let gammaValue =
                gamma.GetValue token

            match
                sources,
                redNumeratorValue,
                redDenominatorValue,
                greenNumeratorValue,
                greenDenominatorValue,
                blueNumeratorValue,
                blueDenominatorValue
            with
            | [], _, _, _, _, _, _ ->
                DefaultTextures.checkerboard.GetValue()

            | _,
              Some redNumerator,
              Some redDenominator,
              Some greenNumerator,
              Some greenDenominator,
              Some blueNumerator,
              Some blueDenominator ->

                loadRgbCompositeTextureFromSources
                    sources
                    redNumerator
                    redDenominator
                    greenNumerator
                    greenDenominator
                    blueNumerator
                    blueDenominator
                    lowerPercentileValue
                    upperPercentileValue
                    gammaValue

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    let private tryReadWavelengthsFromJson (jsonPath : string) =
        try
            use document =
                JsonDocument.Parse(File.ReadAllText jsonPath)

            let mutable wavelengthsElement =
                Unchecked.defaultof<JsonElement>

            if
                document.RootElement.TryGetProperty(
                    "wavelengths",
                    &wavelengthsElement
                )
                && wavelengthsElement.ValueKind = JsonValueKind.Array
            then
                wavelengthsElement.EnumerateArray()
                |> Seq.map (fun value -> value.GetDouble())
                |> Seq.toList
                |> Some
            else
                None
        with error ->
            Log.warn "Could not read wavelengths from %s: %s"
                jsonPath
                error.Message

            None

    // the 2D view displays the texture directly
    let createInstrumentScene
        (rgbTexture : aval<ITexture>) =

        Sg.fullScreenQuad
        |> Sg.noEvents
        |> Sg.texture
            "RgbCompositeTexture"
            rgbTexture
        |> Sg.shader {
            do! Shaders.displayRgbComposite
        }

    let loadMbiBands (mbiPath : string) : list<Image> =
        match tryReadMbiBands mbiPath with
        | None ->
            []

        | Some mbiBands ->

            [
                for band in mbiBands do
                    if File.Exists band.filePath then

                        let _, tiffJson =
                            InstrumentMetadata.tryParseMetadataForImagePath band.filePath

                        let dataType =
                            match tiffJson with
                            | Some metadata ->
                                match metadata.data_type.ToLowerInvariant() with
                                | "uint16" -> DataType.UInt16
                                | "uint32" -> DataType.UInt32
                                | "float"  -> DataType.Float
                                | _        -> DataType.UInt16
                            | None ->
                                DataType.UInt16

                        let minimum, maximum =
                            match tiffJson with
                            | Some metadata when metadata.image_statistics.Length > 0 ->
                                metadata.image_statistics.[0].minimum,
                                metadata.image_statistics.[0].maximum
                            | _ ->
                                0.0, 1.0

                        let sliderMinimum, sliderMaximum =
                            match dataType with
                            | DataType.Float ->
                                minimum, maximum
                            | DataType.UInt16 ->
                                0.0, 65535.0
                            | DataType.UInt32 ->
                                0.0, float UInt32.MaxValue

                        let bandName =
                            match band.label, band.wavelength with
                            | Some label, Some wavelength ->
                                Some (sprintf "%s / %.0f nm" label wavelength)
                            | None, Some wavelength ->
                                Some (sprintf "%.0f nm" wavelength)
                            | Some label, None ->
                                Some label
                            | None, None ->
                                Some (sprintf "Band %d" band.index)

                        let channel =
                            {
                                // The TIFF itself is single-band.
                                idx = 0
                                name = bandName
                            }

                        yield
                            {
                                initial with
                                    texture = band.filePath

                                    // This is the logical multiband index from the MBI file.
                                    bandIndex = band.index
                                    wavelength = band.wavelength

                                    selectedChannel = channel
                                    channelOptions = [ channel ]

                                    defaultMinValues = [ minimum ]
                                    defaultMaxValues = [ maximum ]

                                    inputMinValue =
                                        {
                                            minValue with
                                                value = minimum
                                                min = sliderMinimum
                                                max = sliderMaximum
                                        }

                                    inputMaxValue =
                                        {
                                            maxValue with
                                                value = maximum
                                                min = sliderMinimum
                                                max = sliderMaximum
                                        }

                                    dataType = dataType
                            }

                    else
                        Log.warn "MBI band file does not exist: %s" band.filePath
            ]

    let loadBands (texturePath : string) : list<Image> =

        let fullPath = Path.GetFullPath texturePath

        let tiffMbiJson, tiffJson =
            InstrumentMetadata.tryParseMetadataForImagePath fullPath

        let channelCount =
            match tiffJson with
            | Some metadata -> max 1 metadata.channels
            | None -> 1

        let wavelengths =
            let jsonPath = Path.ChangeExtension(texturePath, ".json")

            if File.Exists jsonPath then
                tryReadWavelengthsFromJson jsonPath
                |> Option.defaultValue []
            else
                []

        let dataType =
            match tiffJson with
            | Some metadata ->
                match metadata.data_type.ToLowerInvariant() with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float"  -> DataType.Float
                | _        -> DataType.UInt16
            | None ->
                DataType.UInt16

        let rawMinValues =
            match tiffJson with
            | Some metadata ->
                metadata.image_statistics
                |> Array.map (fun statistics -> statistics.minimum)
                |> Array.toList
            | None ->
                []

        let rawMaxValues =
            match tiffJson with
            | Some metadata ->
                metadata.image_statistics
                |> Array.map (fun statistics -> statistics.maximum)
                |> Array.toList
            | None ->
                []

        // Ensure that these lists always contain one value per channel.
        // ResetCustomMinMax indexes them using selectedChannel.idx.
        let defaultMinValues =
            [
                for channelIndex in 0 .. channelCount - 1 do
                    yield
                        rawMinValues
                        |> List.tryItem channelIndex
                        |> Option.defaultValue 0.0
            ]

        let defaultMaxValues =
            [
                for channelIndex in 0 .. channelCount - 1 do
                    yield
                        rawMaxValues
                        |> List.tryItem channelIndex
                        |> Option.defaultValue 1.0
            ]

        let distance =
            match tiffMbiJson with
            | Some metadata -> metadata.targetPos.Length
            | None -> 0.0

        let time =
            match tiffMbiJson with
            | Some metadata -> metadata.obs_date
            | None -> DateTime.MinValue

        [
            for channelIndex in 0 .. channelCount - 1 do

                let minimum = defaultMinValues[channelIndex]
                let maximum = defaultMaxValues[channelIndex]

                let wavelengthName =
                    wavelengths
                    |> List.tryItem channelIndex
                    |> Option.map (fun wavelength ->
                        sprintf "%.0f nm" wavelength
                    )

                let wavelength =
                    wavelengths
                    |> List.tryItem channelIndex

                let channel =
                    {
                        idx = channelIndex
                        name = wavelengthName
                    }

                let sliderMinimum, sliderMaximum =
                    match dataType with
                    | DataType.Float ->
                        minimum, maximum

                    | DataType.UInt16 ->
                        0.0, 65535.0

                    | DataType.UInt32 ->
                        0.0, float UInt32.MaxValue

                let inputMinimum =
                    {
                        minValue with
                            value = minimum
                            min = sliderMinimum
                            max = sliderMaximum
                    }

                let inputMaximum =
                    {
                        maxValue with
                            value = maximum
                            min = sliderMinimum
                            max = sliderMaximum
                    }

                yield
                    {
                        initial with
                            texture = fullPath
                            selectedChannel = channel
                            bandIndex = channelIndex
                            wavelength = wavelength

                            // This band entry represents exactly one channel.
                            channelOptions = [channel]

                            defaultMinValues = defaultMinValues
                            defaultMaxValues = defaultMaxValues

                            inputMinValue = inputMinimum
                            inputMaxValue = inputMaximum

                            dataType = dataType
                            distance = distance
                            time = time
                    }
        ]


    let loadNcBands (ncPath : string) : list<Image> =
        let fullPath = Path.GetFullPath ncPath

        match tryReadNcDatasetInfoUncached fullPath with
        | Result.Error error ->
            Log.warn "Could not read NetCDF file %s: %s" fullPath error
            []

        | Result.Ok info ->
            let wavelengths =
                match info.productKind with
                | Reflectance
                | ReflectanceUncertainty ->
                    tryReadNcVectorFloat fullPath "sensor_band_parameters/wavelengths"
                    |> Option.defaultValue []

                | Mask ->
                    []

            let goodWavelengths =
                match info.productKind with
                | Reflectance
                | ReflectanceUncertainty ->
                    tryReadNcVectorInt fullPath "sensor_band_parameters/good_wavelengths"
                    |> Option.defaultValue []

                | Mask ->
                    []

            // Important:
            // Do not read every NetCDF band here just to calculate statistics.
            // An EMIT RFL cube has ~285 bands and each band is large
            // The actual selected bands are still read later by readBandSourceAsFloat
            // when the RGB composite/texture is created.
            let defaultRangeForProduct =
                match info.productKind with
                | Reflectance ->
                    0.0, 1.0, 0.0, 1.0

                | ReflectanceUncertainty ->
                    0.0, 0.1, 0.0, 1.0

                | Mask ->
                    0.0, 1.0, 0.0, 1.0

            [
                // loads bands and creates list of images
                //for bandIndex in 0 .. info.bands - 1 do
                  for bandIndex in 0 .. info.bands - 1 do // only load half of bands for speed
                    let minimum, maximum, sliderMinimum, sliderMaximum =
                        defaultRangeForProduct

                    let bandName, wavelength =
                        match info.productKind with
                        | Mask ->
                            emitMaskBandNames
                            |> List.tryItem bandIndex
                            |> Option.defaultValue (sprintf "Mask band %d" bandIndex)
                            |> Some,
                            None

                        | Reflectance ->
                            match wavelengths |> List.tryItem bandIndex with
                            | Some wavelength ->
                                let goodSuffix =
                                    match goodWavelengths |> List.tryItem bandIndex with
                                    | Some 0 -> " / bad wavelength"
                                    | _ -> ""

                                Some (sprintf "RFL %.1f nm%s" wavelength goodSuffix),
                                Some wavelength

                            | None ->
                                Some (sprintf "RFL band %d" bandIndex),
                                None

                        | ReflectanceUncertainty ->
                            match wavelengths |> List.tryItem bandIndex with
                            | Some wavelength ->
                                Some (sprintf "RFL uncertainty %.1f nm" wavelength),
                                Some wavelength

                            | None ->
                                Some (sprintf "RFL uncertainty band %d" bandIndex),
                                None

                    let channel =
                        {
                            idx = bandIndex
                            name = bandName
                        }

                    yield
                        {
                            initial with
                                texture = fullPath
                                selectedChannel = channel
                                channelOptions = [ channel ]
                                dataType = DataType.Float

                                bandIndex = bandIndex
                                wavelength = wavelength

                                defaultMinValues = [ minimum ]
                                defaultMaxValues = [ maximum ]

                                inputMinValue =
                                    {
                                        minValue with
                                            value = minimum
                                            min = sliderMinimum
                                            max = sliderMaximum
                                    }

                                inputMaxValue =
                                    {
                                        maxValue with
                                            value = maximum
                                            min = sliderMinimum
                                            max = sliderMaximum
                                    }
                        }
            ]

    let loadDataset (path : string) : list<Image> =
        match tryResolveNcPathToLoad path with
        | Some ncPath ->
            loadNcBands ncPath

        | None ->
            match tryReadMbiBands path with
            | Some _ ->
                loadMbiBands path

            | None ->
                loadBands path

    let loadFile (texturePath : string) =
        // this could be a fallback
        let ifUsefulThisIsHowToExtractInfos = MultiBandReader.tryGetChannels texturePath

        let (tiffMbiJson, tiffJson) = InstrumentMetadata.tryParseMetadataForImagePath texturePath

        let channels =
            match tiffJson with
            | Some tf -> tf.channels
            | None -> 1

        let channelOptions = [ 0 .. channels - 1 ] |> List.map (fun channel -> {idx = channel; name = None})

        let selectedChannelIdx = 0

        let defaultMinValues = 
            match tiffJson with
            | Some tf -> tf.image_statistics |> Array.toList |> List.map (fun x -> x.minimum)
            | None -> [0.0]

        let defaultMaxValues = 
            match tiffJson with
            | Some tf -> tf.image_statistics |> Array.toList|> List.map (fun x -> x.maximum)
            | None -> [0.0]

        let dataType = 
            match tiffJson with
            | Some tf -> 
                match tf.data_type with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float" -> DataType.Float
                | _ -> DataType.UInt16
            | None -> DataType.UInt16

        let (rangeMin, rangeMax) =
            match dataType with
            | DataType.Float -> (defaultMinValues[selectedChannelIdx], defaultMaxValues[selectedChannelIdx])
            | DataType.UInt16 
            | _ -> (0, 65536)

        let inputMinValue = { minValue with value = defaultMinValues[selectedChannelIdx]; min = rangeMin; max = rangeMax}

        let inputMaxValue = { minValue with value = defaultMaxValues[selectedChannelIdx]; min = rangeMin; max = rangeMax }

        let distance =
            match tiffMbiJson with
            | Some mbi -> mbi.targetPos.Length
            | None -> 0.0

        let time =
            match tiffMbiJson with
            | Some mbi -> mbi.obs_date
            | None -> System.DateTime.MinValue // which default time?

        { initial with
            texture = Path.GetFullPath(texturePath);
            defaultMinValues = defaultMinValues;
            defaultMaxValues = defaultMaxValues;
            inputMinValue = inputMinValue;
            inputMaxValue = inputMaxValue;
            selectedChannel = channelOptions[selectedChannelIdx];
            channelOptions = channelOptions;
            dataType = dataType;
            distance = distance;
            time = time;
        }

    let update (m : Image) (msg : ImageMessage) =
        match msg with
            | SetDataTypeAndRange (dataType, min, max) ->
                { m with inputMinValue = { minValue with min = min}; inputMaxValue = {minValue with max = max} }
            | SetCustomMin v -> 
                { m with inputMinValue = {minValue with value = v} }
            | SetCustomMax v -> 
                { m with inputMaxValue = {maxValue with value = v} }
            | ResetCustomMinMax ->
                { m with inputMinValue = {minValue with value = m.defaultMinValues[m.selectedChannel.idx]}; inputMaxValue = {maxValue with value = m.defaultMaxValues[m.selectedChannel.idx]} }
            | SetColorMap (map : ColorMap) ->
                { m with colorMap = map }
            | SetEXRChannel channel ->
                let (min, max) = (m.defaultMinValues[channel.idx], m.defaultMaxValues[channel.idx])
                { m with 
                    inputMinValue = {minValue with value = min};
                    inputMaxValue = {maxValue with value = max};
                    selectedChannel = channel
                }
            | ToggleFalseColor ->
                { m with useFalseColor = not m.useFalseColor }
            | ImageMessage.Empty ->
                m


    let whitePix =
        let pi = PixImage<byte>(Col.Format.RGBA, V2i.II)
        pi.GetMatrix<C4b>().SetByCoord(fun (c : V2l) -> C4b.White) |> ignore
        pi

    let whiteTex =
        PixTexture2d(PixImageMipMap [| whitePix :> PixImage |], false) :> ITexture

    let view (m : AdaptiveImage) =
        let content = 
            Html.table [ 
                Html.row "EXR Channel:" [
                    div [style "color: white;"] [
                        let channelRepr (c : Channel) = 
                            match c.name with
                            | None -> string c.idx
                            | Some name -> name
                        Html.SemUi.dropDown' (AList.ofAVal m.channelOptions) m.selectedChannel (fun value -> SetEXRChannel value) channelRepr
                        // Html.SemUi.dropDown m.channel SetEXRChannel
                    ]
                ]
                Html.row "False Color:" [
                    text "Activate: " 
                    Html.SemUi.toggleBox m.useFalseColor ToggleFalseColor
                    br []
                    Html.SemUi.dropDown m.colorMap SetColorMap
                ]
                Html.row "Minimum:" [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.inputMinValue.value) SetCustomMin
                    br []
                    Numeric.view' [Slider] m.inputMinValue
                    |> UI.map (fun action -> 
                        match action with
                        | Numeric.Action.SetValue v ->
                            SetCustomMin v
                        | _ ->
                            ImageMessage.Empty
                        )
                    ]
                Html.row "Maximum:"  [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.inputMaxValue.value) SetCustomMax
                    br []
                    div [style "width: 100%"] [
                        Numeric.numericField' m.inputMaxValue Slider
                        |> UI.map (fun action -> 
                            match action with
                            | Numeric.Action.SetValue v ->
                                SetCustomMax v
                            | _ ->
                                ImageMessage.Empty
                            )
                        ]
                    ] 
                Html.row "" [button [clazz "ui inverted button"; onClick (fun _ -> ResetCustomMinMax)] [
                        text "Reset"
                    ]
                ]
            ]

        require Html.semui (
            div [] [
                div [style "position: relative; paddingLeft: 25px; paddingTop: 25px; width: 100%"] [
                    content
                ]
            ]
        )

    let view2DAnd3DImageAbsolute
        (opacity : aval<float>)
        (boresightAdjustment : aval<Option<Trafo3d>>)
        (orbitState : AdaptiveOrbitState)
        (sourceImagePath : aval<Option<string>>)
        (rgbTexture : aval<ITexture>) =

        let instrumentVisualization =
            createInstrumentScene rgbTexture

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum2D = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))
        let farPlaneMars = 30101626.50 * 1000.0
        let frustum = Frustum.perspective 80.0 10.0 farPlaneMars 1.0 |> AVal.constant

        let observer = cval "MARS" //"HERA_AFC-1" 
        let supportBody = cval "SUN"
        let referenceFrame = cval "ECLIPJ2000"
        let referenceFrame = cval "IAU_MARS"

        let currentProjectedImageFromImage (m : AdaptiveImage) =
            m.texture
            |> AVal.map (fun path ->
                if File.Exists path then
                    Some (
                        path,
                        InstrumentMetadata.tryParseMetadataForImagePath path
                    )
                else
                    None
            )

        let currentProjectedImage =
            sourceImagePath
            |> AVal.map (function
                | Some path when File.Exists path ->
                    Some (
                        path,
                        InstrumentMetadata.tryParseMetadataForImagePath path
                    )

                | _ ->
                    None
            )            

        let imageSettings =
            {
                VisualizationProperties.empty with
                    projectionOpacity = opacity
            }

        let projectionSetup = 
            // instrument projection
            let p : InstrumentProjection = {
                target = InstrumentImages.CameraFocus.FocusBody "MARS"
                cameraSource = InstrumentImages.CameraSource.InBody "HERA"
                instrumentReferenceFrame = "HERA_AFC-1"
                instrumentName = "HERA_AFC-1"
                supportBody = "SUN"
                time = DateTime.Now
                boresightAdjustment = None
            }

            (currentProjectedImage, boresightAdjustment)
            ||> AVal.map2 (fun currentProjectedImage boresight -> 
                match currentProjectedImage with
                | Some (_, (Some mbi, _)) -> 
                    // update using selected image metadata
                    let instrumentName =
                        match InstrumentProjection.instrument2SpiceName mbi.instrument with
                        | Some name ->
                            name
                        | None ->
                            failwith "no spice name for the given instrument."

                    let p = 
                        {
                            p with
                                time = mbi.obs_date
                                instrumentName = instrumentName
                                instrumentReferenceFrame = "J2000"
                                boresightAdjustment = boresight
                        }

                    p, mbi.obs_date

                | _ ->
                    Log.warn
                        "Could not access observation time from selected image metadata. Projection time was not updated. Current fallback value is: %A"
                        p.time

                    p, p.time
            )

        let projection =
            projectionSetup |> AVal.map fst

        let time =
            projectionSetup |> AVal.map snd
            
        let projectPrimaryImage =
            Visualization.creatProjectionFunction
                observer
                time
                referenceFrame
                currentProjectedImage
                projection

        let primaryProjectionEnabled =
            currentProjectedImage
            |> AVal.map (function
                | Some (_, (Some _, _)) -> true
                | _ -> false
            )

        let scene =
            Visualization.createRgbSceneGraph
                imageSettings
                referenceFrame
                supportBody
                observer
                time
                projectPrimaryImage
                rgbTexture
                primaryProjectionEnabled
            |> Sg.noEvents
            

        require Html.semui (
                div [] [
                    div [] [
                        // the 2D control
                        let leftControl = [style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"; attribute "showLoader" "false"]
                        renderControl (AVal.constant (Camera.create cameraView frustum2D)) leftControl instrumentVisualization
                    
                    ]
                ]
        )

    let view2DRelative (rgbTexture : aval<ITexture>) =
        let instrumentVisualization = createInstrumentScene rgbTexture

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        require Html.semui (
            div [style "width: 100%; height: 200px; display: flex; align-items: center; justify-content: center; margin-top: 10px; border: solid 2px black; background: rgb(0, 0, 0, 0.5);"] [
                let style = [style "position: relative; width: 200px; height: 200px; padding: 2px"; attribute "showLoader" "false"]
                renderControl (AVal.constant (Camera.create cameraView frustum')) style instrumentVisualization
            ]
        )
