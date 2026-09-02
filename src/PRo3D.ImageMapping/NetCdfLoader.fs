namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI.Primitives
open PRo3D.ImageMapping.Model

open System.IO
open System.Runtime.InteropServices

open HDF.PInvoke

open PRo3D.InstrumentProjection
open System.Collections.Concurrent
open PRo3D.ImageMapping.ImageDefaults

module NetCdfLoader =
    let ncProductKindFromPath (path : string) =
        let fileName = Path.GetFileName(path).ToUpperInvariant()

        if fileName.Contains("_L2A_RFLUNCERT_") || fileName.Contains("_RFLUNCERT_") then
            ReflectanceUncertainty
        elif fileName.Contains("_L2A_MASK_") || fileName.Contains("_MASK_") then
            Mask
        else
            Reflectance

    // caches to keep already-read metadata and bands in memory so changing one RGB ratio band
    // does not reread the other five bands from disk
    let ncDatasetInfoCache =
        ConcurrentDictionary<string, NcDatasetInfo>()

    let ncRawBandCache =
        ConcurrentDictionary<string, int * int * int * float[]>()

    let normalizedPath (path : string) =
        Path.GetFullPath(path).ToLowerInvariant()

    let ncBandCacheKey (path : string) (datasetPath : string) (bandIndex : int) = 
        sprintf "%s|%s|%d" (normalizedPath path) datasetPath bandIndex

    let emitMaskBandNames =
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
    
    let closeHdf5 (close : int64 -> int) (id : int64) =
        if id >= 0L then close id |> ignore

    let isNcFile (path : string) =
        String.Equals(Path.GetExtension(path), ".nc", StringComparison.OrdinalIgnoreCase)

    let isReflectanceNcFileName (path : string) =
        let name = Path.GetFileName(path).ToUpperInvariant()
        (name.Contains("_L2A_RFL_") || name.Contains("_RFL_"))
        && not (name.Contains("_RFLUNCERT_"))
        && not (name.Contains("_MASK_"))

    let tryResolveNcPathToLoad (path : string) : Option<string> =
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
    
    let ncDatasetPathForKind kind =
        match kind with
        | Reflectance -> "reflectance"
        | ReflectanceUncertainty -> "reflectance_uncertainty"
        | Mask -> "mask"

    let tryOpenHdf5Dataset (path : string) (datasetPath : string) : Result<int64 * int64, string> =
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

    let tryReadNcDatasetInfoUncached (path : string) : Result<NcDatasetInfo, string> =
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

    let tryReadNcDatasetInfoCached (path : string) : Result<NcDatasetInfo, string> =
        let fullPath = Path.GetFullPath path
        let cacheKey = normalizedPath fullPath

        match ncDatasetInfoCache.TryGetValue cacheKey with
        | true, cached ->
            Result.Ok cached
        | false, _ ->
            match tryReadNcDatasetInfoUncached cacheKey with
            | Result.Error error ->
                Result.Error error
            | Result.Ok info ->
                ncDatasetInfoCache.[cacheKey] <- info
                Result.Ok info

    let readNcBandAsFloat
        (path : string)
        (datasetPath : string)
        (bandIndex : int)
        : Result<int * int * int * float[], string> =

        match tryReadNcDatasetInfoCached path with
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

    let tryReadNcVectorFloat (path : string) (datasetPath : string) : Option<float list> =
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

    let tryReadNcVectorInt (path : string) (datasetPath : string) : Option<int list> =
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

    let applyEmitAuxiliaryMasks
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

    let loadNcBands (ncPath : string) : list<Image> =
        let fullPath = Path.GetFullPath ncPath

        match tryReadNcDatasetInfoCached fullPath with
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
                for bandIndex in 0 .. info.bands - 1 do 
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

