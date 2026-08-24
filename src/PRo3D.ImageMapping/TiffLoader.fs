namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI.Primitives
open PRo3D.ImageMapping.Model

open System.IO

open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentProjection
open PRo3D.Core

open PRo3D.ImageMapping.MbiLoader
open PRo3D.ImageMapping.ImageDefaults
open BitMiracle.LibTiff.Classic

module TiffLoader =

    let private tryReadContiguousUInt16Tiff
        (path : string)
        : Result<TiffReadResult, string> =

        try
            use tif = Tiff.Open(path, "r")

            if isNull tif then
                Result.Error "Cannot open TIFF."
            else
                let width =
                    tif.GetFieldDefaulted(TiffTag.IMAGEWIDTH).[0].ToInt()

                let height =
                    tif.GetFieldDefaulted(TiffTag.IMAGELENGTH).[0].ToInt()

                let bitsPerSample =
                    tif.GetFieldDefaulted(TiffTag.BITSPERSAMPLE).[0].ToInt()

                let sampleFormat =
                    tif.GetFieldDefaulted(TiffTag.SAMPLEFORMAT).[0].ToInt()

                let samplesPerPixel =
                    tif.GetFieldDefaulted(TiffTag.SAMPLESPERPIXEL).[0].ToInt()

                let planarConfig =
                    tif.GetFieldDefaulted(TiffTag.PLANARCONFIG).[0].ToInt()

                if planarConfig <> int PlanarConfig.CONTIG then
                    Result.Error "TIFF is not PLANARCONFIG=CONTIG."

                elif
                    bitsPerSample <> 16 ||
                    sampleFormat <> int SampleFormat.UINT
                then
                    Result.Error (
                        sprintf
                            "Unsupported SAMPLEFORMAT=%d BITSPERSAMPLE=%d"
                            sampleFormat
                            bitsPerSample
                    )

                else
                    let scanlineSize = tif.ScanlineSize()
                    let scanline = Array.zeroCreate<byte> scanlineSize
                    let pixelCount = width * height

                    let bands =
                        Array.init samplesPerPixel (fun _ ->
                            Array.zeroCreate<uint16> pixelCount
                        )

                    for row in 0 .. height - 1 do
                        if not (tif.ReadScanline(scanline, row, 0s)) then
                            failwithf "ReadScanline failed at row %d." row

                        for column in 0 .. width - 1 do
                            let pixelIndex = row * width + column

                            for band in 0 .. samplesPerPixel - 1 do
                                let byteOffset =
                                    (column * samplesPerPixel + band) * 2

                                bands.[band].[pixelIndex] <-
                                    BitConverter.ToUInt16(scanline, byteOffset)

                    Result.Ok {
                        width = width
                        height = height
                        bands = samplesPerPixel
                        format = Format.Uint16
                        buffers = PixelBuffers.UInt16Bands bands
                    }

        with error ->
            Result.Error error.Message


    let tryReadMultiBandTiff
        (path : string)
        (forceByteSwap : bool)
        : Result<TiffReadResult, string> =

        try
            match MultiBandReader.tryReadMultiBandTiff path forceByteSwap with
            | Result.Ok image ->
                Result.Ok image

            | Result.Error _ ->
                tryReadContiguousUInt16Tiff path

        with _ ->
            tryReadContiguousUInt16Tiff path

    let  getBandAsFloat
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

    let loadTiffBands (texturePath : string) : list<Image> =

        let fullPath = Path.GetFullPath texturePath

        let tiffMbiJson, tiffJson =
            InstrumentMetadata.tryParseMetadataForImagePath fullPath

        let channelCount =
            match MultiBandReader.tryGetChannels fullPath with
            | Some info ->
                max 1 info.channels

            | None ->
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

        let dataType = dataTypeFromMetadata fullPath

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
              
                createBandImage
                    texturePath
                    channelIndex
                    channel
                    wavelength
                    dataType
                    minimum
                    maximum
                    minimum
                    maximum
                    distance
                    time
        ]
   