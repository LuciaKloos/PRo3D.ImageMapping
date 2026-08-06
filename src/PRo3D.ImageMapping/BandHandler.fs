namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO

open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentProjection

open ImageMath
open NetCdfLoader

open PRo3D.ImageMapping.TiffLoader

module BandHandler =
    
    let availableLogicalBandsMessage (sources : list<RgbBandSource>) =
        sources
        |> List.map (fun source -> source.logicalIndex)
        |> List.distinct
        |> List.sort
        |> List.map string
        |> String.concat ", "

    let readAdaptiveBandSources
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

    let readBandSourceAsFloat
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
                            | Reflectance
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


    let readLogicalBand
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

    let validateSameDimensions
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


    let readOptionalLogicalBand
        (sources : list<RgbBandSource>)
        (bandIndex : Option<int>)
        : Result<Option<RgbBandData>, string> =

        match bandIndex with
        | Some index ->
            readLogicalBand sources index
            |> Result.map Some

        | None ->
            Result.Ok None


    let loadSelectedCompositeRatioBands
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : Option<int>)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : Option<int>)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : Option<int>)
        : Result<RgbBandData * Option<RgbBandData> * RgbBandData * Option<RgbBandData> * RgbBandData * Option<RgbBandData>, string> =

        match
            readLogicalBand sources redNumeratorIndex,
            readOptionalLogicalBand sources redDenominatorIndex,
            readLogicalBand sources greenNumeratorIndex,
            readOptionalLogicalBand sources greenDenominatorIndex,
            readLogicalBand sources blueNumeratorIndex,
            readOptionalLogicalBand sources blueDenominatorIndex
        with
        | Result.Ok redNumerator,
          Result.Ok redDenominator,
          Result.Ok greenNumerator,
          Result.Ok greenDenominator,
          Result.Ok blueNumerator,
          Result.Ok blueDenominator ->

            Result.Ok (
                redNumerator,
                redDenominator,
                greenNumerator,
                greenDenominator,
                blueNumerator,
                blueDenominator
            )

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
    
    let loadSelectedRgbMappingBands
        (sources : list<RgbBandSource>)
        (redBandIndex : int)
        (greenBandIndex : int)
        (blueBandIndex : int)
        : Result<RgbBandData * RgbBandData * RgbBandData, string> =

        match
            readLogicalBand sources redBandIndex,
            readLogicalBand sources greenBandIndex,
            readLogicalBand sources blueBandIndex
        with
        | Result.Ok redBand,
          Result.Ok greenBand,
          Result.Ok blueBand ->

            Result.Ok (
                redBand,
                greenBand,
                blueBand
            )

        | Result.Error error, _, _ ->
            Result.Error error

        | _, Result.Error error, _ ->
            Result.Error error

        | _, _, Result.Error error ->
            Result.Error error


    let loadSelectedTransferFunctionBand
        (sources : list<RgbBandSource>)
        (bandIndex : int)
        : Result<RgbBandData, string> =

        readLogicalBand sources bandIndex

    let readSourceImageRGBChannels 
        (imagePath : string)
        : Result<float [] * float[] * float[], string> =

        try 
            if String.IsNullOrWhiteSpace imagePath then
                Result.Error "The selected image has no source path."
            elif not (File.Exists imagePath) then
                Result.Error (sprintf "Image source does not exist: %s" imagePath)
            elif isNcFile imagePath then
                Result.Error "RGB image channels cannot be read directly from a NetCDF dataset."
            else
                Log.warn
                    "Reading RGB histogram image: path=%s, exists=%b"
                    imagePath
                    (File.Exists imagePath)
                let image =
                    PixImage<byte>(imagePath)
                        .ToPixImage<byte>(Col.Format.RGBA)
                let pixels = image.GetMatrix<C4b>()
                let width = image.Size.X
                let height = image.Size.Y
                let pixelCount = width * height

                let red = Array.zeroCreate<float> pixelCount
                let green = Array.zeroCreate<float> pixelCount
                let blue = Array.zeroCreate<float> pixelCount

                for y in 0 .. height - 1 do
                    for x in 0 .. width - 1 do
                        let i = y * width + x
                        let pixel = pixels.[x, y]

                        red.[i] <- float pixel.R / 255.0
                        green.[i] <- float pixel.G / 255.0
                        blue.[i] <- float pixel.B / 255.0

                Log.warn
                    "Extracted RGB histogram channels: R=%d, G=%d, B=%d"
                    red.Length
                    green.Length
                    blue.Length
                Result.Ok (red, green, blue)

        with error ->
            Result.Error error.Message