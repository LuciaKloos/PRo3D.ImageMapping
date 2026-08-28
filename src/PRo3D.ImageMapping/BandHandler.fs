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

open System.Collections.Concurrent
open System.Threading

open PRo3D.ImageMapping.TiffLoader

module BandHandler =
    
    type BandStatistics =
        {
            finiteCount        : int64
            positiveFiniteCount: int64
            minimum            : float option
            maximum            : float option
            positiveFiniteMean : float option
        }

    type private CachedBandPayload =
        {
            width  : int
            height : int
            values : float[]
            statistics : Lazy<BandStatistics>
        }

    let private computeBandStatistics
        (values: float[])
        : BandStatistics =

        Log.warn "STATISTICS CACHE MISS: scanning %d pixels" values.Length

        let mutable finiteCount = 0L
        let mutable positiveFiniteCount = 0L

        let mutable minimum =
            Double.PositiveInfinity

        let mutable maximum =
            Double.NegativeInfinity

        let mutable positiveFiniteSum = 0.0

        for value in values do
            if Double.IsFinite value then
                finiteCount <- finiteCount + 1L
                minimum <- min minimum value
                maximum <- max maximum value

                if value > 0.0 then
                    positiveFiniteCount <-
                        positiveFiniteCount + 1L

                    positiveFiniteSum <-
                        positiveFiniteSum + value

        {
            finiteCount = finiteCount
            positiveFiniteCount = positiveFiniteCount

            minimum =
                if finiteCount = 0L then
                    None
                else
                    Some minimum

            maximum =
                if finiteCount = 0L then
                    None
                else
                    Some maximum

            positiveFiniteMean =
                if positiveFiniteCount = 0L then
                    None
                else
                    Some (
                        positiveFiniteSum /
                        float positiveFiniteCount
                    )
        }

    let private createCachedBandPayload
        (width: int)
        (height: int)
        (values: float[])
        : CachedBandPayload =

        {
            width = width
            height = height
            values = values

            statistics =
                Lazy<BandStatistics>(
                    (fun () ->
                        computeBandStatistics values
                    ),
                    LazyThreadSafetyMode.ExecutionAndPublication
                )
        }

    [<Struct>]
    type private BandPayloadCacheKey =
        {
            filePath       : string
            channelIndex   : int
            fileLength     : int64
            lastWriteTicks : int64
        }

    [<Struct>]
    type private TiffPayloadCacheKey =
        {
            filePath       : string
            fileLength     : int64
            lastWriteTicks : int64
        }
    
    let private decodedPayloadCache =
        ConcurrentDictionary<
            BandPayloadCacheKey,
            Lazy<Result<CachedBandPayload, string>>>()

    let private decodedTiffPayloadCache =
        ConcurrentDictionary<
            TiffPayloadCacheKey,
            Lazy<Result<CachedBandPayload[], string>>>()

    let clearDecodedBandCache () =
        decodedPayloadCache.Clear()
        decodedTiffPayloadCache.Clear()

    let private createPayloadCacheKey (source: RgbBandSource) =
        let fullPath = Path.GetFullPath source.filePath
        let fileInfo = FileInfo fullPath

        {
            filePath = fullPath
            channelIndex = source.channelIndex
            fileLength = fileInfo.Length
            lastWriteTicks = fileInfo.LastWriteTimeUtc.Ticks
        }

    let private createTiffPayloadCacheKey (filePath: string) =
        let fullPath = Path.GetFullPath filePath
        let fileInfo = FileInfo fullPath

        {
            filePath = fullPath
            fileLength = fileInfo.Length
            lastWriteTicks = fileInfo.LastWriteTimeUtc.Ticks
        }

    let private decodeNcBandPayload
        (source: RgbBandSource)
        : Result<CachedBandPayload, string> =

        Log.warn
            "NETCDF CACHE MISS: decoding channel %d from %s"
            source.channelIndex
            source.filePath

        try
            match tryReadNcDatasetInfoUncached source.filePath with
            | Result.Error error ->
                Result.Error error

            | Result.Ok info ->
                match
                    readNcBandAsFloat
                        source.filePath
                        info.datasetPath
                        source.channelIndex
                with
                | Result.Error error ->
                    Result.Error error

                | Result.Ok (width, height, _, values) ->
                    createCachedBandPayload
                        width
                        height
                        values
                    |> Result.Ok

        with error ->
            Result.Error error.Message

    let private decodeAllTiffPayloads
        (filePath: string)
        : Result<CachedBandPayload[], string> =

        Log.warn "TIFF CACHE MISS: decoding all channels from %s" filePath

        try
            match TiffLoader.tryReadMultiBandTiff filePath false with
            | Result.Error error ->
                Result.Error error

            | Result.Ok image ->
                Array.init image.bands (fun channelIndex ->
                    let values = 
                        TiffLoader.getBandAsFloat
                            channelIndex
                            image

                    createCachedBandPayload
                        image.width
                        image.height
                        values
                )
                |> Result.Ok

        with error ->
            Result.Error error.Message

    let private readCachedNcBandPayload
        (source: RgbBandSource)
        : Result<CachedBandPayload, string> =

        try
            let key = createPayloadCacheKey source

            let lazyPayload =
                decodedPayloadCache.GetOrAdd(
                    key,
                    fun _ ->
                        Lazy<Result<CachedBandPayload, string>>(
                            (fun () -> decodeNcBandPayload source),
                            LazyThreadSafetyMode.ExecutionAndPublication
                        )
                )

            let result = lazyPayload.Value

            // Allow transient failures to be retried later.
            match result with
            | Result.Ok _ ->
                result

            | Result.Error _ ->
                let mutable removed =
                    Unchecked.defaultof<
                        Lazy<Result<CachedBandPayload, string>>
                    >

                decodedPayloadCache.TryRemove(key, &removed)
                |> ignore

                result

        with error ->
            Result.Error error.Message

    let private readCachedTiffBandPayload
        (source: RgbBandSource)
        : Result<CachedBandPayload, string> =

        try
            let key =
                createTiffPayloadCacheKey source.filePath

            let lazyPayloads =
                decodedTiffPayloadCache.GetOrAdd(
                    key,
                    fun _ ->
                        Lazy<Result<CachedBandPayload[], string>>(
                            (fun () ->
                                decodeAllTiffPayloads key.filePath
                            ),
                            LazyThreadSafetyMode.ExecutionAndPublication
                        )
                )

            match lazyPayloads.Value with
            | Result.Error error ->
                let mutable removed =
                    Unchecked.defaultof<
                        Lazy<Result<CachedBandPayload[], string>>
                    >

                decodedTiffPayloadCache.TryRemove(key, &removed)
                |> ignore

                Result.Error error

            | Result.Ok payloads ->
                if
                    source.channelIndex < 0 ||
                    source.channelIndex >= payloads.Length
                then
                    Result.Error (
                        sprintf
                            "Channel %d in %s is outside the available range 0..%d."
                            source.channelIndex
                            source.filePath
                            (payloads.Length - 1)
                    )
                else
                    Result.Ok payloads.[source.channelIndex]

        with error ->

            Result.Error error.Message

    let private readCachedBandPayload
        (source: RgbBandSource)
        : Result<CachedBandPayload, string> =

        if isNcFile source.filePath then
            readCachedNcBandPayload source
        else
            readCachedTiffBandPayload source

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
        (source: RgbBandSource)
        : Result<RgbBandData, string> =

        if String.IsNullOrWhiteSpace source.filePath then
            Result.Error (
                sprintf
                    "Logical band %d has no image path."
                    source.logicalIndex
            )

        elif not (File.Exists source.filePath) then
            Result.Error (
                sprintf
                    "Image source for logical band %d does not exist: %s"
                    source.logicalIndex
                    source.filePath
            )

        else
            readCachedBandPayload source
            |> Result.map (fun payload ->
                {
                    source = source
                    width = payload.width
                    height = payload.height
                    values = payload.values
                }
            )

    let readBandSourceStatistics
        (source: RgbBandSource)
        : Result<BandStatistics, string> =

        if String.IsNullOrWhiteSpace source.filePath then
            Result.Error (
                sprintf
                    "Logical band %d has no image path."
                    source.logicalIndex
            )

        elif not (File.Exists source.filePath) then
            Result.Error (
                sprintf
                    "Image source for logical band %d does not exist: %s"
                    source.logicalIndex
                    source.filePath
            )

        else
            readCachedBandPayload source
            |> Result.map (fun payload ->
                payload.statistics.Value
            )


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

    let readLogicalBandStatistics
        (sources: list<RgbBandSource>)
        (logicalBandIndex: int)
        : Result<BandStatistics, string> =

        match
            sources
            |> List.tryFind (fun source ->
                source.logicalIndex = logicalBandIndex
            )
        with
        | Some source ->
            readBandSourceStatistics source

        | None ->
            Result.Error (
                sprintf
                    "Could not find logical band %d. Available logical bands are: %s"
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
