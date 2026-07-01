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

module RgbComposite =
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


    let averageBandData
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

    let readAverageLogicalBand
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

    // raw band ratio values
    //-> percentile stretch / black-white clip
    //-> gamma
    //-> RGB bytes
    //-> luminance masks for highlights/shadows/midtones
    //-> highlight / shadow / midtone contrast adjustment
    //-> saturation adjustment
    //-> final RGBA image
    let createRgbCompositePixImageFromSources
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : int)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : int)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : int)
        (gamma : float)
        (highlightAmount : float)
        (highlightTone   : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone   : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        : Result<PixImage<byte>, string> =

        try
            let usesNetCDF =
                sources
                |> List.exists (fun source -> isNcFile source.filePath)

            // i am pretty sure the results are better without averaging
            let averageRadius = 1
            //    if usesNetCDF then 0 else 1

            let maxWavelengthDistanceNm = 0.0
            //    if usesNetCDF then 0.0 else 35.0

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


                    let blackClipFraction =
                        if Double.IsFinite blackClipPercentile then
                            blackClipPercentile / 100.0
                            |> max 0.0
                            |> min 1.0
                        else
                            0.0

                    let whiteClipFraction =
                        if Double.IsFinite whiteClipPercentile then
                            whiteClipPercentile / 100.0
                            |> max 0.0
                            |> min 1.0
                        else
                            0.0

                    let lowerPercentileFraction =
                        blackClipFraction

                    let upperPercentileFraction =
                        1.0 - whiteClipFraction

                    // safety check
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

                    let redBytes =
                        Array.zeroCreate<byte> pixelCount

                    let greenBytes =
                        Array.zeroCreate<byte> pixelCount

                    let blueBytes =
                        Array.zeroCreate<byte> pixelCount

                    let alphaBytes =
                        Array.zeroCreate<byte> pixelCount

                    let luminance =
                        Array.zeroCreate<float> pixelCount

                    // First pass: create display RGB bytes and luminance once for the whole image.
                    for index in 0 .. pixelCount - 1 do
                        if validForeground.[index] then
                            let r =
                                valueToByte gamma redMin redMax redBand.[index]

                            let g =
                                valueToByte gamma greenMin greenMax greenBand.[index]

                            let b =
                                valueToByte gamma blueMin blueMax blueBand.[index]

                            redBytes.[index] <- r
                            greenBytes.[index] <- g
                            blueBytes.[index] <- b
                            alphaBytes.[index] <- 255uy

                            let rf =
                                float r / 255.0

                            let gf =
                                float g / 255.0

                            let bf =
                                float b / 255.0

                            luminance.[index] <-
                                0.2126 * rf + 0.7152 * gf + 0.0722 * bf

                            match debugRgbBytes with
                            | Some bytes ->
                                let offset =
                                    index * 3

                                bytes.[offset + 0] <- r
                                bytes.[offset + 1] <- g
                                bytes.[offset + 2] <- b

                            | None ->
                                ()

                    let highlightStart =
                        1.0 - clamp01 highlightTone

                    let highlightMask =
                        Array.init pixelCount (fun index ->
                            if alphaBytes.[index] > 0uy then
                                smoothstep highlightStart 1.0 luminance.[index]
                            else
                                0.0
                        )

                    let shadowEnd =
                        clamp01 shadowTone

                    let shadowMask =
                        Array.init pixelCount (fun index ->
                            if alphaBytes.[index] > 0uy then
                                1.0 - smoothstep 0.0 shadowEnd luminance.[index]
                            else
                                0.0
                        )

                    let boxBlurMask
                        (width : int)
                        (height : int)
                        (radius : int)
                        (mask : float[])
                        : float[] =

                        if radius <= 0 then
                            Array.copy mask
                        else
                            let temp =
                                Array.zeroCreate<float> mask.Length

                            let result =
                                Array.zeroCreate<float> mask.Length

                            // Horizontal pass.
                            for y in 0 .. height - 1 do
                                for x in 0 .. width - 1 do
                                    let mutable sum = 0.0
                                    let mutable count = 0
                                    let x0 = max 0 (x - radius)
                                    let x1 = min (width - 1) (x + radius)

                                    for xx in x0 .. x1 do
                                        let index =
                                            y * width + xx

                                        if alphaBytes.[index] > 0uy then
                                            sum <- sum + mask.[index]
                                            count <- count + 1

                                    temp.[y * width + x] <-
                                        if count > 0 then sum / float count else 0.0

                            // Vertical pass.
                            for y in 0 .. height - 1 do
                                for x in 0 .. width - 1 do
                                    let mutable sum = 0.0
                                    let mutable count = 0
                                    let y0 = max 0 (y - radius)
                                    let y1 = min (height - 1) (y + radius)

                                    for yy in y0 .. y1 do
                                        let index =
                                            yy * width + x

                                        if alphaBytes.[index] > 0uy then
                                            sum <- sum + temp.[index]
                                            count <- count + 1

                                    result.[y * width + x] <-
                                        if count > 0 then sum / float count else 0.0

                            result

                    let radiusHighlightPixels =
                        if Double.IsFinite highlightRadius then
                            int (round highlightRadius) |> max 0
                        else
                            0

                    let radiusShadowPixels =
                        if Double.IsFinite shadowRadius then
                            int (round shadowRadius) |> max 0
                        else    
                            0

                    let localHighlightMask =
                        boxBlurMask width height radiusHighlightPixels highlightMask

                    let localShadowMask =
                        boxBlurMask width height radiusShadowPixels shadowMask

                    let clampedAmountHighlight =
                        clamp01 highlightAmount
                    
                    let clampedAmountShadow =
                        clamp01 shadowAmount

                    let clampedMidtoneContrastGainFactor =
                        if Double.IsFinite midtoneContrastGainFactor then  
                            midtoneContrastGainFactor |> max -1.0 |> min 1.0
                        else 
                            0.0

                    let midtoneGainFactor =
                        if clampedMidtoneContrastGainFactor >= 0.0 then
                            1.0 + 2.0 * clampedMidtoneContrastGainFactor
                        else 
                            1.0 + clampedMidtoneContrastGainFactor

                    let fixedMidtoneLow =
                        0.25

                    let fixedMidtoneHigh =
                        0.75

                    let midtoneLow =
                        clamp01 fixedMidtoneLow

                    let midtoneHigh =
                        clamp01 fixedMidtoneHigh

                    let midtoneMidpoint =
                        (midtoneLow + midtoneHigh) * 0.5

                    let validMidtoneRange =
                        midtoneHigh > midtoneLow

                    let midtoneMask =
                        Array.init pixelCount (fun index ->
                            if alphaBytes.[index] > 0uy && validMidtoneRange then
                                let l =
                                    luminance.[index]

                                if l >= midtoneLow && l <= midtoneHigh then
                                    1.0
                                else
                                    0.0
                            else
                                0.0
                        )

                    // UI saturation is centered around 0:
                    // -1.0 -> grayscale, 0.0 -> unchanged, +1.0 -> oversaturated.
                    // Internally this is a chroma multiplier:
                    //  0.0 -> grayscale, 1.0 -> unchanged, 2.0 -> oversaturated.
                    let saturationGain =
                        if Double.IsFinite saturation then
                            1.0 + (saturation |> max -1.0 |> min 1.0)
                        else
                            1.0


                    let adjustedRedBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedGreenBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedBlueBytes =
                        Array.zeroCreate<byte> pixelCount

                    // Third pass: darken highlights according to Amount * local highlight mask.
                    for index in 0 .. pixelCount - 1 do
                        if alphaBytes.[index] > 0uy then
                            let highlightMaskValue = 
                                localHighlightMask.[index]

                            let shadowMaskValue =
                                localShadowMask.[index]

                            let midtoneMaskValue =
                                midtoneMask.[index]

                            let applyAdjustments (channel : byte) =
                                let c =
                                    float channel / 255.0

                                let afterHighlight =
                                    c * (1.0 - clampedAmountHighlight * highlightMaskValue)

                                let afterShadow =
                                    afterHighlight + (1.0 - afterHighlight) * clampedAmountShadow * shadowMaskValue

                                let afterMidtoneContrast =
                                    contrastPointOperation midtoneGainFactor midtoneMidpoint afterShadow

                                let result =
                                    if midtoneMaskValue > 0.0 then
                                        afterMidtoneContrast
                                    else
                                        afterShadow

                                clamp01 result
                                
                            let r =
                                applyAdjustments redBytes.[index]

                            let g =
                                applyAdjustments greenBytes.[index]

                            let b =
                                applyAdjustments blueBytes.[index]

                            let luminanceAfterAdjustments =
                                0.2126 * r + 0.7152 * g + 0.0722 * b

                            let saturatedRed =
                                luminanceAfterAdjustments + saturationGain * ( r - luminanceAfterAdjustments )

                            let saturatedGreen =
                                luminanceAfterAdjustments + saturationGain * ( g - luminanceAfterAdjustments )

                            let saturatedBlue =
                                luminanceAfterAdjustments + saturationGain * ( b - luminanceAfterAdjustments )

                            adjustedRedBytes.[index] <-
                                byte (round (255.0 * saturatedRed))

                            adjustedGreenBytes.[index] <-
                                byte (round (255.0 * saturatedGreen))

                            adjustedBlueBytes.[index] <-
                                byte (round (255.0 * saturatedBlue))

                    rgbImage
                        .GetMatrix<C4b>()
                        .SetByCoord(fun (position : V2l) ->
                            let x =
                                int position.X

                            let y =
                                int position.Y

                            let index =
                                y * width + x

                            if alphaBytes.[index] > 0uy then
                                C4b(
                                    adjustedRedBytes.[index],
                                    adjustedGreenBytes.[index],
                                    adjustedBlueBytes.[index],
                                    255uy
                                )
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