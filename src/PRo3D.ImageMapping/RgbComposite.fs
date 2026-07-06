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


    let private readOptionalLogicalBand
        (sources : list<RgbBandSource>)
        (bandIndex : Option<int>)
        : Result<Option<RgbBandData>, string> =

        match bandIndex with
        | Some index ->
            readLogicalBand sources index
            |> Result.map Some

        | None ->
            Result.Ok None


    let private loadSelectedCompositeBands
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

    let private erodeMask3x3
        (width : int)
        (height : int)
        (mask : bool[])
        : bool[] =

        Array.init mask.Length (fun index ->
            let x = index % width
            let y = index / width

            if x = 0 || y = 0 || x = width - 1 || y = height - 1 then
                false
            else
                let mutable allNeighboursValid = true

                for yy in y - 1 .. y + 1 do
                    for xx in x - 1 .. x + 1 do
                        let neighbourIndex = yy * width + xx

                        if not mask.[neighbourIndex] then
                            allNeighboursValid <- false

                allNeighboursValid
        )

    let private computeValidForeground
        (pixelCount : int)
        (minimumSignal : float)
        (redNumerator : RgbBandData)
        (redDenominator : Option<RgbBandData>)
        (greenNumerator : RgbBandData)
        (greenDenominator : Option<RgbBandData>)
        (blueNumerator : RgbBandData)
        (blueDenominator : Option<RgbBandData>)
        : bool[] =

        let hasSignal value =
            Double.IsFinite value && value > minimumSignal

        let optionalHasSignal (band : Option<RgbBandData>) index =
            match band with
            | Some band ->
                hasSignal band.values.[index]

            | None ->
                true

        Array.init pixelCount (fun i ->
            hasSignal redNumerator.values.[i] &&
            optionalHasSignal redDenominator i &&

            hasSignal greenNumerator.values.[i] &&
            optionalHasSignal greenDenominator i &&

            hasSignal blueNumerator.values.[i] &&
            optionalHasSignal blueDenominator i
        )


    let private computeChannelImage
        (minimumSignal : float)
        (numerator : RgbBandData)
        (denominator : Option<RgbBandData>)
        : float[] =

        match denominator with
        | Some denominator ->
            safeRatio minimumSignal numerator.values denominator.values

        | None ->
            Array.copy numerator.values


    let private computeCompositeChannelImages
        (minimumSignal : float)
        (redNumerator : RgbBandData)
        (redDenominator : Option<RgbBandData>)
        (greenNumerator : RgbBandData)
        (greenDenominator : Option<RgbBandData>)
        (blueNumerator : RgbBandData)
        (blueDenominator : Option<RgbBandData>)
        : float[] * float[] * float[] =

        let redBand =
            computeChannelImage minimumSignal redNumerator redDenominator

        let greenBand =
            computeChannelImage minimumSignal greenNumerator greenDenominator

        let blueBand =
            computeChannelImage minimumSignal blueNumerator blueDenominator

        redBand, greenBand, blueBand


    let private computeDisplayRange
        (validForeground : bool[])
        (lowerPercentileFraction : float)
        (upperPercentileFraction : float)
        (values : float[])
        : float * float =

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


    let private normalizeToRgbBytes
        (pixelCount : int)
        (gamma : float)
        (validForeground : bool[])
        (redMin : float)
        (redMax : float)
        (greenMin : float)
        (greenMax : float)
        (blueMin : float)
        (blueMax : float)
        (redBand : float[])
        (greenBand : float[])
        (blueBand : float[])
        : byte[] * byte[] * byte[] * byte[] * float[] =

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

        redBytes, greenBytes, blueBytes, alphaBytes, luminance

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
        (redDenominatorIndex : Option<int>)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : Option<int>)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : Option<int>)
        (gamma : float)
        (highlightAmount : float)
        (highlightTone   : float)
        (shadowAmount : float)
        (shadowTone   : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            
            match
                loadSelectedCompositeBands
                    sources
                    redNumeratorIndex
                    redDenominatorIndex
                    greenNumeratorIndex
                    greenDenominatorIndex
                    blueNumeratorIndex
                    blueDenominatorIndex
            with
            | Result.Ok (
                redNumerator,
                redDenominator,
                greenNumerator,
                greenDenominator,
                blueNumerator,
                blueDenominator
             ) ->

                let selectedBands =
                    [
                        Some redNumerator
                        redDenominator
                        Some greenNumerator
                        greenDenominator
                        Some blueNumerator
                        blueDenominator
                    ]
                    |> List.choose id

                match validateSameDimensions selectedBands with
                | Result.Error error ->
                    Result.Error error

                | Result.Ok (width, height, pixelCount) ->

                    // EMIT reflectance is floating-point data. Keep this threshold very small:
                    // a too-high threshold can make the whole RGB result transparent.
                    let minimumSignal = 1.0e-5

                    let rawValidForeground =
                        computeValidForeground
                            pixelCount
                            minimumSignal
                            redNumerator
                            redDenominator
                            greenNumerator
                            greenDenominator
                            blueNumerator
                            blueDenominator

                    let validForeground =
                        erodeMask3x3 width height rawValidForeground

                    let redBand, greenBand, blueBand =
                        computeCompositeChannelImages
                            minimumSignal
                            redNumerator
                            redDenominator
                            greenNumerator
                            greenDenominator
                            blueNumerator
                            blueDenominator

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

                    let redMin, redMax =
                        computeDisplayRange
                            validForeground
                            lowerPercentileFraction
                            upperPercentileFraction
                            redBand

                    let greenMin, greenMax =
                        computeDisplayRange
                            validForeground
                            lowerPercentileFraction
                            upperPercentileFraction
                            greenBand

                    let blueMin, blueMax =
                        computeDisplayRange
                            validForeground
                            lowerPercentileFraction
                            upperPercentileFraction
                            blueBand

                    let rgbImage =
                        PixImage<byte>(
                            Col.Format.RGBA,
                            V2i(width, height)
                        )

                    let redBytes, greenBytes, blueBytes, alphaBytes, luminance =
                        normalizeToRgbBytes
                            pixelCount
                            gamma
                            validForeground
                            redMin
                            redMax
                            greenMin
                            greenMax
                            blueMin
                            blueMax
                            redBand
                            greenBand
                            blueBand

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

                    let brightnessGain =
                        if Double.IsFinite brightness then
                            1.0 + (brightness |> max -1.0 |> min 1.0)
                        else
                            1.0

                    let adjustedRedBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedGreenBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedBlueBytes =
                        Array.zeroCreate<byte> pixelCount

                    // Third pass: darken highlights according to Amount * highlight mask.
                    for index in 0 .. pixelCount - 1 do
                        if alphaBytes.[index] > 0uy then
                            let highlightMaskValue = highlightMask.[index] 

                            let shadowMaskValue = shadowMask.[index] 

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

                            let brightenedRed =
                                clamp01 (saturatedRed * brightnessGain)

                            let brightenedGreen =
                                clamp01 (saturatedGreen * brightnessGain)

                            let brightenedBlue =
                                clamp01 (saturatedBlue * brightnessGain)

                            adjustedRedBytes.[index] <-
                                byte (round (255.0 * brightenedRed))

                            adjustedGreenBytes.[index] <-
                                byte (round (255.0 * brightenedGreen))

                            adjustedBlueBytes.[index] <-
                                byte (round (255.0 * brightenedBlue))

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


            | Result.Error error ->
                Result.Error error
    
        with error ->
            Result.Error error.Message