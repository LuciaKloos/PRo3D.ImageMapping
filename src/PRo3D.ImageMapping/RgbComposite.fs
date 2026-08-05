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
open Aardvark.Rendering

module RgbComposite =

    // returns a readable list of abailable logical bands
    let availableLogicalBandsMessage (sources : list<RgbBandSource>) =
        sources
        |> List.map (fun source -> source.logicalIndex)
        |> List.distinct
        |> List.sort
        |> List.map string
        |> String.concat ", "

    // converts a list of AdaptiveImage to a list of RgbBandSource records
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

    // loads one selected band into a floating-point array
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


    // searches source list by logical band index and loads the corresponding band into a floating-point array
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

    // verifies that all bands have the same dimensions and returns the width, height, and pixel count if they do
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

    // handles optional denominator 
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

    //loads the six possible inputs for band-ratio RGB
    let private loadSelectedCompositeRatioBands
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
    
    // loads exactly three bands for rgb mapping
    let private loadSelectedRgbMappingBands
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

    // loads the single band used for grayscale or false-color transfer-function
    let private loadSelectedTransferFunctionBand
        (sources : list<RgbBandSource>)
        (bandIndex : int)
        : Result<RgbBandData, string> =

        readLogicalBand sources bandIndex

    // converts a normalized floating-point value from [0,1] into an 8-bit value from 0 to 255
    let private byteFrom01 value =
        value
        |> clamp01
        |> fun v -> byte (round (v * 255.0))


    let private sampleColorMap
        (colorMap : ColorMap)
        (normalizedValue : float)
        : C4b =

        let t =
            clamp01 normalizedValue

        let r, g, b =
            match colorMap with
            | ColorMap.Magma ->
                1.5 * t,
                t * t * 0.8,
                0.25 + 0.75 * t * t * t

            | ColorMap.Plasma ->
                0.2 + 0.8 * t,
                0.1 + 0.8 * (1.0 - abs (t - 0.5) * 2.0),
                1.0 - 0.7 * t

            | ColorMap.Viridis ->
                0.25 + 0.55 * t,
                0.15 + 0.85 * t,
                0.75 - 0.55 * t

            | ColorMap.PiYG ->
                1.0 - t,
                0.2 + 0.8 * (1.0 - abs (t - 0.5) * 2.0),
                t

            | ColorMap.TwilightShifted ->
                0.5 + 0.5 * sin (6.28318530718 * t),
                0.5 + 0.5 * sin (6.28318530718 * (t + 0.33)),
                0.5 + 0.5 * sin (6.28318530718 * (t + 0.66))

            | ColorMap.Vanimo ->
                t,
                1.0 - abs (t - 0.5) * 2.0,
                1.0 - t

            | _ ->
                t, t, t

        C4b(
            byteFrom01 r,
            byteFrom01 g,
            byteFrom01 b,
            255uy
        )

    // adjusts brightness non-linearly
    let private adjustBrightness
        (brightness : float)
        (channel : float)
        =
        let c =
            clamp01 channel

        let b =
            if Double.IsFinite brightness then
                brightness |> max -1.0 |> min 1.0
            else
                0.0

        if b > 0.0 then
            // Nonlinear brightening: move toward sqrt(c).
            c + b * (Math.Sqrt(c) - c)
        elif b < 0.0 then
            // Nonlinear darkening: move toward c².
            c + (-b) * (c * c - c)
        else
            c

    // receives image luminance and creates two masks: one for shadows and one for highlights, based on the specified tone and radius parameters
    let createShadowsHighlightsMask 
        (highlightTone : float)
        (highlightRadius : float)
        (shadowTone : float)
        (shadowRadius : float)
        (pixelCount : int)
        (alphaBytes : byte[])
        (luminance : float[])
        (height : int)
        (width : int) = 

        let highlightStart =
            1.0 - clamp01 highlightTone

        let rawHighlightMask =
            Array.init pixelCount (fun index ->
                if alphaBytes.[index] > 0uy then
                    smoothstep highlightStart 1.0 luminance.[index]
                else
                    0.0
            )

        // uses radius to determine blur radius
        let highlightMask =
            boxBlurMask
                width
                height
                highlightRadius
                rawHighlightMask

        let shadowEnd =
            clamp01 shadowTone

        let rawShadowMask =
            Array.init pixelCount (fun index ->
                if alphaBytes.[index] > 0uy then
                    1.0 - smoothstep 0.0 shadowEnd luminance.[index]
                else
                    0.0
            )

        let shadowMask =
            boxBlurMask
                width
                height
                shadowRadius
                rawShadowMask

        shadowMask, highlightMask

    let applyAdjustments 
        (channel : byte)
        (clampedAmountHighlight : float)
        (clampedAmountShadow : float)
        (midtoneGainFactor : float)
        (midtoneMidpoint : float)
        (highlightMask : float[])
        (shadowMask : float[])
        (midtoneMask : float[])
        (index : int) =

            // TODO: include in UI
            let highlightGamma =
                1.4

            let shadowGamma =
                0.5

            let c =
                float channel / 255.0

            // c^gamma, where gamma > 1, produces the darker candidate.
            let highlightCorrected =
                Math.Pow(c, highlightGamma)

            let highlightStrength =
                clampedAmountHighlight
                * highlightMask.[index]
                |> clamp01

            let afterHighlight =
                c
                + highlightStrength
                    * (highlightCorrected - c)

            // Gamma below 1 produces a brighter shadow candidate.
            let shadowCorrected =
                Math.Pow(c, shadowGamma)

            let shadowStrength =
                clampedAmountShadow
                * shadowMask.[index]
                |> clamp01

            let afterShadow =
                c
                + shadowStrength
                    * (shadowCorrected - c)

            let afterMidtoneContrast =
                contrastPointOperation
                    midtoneGainFactor
                    midtoneMidpoint
                    c

            let midtoneStrength =
                midtoneMask.[index]
                |> clamp01

            let midtoneDelta =
                midtoneStrength
                * (afterMidtoneContrast - c)

            let highlightDelta =
                afterHighlight - c

            let shadowDelta =
                afterShadow - c

            let adjusted = c + highlightDelta + shadowDelta + midtoneDelta

            clamp01 adjusted
        
    type private AdjustmentContext =
        {
            highlightAmount : float
            shadowAmount : float
            midtoneGainFactor : float
            midtoneMidpoint : float
            saturationGain : float
            highlightMask : float[]
            shadowMask : float[]
            midtoneMask : float[]
        }

    let private clampFinite
        (minimum : float)
        (maximum : float)
        (fallback : float)
        (value : float) =

        if Double.IsFinite value then
            value |> max minimum |> min maximum
        else
            fallback

    let private createMidtoneMask
        (alphaBytes : byte[])
        (luminance : float[]) =

        // TODO: make these parameters adjustable in the UI
        let midtoneLow = 0.25
        let midtoneHigh = 0.75

        let mask =
            Array.init alphaBytes.Length (fun index ->
                if alphaBytes.[index] > 0uy then
                    let value = luminance.[index]
                    if value >= midtoneLow && value <= midtoneHigh then 1.0 else 0.0
                else
                    0.0
            )

        mask, (midtoneLow + midtoneHigh) * 0.5

    let private createAdjustmentContext
        (width : int)
        (height : int)
        (alphaBytes : byte[])
        (luminance : float[])
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (saturation : float) =

        let shadowMask, highlightMask =
            createShadowsHighlightsMask
                highlightTone
                highlightRadius
                shadowTone
                shadowRadius
                alphaBytes.Length
                alphaBytes
                luminance
                height
                width

        let clampedMidtoneGain =
            clampFinite -1.0 1.0 0.0 midtoneContrastGainFactor

        let midtoneGainFactor =
            if clampedMidtoneGain >= 0.0 then
                1.0 + 2.0 * clampedMidtoneGain
            else
                1.0 + clampedMidtoneGain

        let midtoneMask, midtoneMidpoint =
            createMidtoneMask alphaBytes luminance

        {
            highlightAmount = clamp01 highlightAmount
            shadowAmount = clamp01 shadowAmount
            midtoneGainFactor = midtoneGainFactor
            midtoneMidpoint = midtoneMidpoint
            saturationGain = 1.0 + clampFinite -1.0 1.0 0.0 saturation
            highlightMask = highlightMask
            shadowMask = shadowMask
            midtoneMask = midtoneMask
        }

    let private adjustRgbPixel
        (context : AdjustmentContext)
        (brightness : float)
        (red : byte)
        (green : byte)
        (blue : byte)
        (index : int) =

        let adjust channel =
            applyAdjustments
                channel
                context.highlightAmount
                context.shadowAmount
                context.midtoneGainFactor
                context.midtoneMidpoint
                context.highlightMask
                context.shadowMask
                context.midtoneMask
                index

        let r = adjust red
        let g = adjust green
        let b = adjust blue

        let luminance =
            0.2126 * r + 0.7152 * g + 0.0722 * b

        let adjustChroma value =
            luminance + context.saturationGain * (value - luminance)
            |> adjustBrightness brightness
            |> byteFrom01

        adjustChroma r, adjustChroma g, adjustChroma b

    let private renderAdjustedRgbImage
        (width : int)
        (height : int)
        (redBytes : byte[])
        (greenBytes : byte[])
        (blueBytes : byte[])
        (alphaBytes : byte[])
        (luminance : float[])
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (saturation : float)
        (brightness : float) =

        let context =
            createAdjustmentContext
                width
                height
                alphaBytes
                luminance
                highlightAmount
                highlightTone
                highlightRadius
                shadowAmount
                shadowTone
                shadowRadius
                midtoneContrastGainFactor
                saturation

        let image =
            PixImage<byte>(Col.Format.RGBA, V2i(width, height))

        image
            .GetMatrix<C4b>()
            .SetByCoord(fun (position : V2l) ->
                let index = int position.Y * width + int position.X
                let alpha = alphaBytes.[index]

                if alpha > 0uy then
                    let r, g, b =
                        adjustRgbPixel
                            context
                            brightness
                            redBytes.[index]
                            greenBytes.[index]
                            blueBytes.[index]
                            index

                    C4b(r, g, b, alpha)
                else
                    C4b(0uy, 0uy, 0uy, 0uy)
            )
        |> ignore

        image

    let createTransferFunctionPixImageFromSource
        (sources : list<RgbBandSource>)
        (bandIndex : int)
        (minimum : float)
        (maximum : float)
        (gamma : float)
        (useFalseColor : bool)
        (colorMap : ColorMap)
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            loadSelectedTransferFunctionBand sources bandIndex
            |> Result.map (fun band ->
                let pixelCount = band.values.Length
                let redBytes = Array.zeroCreate<byte> pixelCount
                let greenBytes = Array.zeroCreate<byte> pixelCount
                let blueBytes = Array.zeroCreate<byte> pixelCount
                let alphaBytes = Array.zeroCreate<byte> pixelCount
                let luminance = Array.zeroCreate<float> pixelCount

                for index in 0 .. pixelCount - 1 do
                    let value = band.values.[index]

                    if Double.IsFinite value then
                        let byteValue = valueToByte gamma minimum maximum value
                        let color =
                            if useFalseColor then
                                sampleColorMap colorMap (float byteValue / 255.0)
                            else
                                C4b(byteValue, byteValue, byteValue, 255uy)

                        redBytes.[index] <- color.R
                        greenBytes.[index] <- color.G
                        blueBytes.[index] <- color.B
                        alphaBytes.[index] <- 255uy
                        luminance.[index] <-
                            0.2126 * (float color.R / 255.0)
                            + 0.7152 * (float color.G / 255.0)
                            + 0.0722 * (float color.B / 255.0)

                renderAdjustedRgbImage
                    band.width
                    band.height
                    redBytes
                    greenBytes
                    blueBytes
                    alphaBytes
                    luminance
                    highlightAmount
                    highlightTone
                    highlightRadius
                    shadowAmount
                    shadowTone
                    shadowRadius
                    midtoneContrastGainFactor
                    saturation
                    brightness
            )
        with error ->
            Result.Error error.Message

    let private computeValidForeground
        (pixelCount : int)
        (minimumObjectSignal : float)
        (redNumerator : RgbBandData)
        (greenNumerator : RgbBandData)
        (blueNumerator : RgbBandData)
        : bool[] =

        let isFinite value =
            Double.IsFinite value

        Array.init pixelCount (fun i ->
            let r = redNumerator.values.[i]
            let g = greenNumerator.values.[i]
            let b = blueNumerator.values.[i]

            if isFinite r && isFinite g && isFinite b then
                let averageSignal =
                    (r + g + b) / 3.0

                averageSignal > minimumObjectSignal
            else
                false
        )


    let private computeChannelImage
        (minimumSignal : float)
        (numerator : RgbBandData)
        (denominator : Option<RgbBandData>)
        : float[] =

        match denominator with
        | Some denominator ->
            safeRatioClamped minimumSignal numerator.values denominator.values

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

    let private clipPercentileFractions
        (blackClipPercentile : float)
        (whiteClipPercentile : float) =

        let lower =
            clampFinite 0.0 1.0 0.0 (blackClipPercentile / 100.0)

        let upper =
            1.0 - clampFinite 0.0 1.0 0.0 (whiteClipPercentile / 100.0)

        if upper <= lower then
            lower, min 1.0 (lower + 0.01)
        else
            lower, upper

    let private calculateLuminance
        (alphaBytes : byte[])
        (redValues : float[])
        (greenValues : float[])
        (blueValues : float[]) =

        Array.init alphaBytes.Length (fun index ->
            if alphaBytes.[index] > 0uy then
                0.2126 * redValues.[index]
                + 0.7152 * greenValues.[index]
                + 0.0722 * blueValues.[index]
            else
                0.0
        )

    let createPlainRgbPixImageFromPath
        (path : string)
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            let sourceImage =
                PixImage<byte>(path).ToPixImage<byte>(Col.Format.RGBA)

            let width = sourceImage.Size.X
            let height = sourceImage.Size.Y
            let pixelCount = width * height
            let sourceMatrix = sourceImage.GetMatrix<C4b>()

            let redBytes = Array.zeroCreate<byte> pixelCount
            let greenBytes = Array.zeroCreate<byte> pixelCount
            let blueBytes = Array.zeroCreate<byte> pixelCount
            let alphaBytes = Array.zeroCreate<byte> pixelCount
            let redValues = Array.zeroCreate<float> pixelCount
            let greenValues = Array.zeroCreate<float> pixelCount
            let blueValues = Array.zeroCreate<float> pixelCount

            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    let index = y * width + x
                    let color = sourceMatrix.[x, y]

                    redBytes.[index] <- color.R
                    greenBytes.[index] <- color.G
                    blueBytes.[index] <- color.B
                    alphaBytes.[index] <- color.A
                    redValues.[index] <- float color.R / 255.0
                    greenValues.[index] <- float color.G / 255.0
                    blueValues.[index] <- float color.B / 255.0

            let validForeground =
                alphaBytes |> Array.map (fun alpha -> alpha > 0uy)

            let lower, upper =
                clipPercentileFractions blackClipPercentile whiteClipPercentile

            let shouldApplyClip =
                lower > 0.0 || upper < 1.0

            let finalRedBytes, finalGreenBytes, finalBlueBytes, luminance =
                if shouldApplyClip then
                    let redMin, redMax =
                        computeDisplayRange validForeground lower upper redValues

                    let greenMin, greenMax =
                        computeDisplayRange validForeground lower upper greenValues

                    let blueMin, blueMax =
                        computeDisplayRange validForeground lower upper blueValues

                    let r, g, b, _, l =
                        normalizeToRgbBytes
                            pixelCount
                            1.0
                            validForeground
                            redMin
                            redMax
                            greenMin
                            greenMax
                            blueMin
                            blueMax
                            redValues
                            greenValues
                            blueValues

                    r, g, b, l
                else
                    redBytes,
                    greenBytes,
                    blueBytes,
                    calculateLuminance alphaBytes redValues greenValues blueValues

            renderAdjustedRgbImage
                width
                height
                finalRedBytes
                finalGreenBytes
                finalBlueBytes
                alphaBytes
                luminance
                highlightAmount
                highlightTone
                highlightRadius
                shadowAmount
                shadowTone
                shadowRadius
                midtoneContrastGainFactor
                saturation
                brightness
            |> Result.Ok
        with error ->
            Result.Error error.Message

    let createPlainRgbTexture
        (sourceImagePath : aval<Option<string>>)
        (shadowsHighlightsAdjustmentsRenderSettings : ShadowsHighlightsAdjustmentsRenderSettings)
        : aval<ITexture> =

        AVal.custom (fun token ->

            let highlightAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.highlightAdjustments.GetValue token

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token

            let midtoneContrastValue =
                shadowsHighlightsAdjustmentsRenderSettings.midtoneContrast.GetValue token

            let blackWhiteClipValue =
                shadowsHighlightsAdjustmentsRenderSettings.blackWhiteClip.GetValue token

            let saturationValue =
                shadowsHighlightsAdjustmentsRenderSettings.saturation.GetValue token

            let brightnessValue =
                shadowsHighlightsAdjustmentsRenderSettings.brightness.GetValue token

            match sourceImagePath.GetValue token with
            | Some path when File.Exists path ->
                match
                    createPlainRgbPixImageFromPath
                        path
                        highlightAdjustmentValue.amount.value
                        highlightAdjustmentValue.tone.value
                        highlightAdjustmentValue.radius.value
                        shadowAdjustmentValue.amount.value
                        shadowAdjustmentValue.tone.value
                        shadowAdjustmentValue.radius.value
                        midtoneContrastValue.gainFactor.value
                        blackWhiteClipValue.blackClipPercentile.value
                        blackWhiteClipValue.whiteClipPercentile.value
                        saturationValue.gainFactor.value
                        brightnessValue.gainFactor.value
                with
                | Result.Ok image ->
                    PixTexture2d(
                        PixImageMipMap [|
                            image :> PixImage
                        |],
                        false
                    ) :> ITexture

                | Result.Error error ->
                    Log.warn "Could not create plain RGB image texture: %s" error
                    DefaultTextures.checkerboard.GetValue()

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    let private createCompositePixImage
        (redBandData : RgbBandData)
        (greenBandData : RgbBandData)
        (blueBandData : RgbBandData)
        (additionalBands : list<RgbBandData>)
        (createChannels : unit -> float[] * float[] * float[])
        (gamma : float)
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        let selectedBands =
            redBandData :: greenBandData :: blueBandData :: additionalBands

        validateSameDimensions selectedBands
        |> Result.map (fun (width, height, pixelCount) ->
            let minimumObjectSignal = 0.002

            let validForeground =
                computeValidForeground
                    pixelCount
                    minimumObjectSignal
                    redBandData
                    greenBandData
                    blueBandData

            let redBand, greenBand, blueBand = createChannels ()
            let lower, upper =
                clipPercentileFractions blackClipPercentile whiteClipPercentile

            let redMin, redMax =
                computeDisplayRange validForeground lower upper redBand

            let greenMin, greenMax =
                computeDisplayRange validForeground lower upper greenBand

            let blueMin, blueMax =
                computeDisplayRange validForeground lower upper blueBand

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

            renderAdjustedRgbImage
                width
                height
                redBytes
                greenBytes
                blueBytes
                alphaBytes
                luminance
                highlightAmount
                highlightTone
                highlightRadius
                shadowAmount
                shadowTone
                shadowRadius
                midtoneContrastGainFactor
                saturation
                brightness
        )

    // raw band values / ratios
    // -> percentile stretch / black-white clip
    // -> gamma
    // -> RGB bytes
    // -> luminance masks for highlights/shadows/midtones
    // -> highlight / shadow / midtone contrast adjustment
    // -> saturation and brightness adjustment
    // -> final RGBA image
    let createRgbRatioCompositePixImageFromSources
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : Option<int>)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : Option<int>)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : Option<int>)
        (gamma : float)
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            loadSelectedCompositeRatioBands
                sources
                redNumeratorIndex
                redDenominatorIndex
                greenNumeratorIndex
                greenDenominatorIndex
                blueNumeratorIndex
                blueDenominatorIndex
            |> Result.bind (fun (
                redNumerator,
                redDenominator,
                greenNumerator,
                greenDenominator,
                blueNumerator,
                blueDenominator) ->

                let denominatorBands =
                    [ redDenominator; greenDenominator; blueDenominator ]
                    |> List.choose id

                createCompositePixImage
                    redNumerator
                    greenNumerator
                    blueNumerator
                    denominatorBands
                    (fun () ->
                        computeCompositeChannelImages
                            1.0e-3
                            redNumerator
                            redDenominator
                            greenNumerator
                            greenDenominator
                            blueNumerator
                            blueDenominator
                    )
                    gamma
                    highlightAmount
                    highlightTone
                    highlightRadius
                    shadowAmount
                    shadowTone
                    shadowRadius
                    midtoneContrastGainFactor
                    blackClipPercentile
                    whiteClipPercentile
                    saturation
                    brightness
            )
        with error ->
            Result.Error error.Message

    let createRgbMappingPixImageFromSources
        (sources : list<RgbBandSource>)
        (redBandIndex : int)
        (greenBandIndex : int)
        (blueBandIndex : int)
        (gamma : float)
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            loadSelectedRgbMappingBands
                sources
                redBandIndex
                greenBandIndex
                blueBandIndex
            |> Result.bind (fun (redBand, greenBand, blueBand) ->
                createCompositePixImage
                    redBand
                    greenBand
                    blueBand
                    []
                    (fun () ->
                        Array.copy redBand.values,
                        Array.copy greenBand.values,
                        Array.copy blueBand.values
                    )
                    gamma
                    highlightAmount
                    highlightTone
                    highlightRadius
                    shadowAmount
                    shadowTone
                    shadowRadius
                    midtoneContrastGainFactor
                    blackClipPercentile
                    whiteClipPercentile
                    saturation
                    brightness
            )
        with error ->
            Result.Error error.Message

