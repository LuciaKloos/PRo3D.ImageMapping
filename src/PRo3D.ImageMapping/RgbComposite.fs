namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO

open ImageMath
open BandHandler

open Aardvark.Rendering

module RgbComposite =


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
            // Nonlinear darkening: move toward c�.
            c + (-b) * (c * c - c)
        else
            c

    let private createMidtoneMask 
        (pixelCount : int)
        (alphaBytes : byte[]) 
        (luminance : float[]) =
            let midtoneMask =
                        Array.init pixelCount (fun index ->
                            if alphaBytes.[index] > 0uy then
                                let l =
                                    luminance.[index]

                                if l >= Midtone.init.low && l <= Midtone.init.high then
                                    1.0
                                else
                                    0.0
                            else
                                0.0
                        )
            midtoneMask


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

            let c = float channel / 255.0

            // Highlight correction
            let highlightCorrected =
                Math.Pow(c, Gamma.init.highlights)

            let highlightStrength =
                clampedAmountHighlight
                * highlightMask.[index]
                |> clamp01

            let highlightDelta =
                highlightStrength
                * (highlightCorrected - c)

            // Shadow correction
            let shadowCorrected =
                Math.Pow(c, Gamma.init.shadows)

            let shadowStrength =
                clampedAmountShadow
                * shadowMask.[index]
                |> clamp01

            let shadowDelta =
                shadowStrength
                * (shadowCorrected - c)

            // Midtone contrast correction
            let midtoneCorrected =
                contrastPointOperation
                    midtoneGainFactor
                    midtoneMidpoint
                    c

            let midtoneStrength =
                midtoneMask.[index]
                |> clamp01

            let midtoneDelta =
                midtoneStrength
                * (midtoneCorrected - c)

            // Combine independent changes
            let adjusted =
                c
                + highlightDelta
                + shadowDelta
                + midtoneDelta
                |> clamp01

            adjusted
        
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
            match loadSelectedTransferFunctionBand sources bandIndex with
            | Result.Error error ->
                Result.Error error

            | Result.Ok band ->

                let width =
                    band.width

                let height =
                    band.height

                let pixelCount =
                    band.values.Length

                let image =
                    PixImage<byte>(
                        Col.Format.RGBA,
                        V2i(width, height)
                    )

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

                // First pass:
                // raw single band -> transfer function / grayscale RGB bytes.
                for index in 0 .. pixelCount - 1 do
                    let value =
                        band.values.[index]

                    if Double.IsFinite value then
                        let byteValue =
                            valueToByte gamma minimum maximum value

                        let color =
                            if useFalseColor then
                                let normalizedValue =
                                    float byteValue / 255.0

                                sampleColorMap colorMap normalizedValue
                            else
                                C4b(
                                    byteValue,
                                    byteValue,
                                    byteValue,
                                    255uy
                                )

                        redBytes.[index] <- color.R
                        greenBytes.[index] <- color.G
                        blueBytes.[index] <- color.B
                        alphaBytes.[index] <- 255uy

                        let rf =
                            float color.R / 255.0

                        let gf =
                            float color.G / 255.0

                        let bf =
                            float color.B / 255.0

                        luminance.[index] <-
                            0.2126 * rf + 0.7152 * gf + 0.0722 * bf

                let shadowMask, highlightMask = 
                    createShadowsHighlightsMask
                        highlightTone
                        highlightRadius
                        shadowTone
                        shadowRadius
                        pixelCount
                        alphaBytes
                        luminance
                        height
                        width

                let clampedAmountHighlight =
                    clamp01 highlightAmount

                let clampedAmountShadow =
                    clamp01 shadowAmount

                let midtoneGainFactor = calculateMidtoneContrast midtoneContrastGainFactor

                let midtoneMask = createMidtoneMask pixelCount alphaBytes luminance

                let saturationGain = calculateSaturationGain saturation

                let adjustedRedBytes =
                    Array.zeroCreate<byte> pixelCount

                let adjustedGreenBytes =
                    Array.zeroCreate<byte> pixelCount

                let adjustedBlueBytes =
                    Array.zeroCreate<byte> pixelCount

                // Second pass:
                // transfer-function RGB -> highlights/shadows/midtones/saturation/brightness.
                for index in 0 .. pixelCount - 1 do
                    if alphaBytes.[index] > 0uy then
                        
                        let r =
                            applyAdjustments 
                                redBytes.[index]
                                clampedAmountHighlight
                                clampedAmountShadow
                                midtoneGainFactor
                                Midtone.init.mid
                                highlightMask
                                shadowMask
                                midtoneMask
                                index

                        let g =
                            applyAdjustments 
                                greenBytes.[index]
                                clampedAmountHighlight
                                clampedAmountShadow
                                midtoneGainFactor
                                Midtone.init.mid
                                highlightMask
                                shadowMask
                                midtoneMask
                                index

                        let b =
                            applyAdjustments 
                                blueBytes.[index]
                                clampedAmountHighlight
                                clampedAmountShadow
                                midtoneGainFactor
                                Midtone.init.mid
                                highlightMask
                                shadowMask
                                midtoneMask
                                index

                        let luminanceAfterAdjustments =
                            Luminance.init.red * r + Luminance.init.green * g + Luminance.init.blue * b

                        let saturatedRed =
                            luminanceAfterAdjustments + saturationGain * (r - luminanceAfterAdjustments)

                        let saturatedGreen =
                            luminanceAfterAdjustments + saturationGain * (g - luminanceAfterAdjustments)

                        let saturatedBlue =
                            luminanceAfterAdjustments + saturationGain * (b - luminanceAfterAdjustments)

                        let brightenedRed =
                            adjustBrightness brightness saturatedRed

                        let brightenedGreen =
                            adjustBrightness brightness saturatedGreen

                        let brightenedBlue =
                            adjustBrightness brightness saturatedBlue

                        adjustedRedBytes.[index] <-
                            byte (round (255.0 * brightenedRed))

                        adjustedGreenBytes.[index] <-
                            byte (round (255.0 * brightenedGreen))

                        adjustedBlueBytes.[index] <-
                            byte (round (255.0 * brightenedBlue))

                image
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

                Result.Ok image

        with error ->
            Result.Error error.Message

    

    let private computeValidForeground
        (pixelCount : int)
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

                averageSignal > MinimumObjectSignal.init.signal
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
                    Luminance.init.red * rf + Luminance.init.green * gf + Luminance.init.blue * bf

        redBytes, greenBytes, blueBytes, alphaBytes, luminance

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

            let width =
                sourceImage.Size.X

            let height =
                sourceImage.Size.Y

            let pixelCount =
                width * height

            let sourceMatrix =
                sourceImage.GetMatrix<C4b>()

            let redBytes =
                Array.zeroCreate<byte> pixelCount

            let greenBytes =
                Array.zeroCreate<byte> pixelCount

            let blueBytes =
                Array.zeroCreate<byte> pixelCount

            let alphaBytes =
                Array.zeroCreate<byte> pixelCount

            let redValues =
                Array.zeroCreate<float> pixelCount

            let greenValues =
                Array.zeroCreate<float> pixelCount

            let blueValues =
                Array.zeroCreate<float> pixelCount

            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    let index =
                        y * width + x

                    let color =
                        sourceMatrix.[x, y]

                    redBytes.[index] <- color.R
                    greenBytes.[index] <- color.G
                    blueBytes.[index] <- color.B
                    alphaBytes.[index] <- color.A

                    redValues.[index] <- float color.R / 255.0
                    greenValues.[index] <- float color.G / 255.0
                    blueValues.[index] <- float color.B / 255.0

            let validForeground =
                Array.init pixelCount (fun index ->
                    alphaBytes.[index] > 0uy
                )

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

            let shouldApplyClip =
                blackClipFraction > 0.0 || whiteClipFraction > 0.0

            let redBytes, greenBytes, blueBytes, luminance =
                if shouldApplyClip then
                    let lowerPercentileFraction =
                        blackClipFraction

                    let upperPercentileFraction =
                        1.0 - whiteClipFraction

                    let lowerPercentileFraction, upperPercentileFraction =
                        if upperPercentileFraction <= lowerPercentileFraction then
                            lowerPercentileFraction, min 1.0 (lowerPercentileFraction + 0.01)
                        else
                            lowerPercentileFraction, upperPercentileFraction

                    let redMin, redMax =
                        computeDisplayRange validForeground lowerPercentileFraction upperPercentileFraction redValues

                    let greenMin, greenMax =
                        computeDisplayRange validForeground lowerPercentileFraction upperPercentileFraction greenValues

                    let blueMin, blueMax =
                        computeDisplayRange validForeground lowerPercentileFraction upperPercentileFraction blueValues

                    let clippedRedBytes, clippedGreenBytes, clippedBlueBytes, _, clippedLuminance =
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

                    clippedRedBytes, clippedGreenBytes, clippedBlueBytes, clippedLuminance
                else
                    let luminance =
                        Array.init pixelCount (fun index ->
                            if alphaBytes.[index] > 0uy then
                                Luminance.init.red * redValues.[index]
                                + Luminance.init.green * greenValues.[index]
                                + Luminance.init.blue * blueValues.[index]
                            else
                                0.0
                        )

                    redBytes, greenBytes, blueBytes, luminance


            let shadowMask, highlightMask = 
                    createShadowsHighlightsMask
                        highlightTone
                        highlightRadius
                        shadowTone
                        shadowRadius
                        pixelCount
                        alphaBytes
                        luminance
                        height
                        width

            
            let midtoneMask = createMidtoneMask pixelCount alphaBytes luminance

            let clampedAmountHighlight =
                clamp01 highlightAmount

            let clampedAmountShadow =
                clamp01 shadowAmount
            
            let midtoneGainFactor = calculateMidtoneContrast midtoneContrastGainFactor

            let saturationGain = calculateSaturationGain saturation

            let output =
                PixImage<byte>(
                    Col.Format.RGBA,
                    V2i(width, height)
                )

            output
                .GetMatrix<C4b>()
                .SetByCoord(fun (position : V2l) ->
                    let x =
                        int position.X

                    let y =
                        int position.Y

                    let index =
                        y * width + x

                    if alphaBytes.[index] > 0uy then
                        

                        let r =
                            applyAdjustments    
                                redBytes.[index]
                                clampedAmountHighlight
                                clampedAmountShadow
                                midtoneGainFactor
                                Midtone.init.mid
                                highlightMask
                                shadowMask
                                midtoneMask
                                index

                        let g =
                            applyAdjustments 
                                greenBytes.[index]
                                clampedAmountHighlight
                                clampedAmountShadow
                                midtoneGainFactor
                                Midtone.init.mid
                                highlightMask
                                shadowMask
                                midtoneMask
                                index

                        let b =
                            applyAdjustments 
                                blueBytes.[index]
                                clampedAmountHighlight
                                clampedAmountShadow
                                midtoneGainFactor
                                Midtone.init.mid
                                highlightMask
                                shadowMask
                                midtoneMask
                                index

                        let l =
                            Luminance.init.red * r + Luminance.init.green * g + Luminance.init.blue * b

                        let r =
                            l + saturationGain * (r - l)

                        let g =
                            l + saturationGain * (g - l)

                        let b =
                            l + saturationGain * (b - l)

                        let brightenedRed =
                            adjustBrightness brightness r

                        let brightenedGreen =
                            adjustBrightness brightness g

                        let brightenedBlue =
                            adjustBrightness brightness b

                        C4b(
                            byte (round (255.0 * clamp01 (brightenedRed))),
                            byte (round (255.0 * clamp01 (brightenedGreen))),
                            byte (round (255.0 * clamp01 (brightenedBlue))),
                            alphaBytes.[index]
                        )
                    else
                        C4b(0uy, 0uy, 0uy, 0uy)
                )
            |> ignore

            Result.Ok output

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

    // raw band ratio values
    //-> percentile stretch / black-white clip
    //-> gamma
    //-> RGB bytes
    //-> luminance masks for highlights/shadows/midtones
    //-> highlight / shadow / midtone contrast adjustment
    //-> saturation adjustment
    //-> final RGBA image
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
        (highlightTone   : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone   : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        (saturation : float)
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            
            match
                loadSelectedCompositeRatioBands
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
                    // Ratio clamp.
                    // Prevents denominator division explosions but does not create alpha holes.
                    let minimumDenominator =
                        1.0e-3

                    let validForeground =
                        computeValidForeground
                            pixelCount
                            redNumerator
                            greenNumerator
                            blueNumerator

                    let redBand, greenBand, blueBand =
                        computeCompositeChannelImages
                            minimumDenominator
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

                    let shadowMask, highlightMask = 
                        createShadowsHighlightsMask
                            highlightTone
                            highlightRadius
                            shadowTone
                            shadowRadius
                            pixelCount
                            alphaBytes
                            luminance
                            height
                            width
                                                
                    let clampedAmountHighlight =
                        clamp01 highlightAmount
                    
                    let clampedAmountShadow =
                        clamp01 shadowAmount
                    
                    let midtoneGainFactor = calculateMidtoneContrast midtoneContrastGainFactor

                    
                    let midtoneMask = createMidtoneMask pixelCount alphaBytes luminance

                    let saturationGain = calculateSaturationGain saturation

                    let adjustedRedBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedGreenBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedBlueBytes =
                        Array.zeroCreate<byte> pixelCount

                    // Third pass: darken highlights according to Amount * highlight mask.
                    for index in 0 .. pixelCount - 1 do
                        if alphaBytes.[index] > 0uy then
                            
                            let r =
                                applyAdjustments 
                                    redBytes.[index]
                                    clampedAmountHighlight
                                    clampedAmountShadow
                                    midtoneGainFactor
                                    Midtone.init.mid
                                    highlightMask
                                    shadowMask
                                    midtoneMask
                                    index

                            let g =
                                applyAdjustments 
                                    greenBytes.[index]
                                    clampedAmountHighlight
                                    clampedAmountShadow
                                    midtoneGainFactor
                                    Midtone.init.mid
                                    highlightMask
                                    shadowMask
                                    midtoneMask
                                    index

                            let b =
                                applyAdjustments 
                                    blueBytes.[index]
                                    clampedAmountHighlight
                                    clampedAmountShadow
                                    midtoneGainFactor
                                    Midtone.init.mid
                                    highlightMask
                                    shadowMask
                                    midtoneMask
                                    index

                            let luminanceAfterAdjustments =
                                Luminance.init.red * r + Luminance.init.green * g + Luminance.init.blue * b

                            let saturatedRed =
                                luminanceAfterAdjustments + saturationGain * ( r - luminanceAfterAdjustments )

                            let saturatedGreen =
                                luminanceAfterAdjustments + saturationGain * ( g - luminanceAfterAdjustments )

                            let saturatedBlue =
                                luminanceAfterAdjustments + saturationGain * ( b - luminanceAfterAdjustments )

                            let brightenedRed =
                                adjustBrightness brightness saturatedRed

                            let brightenedGreen =
                                adjustBrightness brightness saturatedGreen

                            let brightenedBlue =
                                adjustBrightness brightness saturatedBlue

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
    
    
    // raw band ratio values
    //-> percentile stretch / black-white clip
    //-> gamma
    //-> RGB bytes
    //-> luminance masks for highlights/shadows/midtones
    //-> highlight / shadow / midtone contrast adjustment
    //-> saturation adjustment
    //-> final RGBA image
    let createRgbMappingPixImageFromSources
        (sources : list<RgbBandSource>)
        (redBandIndex : int)
        (greenBandIndex : int)
        (blueBandIndex : int)
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
        (brightness : float)
        : Result<PixImage<byte>, string> =

        try
            
            match
                loadSelectedRgbMappingBands
                    sources
                    redBandIndex
                    greenBandIndex
                    blueBandIndex
            with
            | Result.Ok (
                redBandData,
                greenBandData,
                blueBandData
             ) ->

                let selectedBands =
                    [
                        redBandData
                        greenBandData
                        blueBandData
                    ]

                match validateSameDimensions selectedBands with
                | Result.Error error ->
                    Result.Error error

                | Result.Ok (width, height, pixelCount) ->

                    let rawValidForeground =
                        computeValidForeground
                            pixelCount
                            redBandData
                            greenBandData
                            blueBandData


                    let validForeground = rawValidForeground

                    let redBand =
                        Array.copy redBandData.values

                    let greenBand =
                        Array.copy greenBandData.values

                    let blueBand =
                        Array.copy blueBandData.values

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

                    let shadowMask, highlightMask = 
                        createShadowsHighlightsMask
                            highlightTone
                            highlightRadius
                            shadowTone
                            shadowRadius
                            pixelCount
                            alphaBytes
                            luminance
                            height
                            width

                    let clampedAmountHighlight =
                        clamp01 highlightAmount
                    
                    let clampedAmountShadow =
                        clamp01 shadowAmount
                    
                    let midtoneGainFactor = calculateMidtoneContrast midtoneContrastGainFactor
                    
                    let midtoneMask = createMidtoneMask pixelCount alphaBytes luminance

                    let saturationGain = calculateSaturationGain saturation

                    let adjustedRedBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedGreenBytes =
                        Array.zeroCreate<byte> pixelCount

                    let adjustedBlueBytes =
                        Array.zeroCreate<byte> pixelCount

                    // Third pass: darken highlights according to Amount * highlight mask.
                    for index in 0 .. pixelCount - 1 do
                        if alphaBytes.[index] > 0uy then
                            
                            let r =
                                applyAdjustments 
                                    redBytes.[index]
                                    clampedAmountHighlight
                                    clampedAmountShadow
                                    midtoneGainFactor
                                    Midtone.init.mid
                                    highlightMask
                                    shadowMask
                                    midtoneMask
                                    index

                            let g =
                                applyAdjustments 
                                    greenBytes.[index]
                                    clampedAmountHighlight
                                    clampedAmountShadow
                                    midtoneGainFactor
                                    Midtone.init.mid
                                    highlightMask
                                    shadowMask
                                    midtoneMask
                                    index

                            let b =
                                applyAdjustments 
                                    blueBytes.[index]
                                    clampedAmountHighlight
                                    clampedAmountShadow
                                    midtoneGainFactor
                                    Midtone.init.mid
                                    highlightMask
                                    shadowMask
                                    midtoneMask
                                    index

                            let luminanceAfterAdjustments =
                                Luminance.init.red * r + Luminance.init.green * g + Luminance.init.blue * b

                            let saturatedRed =
                                luminanceAfterAdjustments + saturationGain * ( r - luminanceAfterAdjustments )

                            let saturatedGreen =
                                luminanceAfterAdjustments + saturationGain * ( g - luminanceAfterAdjustments )

                            let saturatedBlue =
                                luminanceAfterAdjustments + saturationGain * ( b - luminanceAfterAdjustments )

                            let brightenedRed =
                                adjustBrightness brightness saturatedRed

                            let brightenedGreen =
                                adjustBrightness brightness saturatedGreen

                            let brightenedBlue =
                                adjustBrightness brightness saturatedBlue

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