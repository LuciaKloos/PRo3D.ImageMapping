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

    type RgbHistogram =
        {
            red : int[]     // 256 bins each
            green : int[]
            blue : int[]
            count : int     // number of foreground pixels (alpha > 0)
        }


    let loadRgbHistogram (path : string) : Result<RgbHistogram, string> =
        BandHandler.readCachedRgbImagePayload path
        |> Result.map (fun payload ->
            let r = Array.zeroCreate<int> 256
            let g = Array.zeroCreate<int> 256
            let b = Array.zeroCreate<int> 256
            let m = payload.sourceMatrix
            let mutable n = 0
            
            for y in 0 .. payload.height - 1 do
                for x in 0 .. payload.width - 1 do
                    let c = m.[x, y]
                    if c.A > 0uy then
                        r.[int c.R] <- r.[int c.R] + 1
                        g.[int c.G] <- g.[int c.G] + 1
                        b.[int c.B] <- b.[int c.B] + 1
                        n <- n + 1

            { red = r; green = g; blue = b; count = n })

    // Value (0..1) at a 0-based position in the sorted list of foreground values.
    // Same result as sorting and indexing, but at most 256 steps
    let private valueAtRank (hist : int[]) (rank : int) =
        let mutable b = 0
        let mutable cum = hist.[0]
        while cum <= rank && b < 255 do
            b <- b + 1
            cum <- cum + hist.[b]

        float b / 255.0

    // Matches ImageMath.percentile exactly: nearest rank, Math.Round (banker's rounding), clamped.
    let private histogramPercentile (hist : int[]) (count : int) (fraction : float) =
        if count = 0 then
            0.0
        else
            let index =
                fraction * float (count - 1)
                |> Math.Round
                |> int
                |> max 0
                |> min (count - 1)

            valueAtRank hist index

    // returns ClipMin, ClipMax uniforms
    let computeClipRange 
        (h : RgbHistogram)
        (blackClipPercentile : float)
        (whiteClipPercentile : float)
        : V3d * V3d =

        let toFraction p = if Double.IsFinite p then clamp01 (p / 100.0) else 0.0
        let black = toFraction blackClipPercentile
        let white = toFraction whiteClipPercentile

        if h.count = 0 || (black <= 0.0 && white <= 0.0) then
            V3d.Zero, V3d.One // no clipping -> identity in the shader

        else
           let lower = black
           let upper =
               if 1.0 - white <= lower then
                   min 1.0 (lower + 0.01)
               else
                   1.0 - white

           let range hist = 
                let mn = histogramPercentile hist h.count lower
                let mx = histogramPercentile hist h.count upper
                if mx <= mn then
                    mn, mn + 1.0
                else
                    mn, mx

           let rMin, rMax = range h.red
           let gMin, gMax = range h.green
           let bMin, bMax = range h.blue

           V3d(rMin, gMin, bMin), V3d(rMax, gMax, bMax)

    let createSourceRgbTexture (sourceImagePath : aval<Option<string>>) : aval<ITexture> =
        sourceImagePath
        |> AVal.map (function
            | Some path when File.Exists path ->
                match BandHandler.readCachedRgbImagePayload path with
                | Result.Ok p ->
                    let img = PixImage<byte>(Col.Format.RGBA, V2i(p.width, p.height))
                    let d = img.Volume.Data
                    let m = p.sourceMatrix
                    for y in 0 .. p.height - 1 do
                        for x in 0 .. p.width - 1 do
                            let c = m.[x, y]
                            let o = (y * p.width + x) * 4
                            d.[o] <- c.R; d.[o + 1] <- c.G; d.[o + 2] <- c.B; d.[o + 3] <- c.A

                    PixTexture2d(PixImageMipMap [| img :> PixImage |], false) :> ITexture
                | Result.Error e ->
                    Log.warn "Could not load source RGB image: %s" e
                    DefaultTextures.checkerboard.GetValue()
            | _ -> 
                DefaultTextures.checkerboard.GetValue()
        )


    let sampleColorMap
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
        : Result<PixImage<byte>, string> =

        try
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let lap (name : string) =
                Log.line "[timing] %s: %.2f ms" name sw.Elapsed.TotalMilliseconds
                sw.Restart()

            let cachedPayload =
                BandHandler.readCachedRgbImagePayload path
                |> Result.defaultWith failwith

            lap "load (cached payload)"

            let width =
                cachedPayload.width

            let height =
                cachedPayload.height

            let sourceMatrix =
                cachedPayload.sourceMatrix

            let pixelCount =
                width * height

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

            lap "unpack pixels"


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
                        C4b(
                            redBytes.[index],
                            greenBytes.[index],
                            blueBytes.[index],
                            255uy
                        )
                    else
                        C4b(0uy, 0uy, 0uy, 0uy)
                )
            |> ignore
            
            lap "build output image"

            Result.Ok (output)

        with error ->
            Result.Error error.Message

    let createPlainRgbPixImage
        (sourceImagePath : aval<Option<string>>)
        : aval<Result<PixImage<byte>, string>> =

        AVal.custom (fun token ->

            match sourceImagePath.GetValue token with
            | Some path when File.Exists path ->
                createPlainRgbPixImageFromPath
                    path

            | _ ->
                Result.Error "No plain RGB source image is available."
        )

    let createPlainRgbTexture
        (adjustedImage : aval<Result<PixImage<byte>, string>>)
        : aval<ITexture> =

        adjustedImage
        |> AVal.map (function
            | Result.Ok image ->
                PixTexture2d(
                    PixImageMipMap [| image :> PixImage |],
                    false
                ) :> ITexture

            | Result.Error error ->
                Log.warn "Could not create plain RGB image texture: %s" error
                DefaultTextures.checkerboard.GetValue()
        )

    // raw band ratio values
    let createRgbRatioCompositePixImageFromSources
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : Option<int>)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : Option<int>)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : Option<int>)
        (gamma : float)
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
                    
                    let redMin, redMax =
                        computeDisplayRange
                            validForeground
                            0.0
                            1.0
                            redBand

                    let greenMin, greenMax =
                        computeDisplayRange
                            validForeground
                            0.0
                            1.0
                            greenBand

                    let blueMin, blueMax =
                        computeDisplayRange
                            validForeground
                            0.0
                            1.0
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
                                    redBytes.[index],
                                    greenBytes.[index],
                                    blueBytes.[index],
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
    
    // display range -> bytes -> PixImage
    let private renderRgbBands
        (width : int)
        (height : int)
        (pixelCount : int)
        (gamma : float)
        (validForeground : bool[])
        (redBand : float[])
        (greenBand : float[])
        (blueBand : float[])
        : PixImage<byte> =

        let range band = computeDisplayRange validForeground 0.0 1.0 band
        let redMin, redMax = range redBand
        let greenMin, greenMax = range greenBand
        let blueMin, blueMax = range blueBand

        let redBytes, greenBytes, blueBytes, alphaBytes, _ =
            normalizeToRgbBytes
                pixelCount gamma validForeground
                redMin redMax greenMin greenMax blueMin blueMax
                redBand greenBand blueBand

        let rgbImage = PixImage<byte>(Col.Format.RGBA, V2i(width, height))

        rgbImage
            .GetMatrix<C4b>()
            .SetByCoord(fun (position : V2l) ->
                let index = int position.Y * width + int position.X
                if alphaBytes.[index] > 0uy then
                    C4b(redBytes.[index], greenBytes.[index], blueBytes.[index], 255uy)
                else
                    C4b(0uy, 0uy, 0uy, 0uy))
        |> ignore

        rgbImage

    // pixel is foreground if every referenced band is finite there
    // and their mean is above the minimum object signal
    let private computeValidForegroundFromBands
        (pixelCount : int)
        (bands : RgbBandData list)
        : bool[] =

        let count = float bands.Length

        Array.init pixelCount (fun i ->
            let mutable sum = 0.0
            let mutable allFinite = true

            for band in bands do
                let v = band.values.[i]
                if Double.IsFinite v then sum <- sum + v
                else allFinite <- false

            allFinite && sum / count > MinimumObjectSignal.init.signal)

    // loads only the bands (0-based logical indices) the expressions reference
    let private loadBands
        (sources : list<RgbBandSource>)
        (indices : int list)
        : Result<Map<int, RgbBandData>, string> =

        (Result.Ok Map.empty, indices)
        ||> List.fold (fun acc idx ->
            acc |> Result.bind (fun m ->
                readLogicalBand sources idx
                |> Result.map (fun b -> Map.add idx b m)))

    let createRgbRatioCompositeFromExpressions
        (sources : list<RgbBandSource>)
        ((r, g, b) : BandExpression.BandExpression
                     * BandExpression.BandExpression
                     * BandExpression.BandExpression)
        (gamma : float)
        : Result<PixImage<byte>, string> =

        try
            let indices =
                [ r; g; b ]
                |> List.map BandExpression.referenceBands
                |> Set.unionMany
                |> Set.map (fun n -> n - 1)   // 1-based "B6" -> 0-based logical index
                |> Set.toList

            match loadBands sources indices with
            | Result.Error e -> Result.Error e

            | Result.Ok bands ->
                let bandList = bands |> Map.toList |> List.map snd

                match validateSameDimensions bandList with
                | Result.Error e -> Result.Error e

                | Result.Ok (width, height, pixelCount) ->
                    let minimumDenominator = 1.0e-3

                    let channel e =
                        let f =
                            BandExpression.compile
                                (fun idx -> bands.[idx].values)
                                minimumDenominator
                                e
                        Array.init pixelCount f

                    let redBand = channel r
                    let greenBand = channel g
                    let blueBand = channel b

                    let validForeground =
                        computeValidForegroundFromBands pixelCount bandList

                    renderRgbBands
                        width height pixelCount gamma validForeground
                        redBand greenBand blueBand
                    |> Result.Ok

        with error ->
            Result.Error error.Message
    
    // raw band ratio values
    let createRgbMappingPixImageFromSources
        (sources : list<RgbBandSource>)
        (redBandIndex : int)
        (greenBandIndex : int)
        (blueBandIndex : int)
        (gamma : float)
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

                    let redMin, redMax =
                        computeDisplayRange
                            validForeground
                            0.0
                            1.0
                            redBand

                    let greenMin, greenMax =
                        computeDisplayRange
                            validForeground
                            0.0
                            1.0
                            greenBand

                    let blueMin, blueMax =
                        computeDisplayRange
                            validForeground
                            0.0
                            1.0
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
                                    redBytes.[index],
                                    greenBytes.[index],
                                    blueBytes.[index],
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