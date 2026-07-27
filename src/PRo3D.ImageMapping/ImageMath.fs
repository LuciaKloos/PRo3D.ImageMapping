namespace PRo3D.ImageMapping

open System
open Aardvark.Base

module ImageMath =

    let clamp01 (x : float) : float =
        if x < 0.0 then 0.0
        elif x > 1.0 then 1.0
        else x

    let contrastPointOperation
        (gain : float)
        (midpoint : float)
        (inputValue : float) =

        let b =
            midpoint * (1.0 - gain)

        gain * inputValue + b
        |> clamp01

    let boxBlurMask
        (width : int)
        (height : int)
        (radius : float)
        (mask : float[])
        =
        let r =
            if Double.IsFinite radius then
                int (round radius) |> max 0
            else
                0

        if r = 0 then
            Array.copy mask
        else
            let horizontal =
                Array.zeroCreate<float> mask.Length

            let result =
                Array.zeroCreate<float> mask.Length

            // Horizontal pass
            for y in 0 .. height - 1 do
                let rowStart =
                    y * width

                let mutable sum =
                    0.0

                for x in 0 .. min (width - 1) r do
                    sum <- sum + mask.[rowStart + x]

                for x in 0 .. width - 1 do
                    let left =
                        x - r

                    let right =
                        x + r

                    if left > 0 then
                        sum <- sum - mask.[rowStart + left - 1]

                    if right < width - 1 then
                        sum <- sum + mask.[rowStart + right + 1]

                    let sampleCount =
                        min (width - 1) right - max 0 left + 1

                    horizontal.[rowStart + x] <-
                        sum / float sampleCount

            // Vertical pass
            for x in 0 .. width - 1 do
                let mutable sum =
                    0.0

                for y in 0 .. min (height - 1) r do
                    sum <- sum + horizontal.[y * width + x]

                for y in 0 .. height - 1 do
                    let top =
                        y - r

                    let bottom =
                        y + r

                    if top > 0 then
                        sum <- sum - horizontal.[(top - 1) * width + x]

                    if bottom < height - 1 then
                        sum <- sum + horizontal.[(bottom + 1) * width + x]

                    let sampleCount =
                        min (height - 1) bottom - max 0 top + 1

                    result.[y * width + x] <-
                        sum / float sampleCount

            result

    let smoothstep edge0 edge1 x =
        let t = 
            if edge1 <= edge0 then
                if x >= edge1 then 1.0 else 0.0
            else 
                clamp01 ((x - edge0) / (edge1 - edge0))

        t * t * (3.0 - 2.0 * t)

    
    let percentile
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

    let valueToByte
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
            let gammaCorrected = 
                Math.Pow(normalized, safeGamma) // gamma < 1 -> brightens; gamma > 1 -> darkens

            // produces one byte of each of RGB
            gammaCorrected * 255.0
            |> Math.Round
            |> byte

    let safeRatioClamped
        (minimumDenominator : float)
        (numeratorValues : float[])
        (denominatorValues : float[])
        : float[] =

        if numeratorValues.Length <> denominatorValues.Length then
            invalidArg "denominatorValues" "Ratio bands must contain the same number of pixels."

        Array.init numeratorValues.Length (fun i ->
            let n = numeratorValues.[i]
            let d = denominatorValues.[i]

            if Double.IsFinite n && Double.IsFinite d then
                n / max d minimumDenominator
            else
                0.0
        )

    let safeRatio
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

    type HistogramBin =
        {
            lower    : float
            upper    : float
            count    : int
            fraction : float
        }

    let computeHistogram
        (binCount : int)
        (minimumSignal : float)
        (values : float[])
        : HistogramBin[] =

        let validValues =
            values
            |> Array.filter (fun value ->
                Double.IsFinite value &&
                value > minimumSignal
            )

        if validValues.Length = 0 || binCount <= 0 then
            [||]
        else
            let minimum =
                validValues |> Array.min

            let maximum =
                validValues |> Array.max

            if maximum <= minimum then
                [|
                    {
                        lower = minimum
                        upper = maximum
                        count = validValues.Length
                        fraction = 1.0
                    }
                |]
            else
                let counts =
                    Array.zeroCreate<int> binCount

                let range =
                    maximum - minimum

                for value in validValues do
                    let normalized =
                        (value - minimum) / range

                    let binIndex =
                        int (normalized * float binCount)
                        |> max 0
                        |> min (binCount - 1)

                    counts.[binIndex] <- counts.[binIndex] + 1

                let maxCount =
                    counts |> Array.max |> max 1

                Array.init binCount (fun index ->
                    let lower =
                        minimum + range * float index / float binCount

                    let upper =
                        minimum + range * float (index + 1) / float binCount

                    {
                        lower = lower
                        upper = upper
                        count = counts.[index]
                        fraction = float counts.[index] / float maxCount
                    }
                )