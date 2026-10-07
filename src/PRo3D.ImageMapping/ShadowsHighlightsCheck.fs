namespace PRo3D.ImageMapping

open System
open System.Globalization
open Aardvark.Base
open PRo3D.ImageMapping.Model

// Measurable checks 1-3 for the shadows/highlights test cases (see CLAUDE.md).
// Values on a 0-255 scale; only pixels with output alpha > 0 are evaluated.
module ShadowsHighlightsCheck =

    type CheckCase =
        | Identity
        | ShadowsOnly
        | HighlightsOnly

    let tryParseCase (name : string) =
        match name.ToLowerInvariant() with
        | "identity" -> Some Identity
        | "shadows-only" -> Some ShadowsOnly
        | "highlights-only" -> Some HighlightsOnly
        | _ -> None

    // identity: allowed max difference per channel
    let identityTolerance = 1

    // shadows-only / highlights-only: minimum change of the region's mean luminance
    // (0-255 scale) that counts as "clearly"
    let minLuminanceChange = 2.0

    type private Orientation =
        | AsIs
        | FlippedVertically
        | MirroredHorizontally

    type private Stats =
        {
            count : int64
            maxAbsDiff : int[]                  // R, G, B
            meanAbsDiff : float[]
            pixelsOverTolerance : int64
            increased : int64                   // pixels with any channel increased
            decreased : int64
            maxIncrease : int
            maxDecrease : int
            darkCount : int64                   // input luminance < 0.25
            darkMeanIn : float
            darkMeanOut : float
            brightCount : int64                 // input luminance > 0.75
            brightMeanIn : float
            brightMeanOut : float
        }

    let private load (path : string) =
        PixImage<byte>(path).ToPixImage<byte>(Col.Format.RGBA).GetMatrix<C4b>()

    let private luminance (c : C4b) =
        Luminance.init.red * float c.R + Luminance.init.green * float c.G + Luminance.init.blue * float c.B

    let private compare (input : Matrix<byte, C4b>) (output : Matrix<byte, C4b>) (orientation : Orientation) =
        let w = int input.SX
        let h = int input.SY

        let maxAbsDiff = Array.zeroCreate 3
        let sumAbsDiff = Array.zeroCreate 3
        let mutable count = 0L
        let mutable over = 0L
        let mutable increased = 0L
        let mutable decreased = 0L
        let mutable maxIncrease = 0
        let mutable maxDecrease = 0
        let mutable darkCount = 0L
        let mutable darkIn = 0.0
        let mutable darkOut = 0.0
        let mutable brightCount = 0L
        let mutable brightIn = 0.0
        let mutable brightOut = 0.0

        for y in 0 .. h - 1 do
            for x in 0 .. w - 1 do
                let ox, oy =
                    match orientation with
                    | AsIs -> x, y
                    | FlippedVertically -> x, h - 1 - y
                    | MirroredHorizontally -> w - 1 - x, y

                let a = input.[x, y]
                let b = output.[ox, oy]

                if b.A > 0uy then
                    count <- count + 1L

                    let diffs = [| int b.R - int a.R; int b.G - int a.G; int b.B - int a.B |]
                    let mutable isOver = false
                    let mutable isInc = false
                    let mutable isDec = false

                    for c in 0 .. 2 do
                        let d = diffs.[c]
                        let ad = abs d
                        maxAbsDiff.[c] <- max maxAbsDiff.[c] ad
                        sumAbsDiff.[c] <- sumAbsDiff.[c] + float ad
                        if ad > identityTolerance then isOver <- true
                        if d > 0 then
                            isInc <- true
                            maxIncrease <- max maxIncrease d
                        if d < 0 then
                            isDec <- true
                            maxDecrease <- max maxDecrease -d

                    if isOver then over <- over + 1L
                    if isInc then increased <- increased + 1L
                    if isDec then decreased <- decreased + 1L

                    let lumIn = luminance a
                    let lumOut = luminance b

                    if lumIn < 0.25 * 255.0 then
                        darkCount <- darkCount + 1L
                        darkIn <- darkIn + lumIn
                        darkOut <- darkOut + lumOut

                    if lumIn > 0.75 * 255.0 then
                        brightCount <- brightCount + 1L
                        brightIn <- brightIn + lumIn
                        brightOut <- brightOut + lumOut

        let mean sum n = if n > 0L then sum / float n else 0.0

        {
            count = count
            maxAbsDiff = maxAbsDiff
            meanAbsDiff = sumAbsDiff |> Array.map (fun s -> mean s count)
            pixelsOverTolerance = over
            increased = increased
            decreased = decreased
            maxIncrease = maxIncrease
            maxDecrease = maxDecrease
            darkCount = darkCount
            darkMeanIn = mean darkIn darkCount
            darkMeanOut = mean darkOut darkCount
            brightCount = brightCount
            brightMeanIn = mean brightIn brightCount
            brightMeanOut = mean brightOut brightCount
        }

    let private fmt (v : float) = v.ToString("0.000", CultureInfo.InvariantCulture)

    // prints the report and returns the exit code: 0 = PASS, 1 = FAIL, 2 = not comparable
    let run (case : CheckCase) (inputPath : string) (outputPath : string) : int =
        let ext = IO.Path.GetExtension(inputPath).ToLowerInvariant()
        if ext = ".jpg" || ext = ".jpeg" then
            printfn "WARNING: JPEG input; decoder differences make the identity check unreliable."

        let input = load inputPath
        let output = load outputPath

        if input.Size <> output.Size then
            printfn "FAIL: size mismatch, input %dx%d, output %dx%d" input.SX input.SY output.SX output.SY
            2
        else
            let s = compare input output AsIs

            printfn "%A: %dx%d, evaluated pixels (alpha > 0): %d" case input.SX input.SY s.count
            printfn "  max |diff| R/G/B = %d/%d/%d, mean |diff| R/G/B = %s/%s/%s"
                s.maxAbsDiff.[0] s.maxAbsDiff.[1] s.maxAbsDiff.[2]
                (fmt s.meanAbsDiff.[0]) (fmt s.meanAbsDiff.[1]) (fmt s.meanAbsDiff.[2])

            let failures =
                match case with
                | Identity ->
                    printfn "  pixels with diff > %d: %d" identityTolerance s.pixelsOverTolerance
                    if s.pixelsOverTolerance = 0L then
                        []
                    else
                        // identity failures are usually wiring errors; look for a flipped orientation
                        let orientationHints =
                            [ FlippedVertically, "output is flipped vertically"
                              MirroredHorizontally, "output is mirrored horizontally" ]
                            |> List.filter (fun (o, _) -> (compare input output o).pixelsOverTolerance = 0L)
                            |> List.map snd

                        sprintf "max difference exceeds %d" identityTolerance :: orientationHints

                | ShadowsOnly ->
                    let delta = s.darkMeanOut - s.darkMeanIn
                    printfn "  pixels with a decreased channel: %d (max decrease %d)" s.decreased s.maxDecrease
                    printfn "  dark region (lum < 0.25, %d px): mean luminance %s -> %s (delta %s)"
                        s.darkCount (fmt s.darkMeanIn) (fmt s.darkMeanOut) (fmt delta)
                    [
                        if s.decreased > 0L then "some channels decreased"
                        if s.darkCount = 0L then "no dark pixels in input"
                        elif delta < minLuminanceChange then
                            sprintf "dark-region luminance increase < %s" (fmt minLuminanceChange)
                    ]

                | HighlightsOnly ->
                    let delta = s.brightMeanIn - s.brightMeanOut
                    printfn "  pixels with an increased channel: %d (max increase %d)" s.increased s.maxIncrease
                    printfn "  bright region (lum > 0.75, %d px): mean luminance %s -> %s (delta %s)"
                        s.brightCount (fmt s.brightMeanIn) (fmt s.brightMeanOut) (fmt (-delta))
                    [
                        if s.increased > 0L then "some channels increased"
                        if s.brightCount = 0L then "no bright pixels in input"
                        elif delta < minLuminanceChange then
                            sprintf "bright-region luminance decrease < %s" (fmt minLuminanceChange)
                    ]

            match failures with
            | [] ->
                printfn "PASS"
                0
            | _ ->
                printfn "FAIL: %s" (String.Join("; ", failures))
                1
