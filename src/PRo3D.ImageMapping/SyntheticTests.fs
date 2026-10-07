namespace PRo3D.ImageMapping

open System
open System.IO
open System.Globalization
open Aardvark.Base
open Aardvark.Rendering

// Synthetic shadows/highlights tests (--synthetic-tests): generates test images in code,
// runs them through the headless GPU pipeline and checks the result.
// Inputs and outputs are written as PNG to the output directory for inspection.
module SyntheticTests =

    type private Result =
        {
            name : string
            passed : bool
            maxDiff : float
            meanDiff : float
            details : list<string>
        }

    let private fmt (v : float) = v.ToString("0.000", CultureInfo.InvariantCulture)

    // ---- image generation ------------------------------------------------------------

    let private createGreyImage (width : int) (height : int) (value : int -> int -> byte) =
        let image = PixImage<byte>(Col.Format.RGBA, V2i(width, height))
        image.GetMatrix<C4b>().SetByCoord(fun (c : V2l) ->
            let v = value (int c.X) (int c.Y)
            C4b(v, v, v, 255uy)
        ) |> ignore
        image

    let private toByte (value : float) =
        byte (Math.Round(value * 255.0) |> max 0.0 |> min 255.0)

    let patchLuminances = [| 0.1; 0.3; 0.5; 0.7; 0.9 |]
    let private patchSize = 160

    // patches are wider than the blur footprint; only the centre region is evaluated
    let private patchEvalHalfSize = 24

    let private createPatchImage () =
        createGreyImage (patchSize * patchLuminances.Length) patchSize (fun x _ ->
            toByte patchLuminances.[x / patchSize])

    // one pixel per input level: on a wider gradient the input stays constant over several
    // pixels while the blurred mask keeps changing, which gives 1-level dips after rounding
    // (an 8-bit staircase artifact, not a property of the formula)
    let private gradientWidth = 256

    let private createGradientImage () =
        createGreyImage gradientWidth 32 (fun x _ ->
            byte (Math.Round(float x * 255.0 / float (gradientWidth - 1))))

    let private createEdgeImage () =
        createGreyImage 512 128 (fun x _ -> if x < 256 then 0uy else 255uy)

    // ---- expected values ---------------------------------------------------------------

    // GLSL smoothstep, as used in Shaders.hshShadowsHighlights
    let private smoothstep (edge0 : float) (edge1 : float) (x : float) =
        let t = (x - edge0) / (edge1 - edge0) |> max 0.0 |> min 1.0
        t * t * (3.0 - 2.0 * t)

    // the mask is stored in an Rgba8 target, i.e. quantized to 1/255
    let private quantize (v : float) = Math.Round(v * 255.0) / 255.0

    // Expected output (0-1) of a uniform grey region: the blurred mask of a uniform region
    // equals the raw mask, so mask (hshShadowsHighlights) + Shaders.applyShadowsHighlights.
    let expectedUniform (config : TestConfig) (c : float) =
        let highlightMask = smoothstep (1.0 - (config.highlightTone |> max 0.0 |> min 1.0)) 1.0 c
        let shadowMask = 1.0 - smoothstep 0.0 (config.shadowTone |> max 0.0 |> min 1.0) c

        Shaders.applyShadowsHighlights
            c
            (quantize shadowMask)
            (quantize highlightMask)
            config.highlightAmount
            config.shadowAmount

    // ---- checks ----------------------------------------------------------------------

    // allowed |measured - expected| on a 0-255 scale (8-bit rounding of mask and output)
    let private expectedTolerance = 1.0

    let private checkPatches (name : string) (config : TestConfig) (output : PixImage<byte>) =
        let m = output.GetMatrix<C4b>()
        let centreY = patchSize / 2

        let rows =
            patchLuminances
            |> Array.mapi (fun i lum ->
                let inputByte = toByte lum
                let expected = expectedUniform config (float inputByte / 255.0) * 255.0
                let centreX = i * patchSize + patchSize / 2

                let diffs =
                    [|
                        for y in centreY - patchEvalHalfSize .. centreY + patchEvalHalfSize - 1 do
                            for x in centreX - patchEvalHalfSize .. centreX + patchEvalHalfSize - 1 do
                                let c = m.[x, y]
                                for v in [| c.R; c.G; c.B |] do
                                    yield float v - expected
                    |]

                let measuredMean = expected + Array.average diffs
                let maxAbs = diffs |> Array.map abs |> Array.max
                lum, inputByte, expected, measuredMean, maxAbs, Array.averageBy abs diffs)

        let maxDiff = rows |> Array.map (fun (_, _, _, _, d, _) -> d) |> Array.max
        let meanDiff = rows |> Array.averageBy (fun (_, _, _, _, _, d) -> d)

        {
            name = name
            passed = maxDiff <= expectedTolerance
            maxDiff = maxDiff
            meanDiff = meanDiff
            details =
                [
                    yield "lum   input  expected  measured  max|diff|"
                    for (lum, inputByte, expected, measured, maxAbs, _) in rows do
                        yield sprintf "%.1f   %5d  %8s  %8s  %9s"
                                lum inputByte (fmt expected) (fmt measured) (fmt maxAbs)
                ]
        }

    let private checkMonotonic (name : string) (input : PixImage<byte>) (output : PixImage<byte>) =
        let inp = input.GetMatrix<C4b>()
        let m = output.GetMatrix<C4b>()
        let w = int m.SX
        let h = int m.SY

        let mutable maxDrop = 0
        let mutable dropCount = 0
        let mutable firstDrop = None
        let mutable sumDiff = 0.0
        let mutable maxDiff = 0

        for y in 0 .. h - 1 do
            for x in 0 .. w - 1 do
                let c = m.[x, y]
                let i = inp.[x, y]
                for (o, v) in [ c.R, i.R; c.G, i.G; c.B, i.B ] do
                    let d = abs (int o - int v)
                    maxDiff <- max maxDiff d
                    sumDiff <- sumDiff + float d

                if x > 0 then
                    let p = m.[x - 1, y]
                    for (prev, cur) in [ p.R, c.R; p.G, c.G; p.B, c.B ] do
                        let drop = int prev - int cur
                        if drop > 0 then
                            dropCount <- dropCount + 1
                            if firstDrop.IsNone then firstDrop <- Some (x, y, prev, cur)
                            maxDrop <- max maxDrop drop

        {
            name = name
            passed = dropCount = 0
            maxDiff = float maxDiff
            meanDiff = sumDiff / float (w * h * 3)
            details =
                [
                    yield sprintf "decreasing steps: %d, largest drop: %d" dropCount maxDrop
                    match firstDrop with
                    | Some (x, y, prev, cur) -> yield sprintf "first drop at x=%d y=%d: %d -> %d" x y prev cur
                    | None -> ()
                    let row = gradientWidth / 8
                    yield
                        "output along the row (every 1/8): " +
                        String.Join(" ", [ for x in 0 .. row .. w - 1 -> string m.[x, 0].R ] @ [ string m.[w - 1, 0].R ])
                ]
        }

    // black (0) and white (1) are fixed points of applyShadowsHighlights, so the output must
    // equal the input; any difference is bleeding across the edge (sampling, offsets, wiring)
    let private checkEqualsInput (name : string) (input : PixImage<byte>) (output : PixImage<byte>) =
        let inp = input.GetMatrix<C4b>()
        let m = output.GetMatrix<C4b>()
        let w = int m.SX
        let h = int m.SY

        let mutable maxDiff = 0
        let mutable sumDiff = 0.0
        let mutable worstX = -1

        for y in 0 .. h - 1 do
            for x in 0 .. w - 1 do
                let c = m.[x, y]
                let i = inp.[x, y]
                for (o, v) in [ c.R, i.R; c.G, i.G; c.B, i.B ] do
                    let d = abs (int o - int v)
                    if d > maxDiff then
                        maxDiff <- d
                        worstX <- x
                    sumDiff <- sumDiff + float d

        {
            name = name
            passed = maxDiff <= 1
            maxDiff = float maxDiff
            meanDiff = sumDiff / float (w * h * 3)
            details =
                [
                    if maxDiff > 0 then
                        yield sprintf "largest difference at x=%d (edge between x=255 and x=256)" worstX
                    yield
                        "output around the edge (x=250..261): " +
                        String.Join(" ", [ for x in 250 .. 261 -> string m.[x, h / 2].R ])
                ]
        }

    // ---- runner ----------------------------------------------------------------------

    let private config imagePath highlightAmount shadowAmount radius : TestConfig =
        {
            imagePath = imagePath
            highlightAmount = highlightAmount
            highlightTone = 0.5
            highlightRadius = radius
            shadowAmount = shadowAmount
            shadowTone = 0.5
            shadowRadius = radius
            outputPath = None
        }

    // prints the report and returns the exit code: 0 = all passed, 1 = at least one failed
    let run (runtime : IRuntime) (outputDir : string) : int =
        Directory.CreateDirectory outputDir |> ignore

        let save (image : PixImage<byte>) (fileName : string) =
            let path = Path.Combine(outputDir, fileName)
            image.SaveAsPng path
            path

        let patches = createPatchImage ()
        let gradient = createGradientImage ()
        let edge = createEdgeImage ()

        let patchesPath = save patches "input-patches.png"
        let gradientPath = save gradient "input-gradient.png"
        let edgePath = save edge "input-edge.png"

        // (name, config, check)
        let cases =
            [
                "patches-shadows-only",
                config patchesPath 0.0 0.8 30.0,
                checkPatches

                "patches-highlights-only",
                config patchesPath 0.8 0.0 30.0,
                checkPatches

                "patches-combined",
                config patchesPath 0.4 0.8 30.0,
                checkPatches

                "gradient-radius-0",
                config gradientPath 0.8 0.8 0.0,
                fun name _ output -> checkMonotonic name gradient output

                "gradient-radius-30",
                config gradientPath 0.8 0.8 30.0,
                fun name _ output -> checkMonotonic name gradient output

                "edge-radius-0",
                config edgePath 0.8 0.8 0.0,
                fun name _ output -> checkEqualsInput name edge output

                "edge-radius-60",
                config edgePath 0.8 0.8 60.0,
                fun name _ output -> checkEqualsInput name edge output
            ]

        let results =
            cases
            |> List.map (fun (name, cfg, check) ->
                let output = Image.renderAdjustedImage runtime cfg
                save output (name + ".png") |> ignore
                check name cfg output)

        printfn ""
        printfn "Synthetic shadows/highlights tests (values 0-255, output in %s)" outputDir

        for r in results do
            printfn ""
            printfn "%s  %s  (max |diff| %s, mean |diff| %s)"
                (if r.passed then "PASS" else "FAIL") r.name (fmt r.maxDiff) (fmt r.meanDiff)
            for d in r.details do
                printfn "    %s" d

        let failed = results |> List.filter (fun r -> not r.passed)
        printfn ""
        printfn "%d of %d passed" (results.Length - failed.Length) results.Length

        if failed.IsEmpty then 0 else 1
