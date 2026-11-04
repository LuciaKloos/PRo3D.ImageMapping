module SplineTests

open System
open System.Globalization
open System.Text.Json
open Expecto
open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping
open PRo3D.ImageMapping.Model

// ---------------------------------------------------------------- helpers

let private near (tol : float) (actual : float) (expected : float) (msg : string) =
    Expect.isTrue (abs (actual - expected) <= tol) $"{msg}: expected {expected}, got {actual}"

let private pt px py slope : SplinePoint = { px = px; py = py; slope = slope }
let private mk (pts : list<SplinePoint>) : TransferSpline = { points = pts }

let private evalAt s x = TransferSpline.evaluate s x

/// A gentle, non-trivial spline whose values stay well inside (0,1) around the interior knots.
let private gentle =
    mk [ pt 0.0 0.1 1.0
         pt 0.4 0.5 1.0
         pt 0.8 0.7 0.5
         pt 1.0 0.9 1.0 ]

/// Structural invariants every edited spline must keep.
let private checkInvariants (ctx : string) (s : TransferSpline) =
    let pts = s.points
    Expect.isTrue (List.length pts >= 2) $"{ctx}: needs at least two points"

    for p in pts do
        Expect.isTrue (Double.IsFinite p.px && Double.IsFinite p.py && Double.IsFinite p.slope) $"{ctx}: non-finite value in {p}"
        Expect.isTrue (p.px >= 0.0 && p.px <= 1.0) $"{ctx}: px out of [0,1]: {p.px}"
        Expect.isTrue (p.py >= 0.0 && p.py <= 1.0) $"{ctx}: py out of [0,1]: {p.py}"
        Expect.isTrue (abs p.slope <= TransferSpline.maxSlope + 1e-12) $"{ctx}: slope out of range: {p.slope}"

    for (a, b) in List.pairwise pts do
        Expect.isTrue (b.px - a.px >= TransferSpline.minGap - 1e-9) $"{ctx}: points too close or unsorted: {a.px}, {b.px}"

    for i in 0 .. 50 do
        let y = evalAt s (float i / 50.0)
        Expect.isTrue (Double.IsFinite y && y >= 0.0 && y <= 1.0) $"{ctx}: evaluate gave {y}"

let private runRandomEdits (seed : int) (steps : int) =
    let rnd = Random(seed)
    let mutable s = TransferSpline.init

    for step in 1 .. steps do
        let n = List.length s.points
        let idx = rnd.Next(-1, n + 1)               // includes invalid indices on purpose
        let r () = rnd.NextDouble() * 1.4 - 0.2     // includes values outside [0,1]

        s <-
            match rnd.Next 5 with
            | 0 -> TransferSpline.movePoint idx (r ()) (r ()) s
            | 1 -> TransferSpline.setTangent idx ((rnd.NextDouble() - 0.5) * 100.0) s
            | 2 -> TransferSpline.resetTangent idx s
            | 3 -> TransferSpline.addPoint (r ()) (r ()) s
            | _ -> TransferSpline.removePoint idx s

        checkInvariants $"random edit step {step}" s

// ---------------------------------------------------------------- tests

let splineTests =
    testList "transfer spline" [

        // ---- three trivial tests
        test "init spline is identity" {
            for i in 0 .. 100 do
                let x = float i / 100.0
                Expect.floatClose Accuracy.high (TransferSpline.evaluate TransferSpline.init x) x "identity"
        }

        test "passes through control points" {
            let s = TransferSpline.init
            for p in s.points do
                Expect.floatClose Accuracy.high (TransferSpline.evaluate s p.px) p.py "control point"
        }

        test "output stays in [0,1]" {
            let s = TransferSpline.init
            for i in 0 .. 255 do
                let y = TransferSpline.evaluate s (float i / 255.0)
                Expect.isTrue (y >= 0.0 && y <= 1.0) $"out of range at {i}: {y}"
        }

        // ---- evaluate on non-trivial splines
        testList "evaluate" [
            test "endpoints return first and last py" {
                near 1e-12 (evalAt gentle 0.0) 0.1 "x = 0"
                near 1e-12 (evalAt gentle 1.0) 0.9 "x = 1"
            }

            test "outside [first.px, last.px] the curve is flat" {
                near 1e-12 (evalAt gentle -0.5) 0.1 "left of range"
                near 1e-12 (evalAt gentle 1.5) 0.9 "right of range"
            }

            test "passes through every control point of a non-trivial spline" {
                for p in gentle.points do
                    near 1e-9 (evalAt gentle p.px) p.py $"knot at {p.px}"
            }

            test "continuous across knots" {
                for p in gentle.points |> List.filter (fun p -> p.px > 0.0 && p.px < 1.0) do
                    let below = evalAt gentle (p.px - 1e-9)
                    let above = evalAt gentle (p.px + 1e-9)
                    near 1e-6 below p.py $"just below knot {p.px}"
                    near 1e-6 above p.py $"just above knot {p.px}"
            }

            test "derivative at interior knots matches the stored slope" {
                let h = 1e-5
                for p in gentle.points |> List.filter (fun p -> p.px > 0.0 && p.px < 1.0) do
                    let d = (evalAt gentle (p.px + h) - evalAt gentle (p.px - h)) / (2.0 * h)
                    near 1e-3 d p.slope $"slope at {p.px}"
            }

            test "inverted curve (0,1) -> (1,0) is a descending line" {
                let s = mk [ pt 0.0 1.0 -1.0; pt 1.0 0.0 -1.0 ]
                for i in 0 .. 20 do
                    let x = float i / 20.0
                    near 1e-9 (evalAt s x) (1.0 - x) $"inverted at {x}"
            }

            test "smooth S-curve is monotone (Fritsch-Carlson safe slopes)" {
                let s = mk [ pt 0.0 0.0 0.0; pt 0.5 0.5 1.5; pt 1.0 1.0 0.0 ]
                let mutable prev = evalAt s 0.0
                for i in 1 .. 1000 do
                    let y = evalAt s (float i / 1000.0)
                    Expect.isTrue (y >= prev - 1e-12) $"decreased at step {i}: {prev} -> {y}"
                    prev <- y
            }

            test "steep tangents overshoot is clamped to [0,1]" {
                for slope in [ 20.0; -20.0 ] do
                    let s = mk [ pt 0.0 0.0 slope; pt 1.0 1.0 slope ]
                    for i in 0 .. 1000 do
                        let y = evalAt s (float i / 1000.0)
                        Expect.isTrue (y >= 0.0 && y <= 1.0) $"slope {slope}: out of range at {i}: {y}"
            }

            test "empty spline behaves as clamped identity" {
                let s = mk []
                near 1e-12 (evalAt s 0.3) 0.3 "inside"
                near 1e-12 (evalAt s -1.0) 0.0 "below"
                near 1e-12 (evalAt s 2.0) 1.0 "above"
            }

            test "two nearly coincident points stay finite" {
                let s = mk [ pt 0.0 0.0 1.0; pt 0.5 0.2 1.0; pt (0.5 + 1e-12) 0.8 1.0; pt 1.0 1.0 1.0 ]
                for i in 0 .. 1000 do
                    let y = evalAt s (float i / 1000.0)
                    Expect.isTrue (Double.IsFinite y && y >= 0.0 && y <= 1.0) $"bad value {y} at {i}"
            }
        ]

        // ---- edit operations
        testList "movePoint" [
            let three = TransferSpline.addPoint 0.5 0.5 TransferSpline.init

            test "moves a middle point" {
                let s = TransferSpline.movePoint 1 0.6 0.3 three
                near 1e-12 s.points.[1].px 0.6 "px"
                near 1e-12 s.points.[1].py 0.3 "py"
                checkInvariants "move" s
            }

            test "middle point is clamped between its neighbours" {
                let right = TransferSpline.movePoint 1 5.0 0.5 three
                let left = TransferSpline.movePoint 1 -5.0 0.5 three
                near 1e-12 right.points.[1].px (1.0 - TransferSpline.minGap) "right limit"
                near 1e-12 left.points.[1].px (0.0 + TransferSpline.minGap) "left limit"
                checkInvariants "right" right
                checkInvariants "left" left
            }

            test "py is clamped to [0,1]" {
                let s = TransferSpline.movePoint 1 0.5 7.0 three
                near 1e-12 s.points.[1].py 1.0 "upper"
                let s = TransferSpline.movePoint 1 0.5 -7.0 three
                near 1e-12 s.points.[1].py 0.0 "lower"
            }

            test "end points cannot cross their neighbour" {
                let first = TransferSpline.movePoint 0 0.9 0.0 three
                near 1e-12 first.points.[0].px (0.5 - TransferSpline.minGap) "first"
                let last = TransferSpline.movePoint 2 0.0 1.0 three
                near 1e-12 last.points.[2].px (0.5 + TransferSpline.minGap) "last"
                checkInvariants "first" first
                checkInvariants "last" last
            }

            test "moving the first point right makes the curve flat to its left" {
                let s = TransferSpline.movePoint 0 0.2 0.3 TransferSpline.init
                near 1e-12 (evalAt s 0.1) 0.3 "flat region"
                near 1e-12 (evalAt s 0.0) 0.3 "flat region at 0"
            }

            test "invalid index leaves the spline unchanged" {
                Expect.equal (TransferSpline.movePoint -1 0.5 0.5 three) three "negative"
                Expect.equal (TransferSpline.movePoint 99 0.5 0.5 three) three "too large"
            }
        ]

        testList "tangents" [
            test "setTangent changes only the chosen point" {
                let s = TransferSpline.setTangent 1 2.5 gentle
                near 1e-12 s.points.[1].slope 2.5 "changed"
                for i in [ 0; 2; 3 ] do
                    Expect.equal s.points.[i] gentle.points.[i] $"point {i} untouched"
            }

            test "setTangent clamps to +-maxSlope" {
                let hi = TransferSpline.setTangent 1 1000.0 gentle
                let lo = TransferSpline.setTangent 1 -1000.0 gentle
                near 1e-12 hi.points.[1].slope TransferSpline.maxSlope "upper"
                near 1e-12 lo.points.[1].slope (-TransferSpline.maxSlope) "lower"
            }

            test "resetTangent restores slope 1" {
                let s = TransferSpline.setTangent 1 5.0 gentle |> TransferSpline.resetTangent 1
                near 1e-12 s.points.[1].slope 1.0 "reset"
            }

            test "invalid index leaves the spline unchanged" {
                Expect.equal (TransferSpline.setTangent 42 3.0 gentle) gentle "setTangent"
                Expect.equal (TransferSpline.resetTangent -1 gentle) gentle "resetTangent"
            }
        ]

        testList "addPoint / removePoint" [
            test "addPoint inserts in sorted position with the curve's local slope" {
                let s = TransferSpline.addPoint 0.5 0.5 TransferSpline.init
                Expect.equal (List.length s.points) 3 "count"
                near 1e-12 s.points.[1].px 0.5 "px"
                near 1e-12 s.points.[1].py 0.5 "py"
                near 1e-6 s.points.[1].slope 1.0 "slope of identity"
                checkInvariants "add" s
            }

            test "addPoint keeps points sorted when adding out of order" {
                let s =
                    TransferSpline.init
                    |> TransferSpline.addPoint 0.7 0.7
                    |> TransferSpline.addPoint 0.2 0.2
                    |> TransferSpline.addPoint 0.45 0.4
                Expect.equal (s.points |> List.map (fun p -> p.px)) (s.points |> List.map (fun p -> p.px) |> List.sort) "sorted"
                checkInvariants "add x3" s
            }

            test "addPoint closer than minGap to an existing point is ignored" {
                let s = TransferSpline.addPoint (TransferSpline.minGap / 2.0) 0.5 TransferSpline.init
                Expect.equal s TransferSpline.init "unchanged"
            }

            test "addPoint beyond 1 is clamped, and ignored if it would sit on the last point" {
                Expect.equal (TransferSpline.addPoint 1.5 0.5 TransferSpline.init) TransferSpline.init "unchanged"
            }

            test "removePoint removes the chosen point" {
                let s = TransferSpline.removePoint 1 gentle
                Expect.equal (List.length s.points) 3 "count"
                Expect.equal (s.points |> List.map (fun p -> p.px)) [ 0.0; 0.8; 1.0 ] "remaining"
            }

            test "removePoint never goes below two points" {
                Expect.equal (TransferSpline.removePoint 0 TransferSpline.init) TransferSpline.init "two points"
            }

            test "removePoint with invalid index changes nothing" {
                Expect.equal (TransferSpline.removePoint 99 gentle) gentle "out of range"
            }
        ]

        testList "random edits" [
            test "invariants hold over 2000 random edits (seed 1)" { runRandomEdits 1 2000 }
            test "invariants hold over 2000 random edits (seed 2)" { runRandomEdits 2 2000 }
            test "invariants hold over 2000 random edits (seed 3)" { runRandomEdits 3 2000 }
        ]

        // ---- Update.update wiring (messages -> TransferSpline functions)
        testList "Update messages" [
            let m0 = App.initial

            test "SplineAddPoint / SplineMovePoint / SplineSetTangent go through TransferSpline" {
                let m1 = Update.update m0 (SplineAddPoint (0.5, 0.8))
                Expect.equal m1.transferSpline (TransferSpline.addPoint 0.5 0.8 m0.transferSpline) "add"

                let m2 = Update.update m1 (SplineMovePoint (1, 0.6, 0.4))
                Expect.equal m2.transferSpline (TransferSpline.movePoint 1 0.6 0.4 m1.transferSpline) "move"

                let m3 = Update.update m2 (SplineSetTangent (1, 3.0))
                Expect.equal m3.transferSpline (TransferSpline.setTangent 1 3.0 m2.transferSpline) "tangent"

                let m4 = Update.update m3 (SplineResetTangent 1)
                Expect.equal m4.transferSpline (TransferSpline.resetTangent 1 m3.transferSpline) "reset tangent"

                let m5 = Update.update m4 (SplineRemovePoint 1)
                Expect.equal m5.transferSpline (TransferSpline.removePoint 1 m4.transferSpline) "remove"
            }

            test "ResetSpline restores the identity spline" {
                let edited =
                    Update.update m0 (SplineAddPoint (0.5, 0.9))
                    |> fun m -> Update.update m (SplineSetTangent (1, 5.0))
                Expect.notEqual edited.transferSpline TransferSpline.init "edited differs"
                let reset = Update.update edited ResetSpline
                Expect.equal reset.transferSpline TransferSpline.init "back to init"
            }

            test "Nop does not touch the spline" {
                Expect.equal (Update.update m0 Nop).transferSpline m0.transferSpline "unchanged"
            }
        ]

        // ---- serialization used by the JS editor
        testList "SplineEditor.serialize" [
            let parse (spline : TransferSpline) samples =
                let json = SplineEditor.serialize spline samples
                use doc = JsonDocument.Parse json
                let root = doc.RootElement
                let pts =
                    [ for p in root.GetProperty("pts").EnumerateArray() ->
                        [ for v in p.EnumerateArray() -> v.GetDouble() ] ]
                let curve =
                    [ for c in root.GetProperty("curve").EnumerateArray() ->
                        [ for v in c.EnumerateArray() -> v.GetDouble() ] ]
                pts, curve

            test "produces valid JSON with the expected shape" {
                let pts, curve = parse gentle 256
                Expect.equal (List.length pts) 4 "point count"
                Expect.equal (List.length curve) 256 "curve samples"
                Expect.isTrue (pts |> List.forall (fun p -> List.length p = 3)) "[px,py,slope]"
                Expect.isTrue (curve |> List.forall (fun c -> List.length c = 2)) "[x,y]"
            }

            test "points and curve match the model (5-decimal rounding)" {
                let pts, curve = parse gentle 64
                List.zip pts gentle.points
                |> List.iter (fun (j, p) ->
                    near 1e-5 j.[0] p.px "px"
                    near 1e-5 j.[1] p.py "py"
                    near 1e-5 j.[2] p.slope "slope")
                for c in curve do
                    near 1e-5 c.[1] (evalAt gentle c.[0]) $"curve at {c.[0]}"
                near 1e-12 curve.Head.[0] 0.0 "first x"
                near 1e-12 (List.last curve).[0] 1.0 "last x"
            }

            test "is culture independent (decimal comma cultures)" {
                let old = CultureInfo.CurrentCulture
                try
                    CultureInfo.CurrentCulture <- CultureInfo("de-AT")
                    let pts, curve = parse gentle 32
                    Expect.equal (List.length pts) 4 "points under de-AT"
                    Expect.equal (List.length curve) 32 "curve under de-AT"
                    near 1e-5 pts.[1].[0] 0.4 "decimal parsed correctly"
                finally
                    CultureInfo.CurrentCulture <- old
            }
        ]

        // ---- LUT uploaded to the GPU
        testList "LUT texture" [
            test "256-sample LUT equals evaluate at i/255" {
                let s =
                    TransferSpline.init
                    |> TransferSpline.addPoint 0.3 0.6
                    |> TransferSpline.addPoint 0.7 0.4
                let tex =
                    PRo3D.ImageMapping.Image.createSplineTexture (AVal.constant s) 256
                    |> AVal.force
                let pix = (tex :?> PixTexture2d).PixImageMipMap.ImageArray.[0] :?> PixImage<float32>
                let data = pix.Volume.Data
                Expect.equal data.Length 256 "256 samples"
                for i in 0 .. 255 do
                    near 1e-6 (float data.[i]) (evalAt s (float i / 255.0)) $"LUT[{i}]"
            }
        ]
    ]