module SplineTests

open Expecto
open PRo3D.ImageMapping.Model

let splineTests =
    testList "transfer spline" [
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
    ]