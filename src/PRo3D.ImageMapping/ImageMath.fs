namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO
open System.Runtime.InteropServices

open HDF.PInvoke

open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE

open System.Text.Json
open System.Collections.Concurrent

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

    // important, otherwise black
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
            // todo make this interactive
            let gammaCorrected = 
                Math.Pow(normalized, safeGamma) // gamma < 1 -> brightens; gamma > 1 -> darkens

            // produces one byte of each of RGB
            gammaCorrected * 255.0
            |> Math.Round
            |> byte

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

