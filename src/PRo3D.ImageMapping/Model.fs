namespace PRo3D.ImageMapping.Model

open Aardvark.UI.Primitives
open Adaptify

open PRo3D.InstrumentProjection

open FSharp.Data.Adaptive
open Aardvark.Base
open PRo3D.ImageMapping

type ColorMap =
    | Magma = 0
    | Plasma = 1
    | TwilightShifted = 2
    | Viridis = 3
    | PiYG = 4
    | Vanimo = 5 

type DataType =
    | UInt32 = 0
    | UInt16 = 1
    | Float = 2

type RgbChannel =
    | Red
    | Green
    | Blue

type RgbBandRole =
    | Numerator
    | Denominator

[<ModelType>]
type Image =
    {
        colorMap        : ColorMap
        useFalseColor   : bool
        selectedChannel : Channel
        channelOptions  : list<Channel>
        dataType        : DataType
        defaultMinValues : list<float>
        defaultMaxValues : list<float>
        inputMinValue : NumericInput
        inputMaxValue : NumericInput
        texture : string
        bandIndex : int
        wavelength : Option<float>
        distance: float
        time: System.DateTime
    }

[<ModelType>]
type BandExpressionInput = 
    {
        text : string
        error : Option<string>
        info : Option<string> 
        parsed : Option<BandExpression.BandExpression>  // last valid expression
    }

[<ModelType>]
type RgbRatioComposite =
    {
        redNumeratorBand       : Option<int>
        greenNumeratorBand     : Option<int>
        blueNumeratorBand      : Option<int>
        redDenominatorBand     : Option<int>
        greenDenominatorBand   : Option<int>
        blueDenominatorBand    : Option<int>
        gamma                  : NumericInput
        redExpression          : BandExpressionInput
        greenExpression        : BandExpressionInput
        blueExpression         : BandExpressionInput
    }

[<ModelType>]
type TransferFunctionMapping =
    {
        wavelengthMin        : NumericInput
        wavelengthMax        : NumericInput
        availableSpan        : Option<float * float>
        selectedBands        : list<int>      // logical band indices inside the interval
        usedNearestFallback  : bool
        gamma                : NumericInput
    }

[<ModelType>]
type HighlightAdjustment = 
    { 
        amount  :   NumericInput
        tone    :   NumericInput
        radius  :   NumericInput
    }

[<ModelType>]
type ShadowAdjustment = 
    { 
        amount  :   NumericInput
        tone    :   NumericInput
        radius  :   NumericInput
    }

[<ModelType>]
type MidtoneContrastAdjustment =
    {
        gainFactor  : NumericInput
    }

[<ModelType>]
type BlackWhiteClip =
    {
        blackClipPercentile : NumericInput
        whiteClipPercentile : NumericInput
    }

[<ModelType>]
type Saturation =
    {
        gainFactor  : NumericInput
    }

[<ModelType>]
type Brightness =
    {
        gainFactor  : NumericInput
    }

[<ModelType>]
type Luminance =
    {
        red : float
        green : float
        blue : float
    }

[<ModelType>]
type BoresightAdjustment =
    {
        roll : NumericInput
        pitch : NumericInput
        yaw : NumericInput

    }
    
[<ModelType>]
type MinimumObjectSignal =
    {
        signal : float
    }

[<ModelType>]
type Midtone =
    {
        low : float
        mid : float
        high : float
    }

[<ModelType>]
type Gamma =
    {
        exponent : float
    }

type VisualizationMode =
    | RgbRatioComposite
    | SingleBandTransferFunction    

type ActiveCategory =
    | GreyscaleImage
    | RgbImage
    | MultispectralImage

type SourceImageKind =
    | Multispectral
    | PlainRgbImage

/// px/py normalized to [0,1]; slope is the tangent dy/dx at the point.
type SplinePoint =
    {
        px  : float
        py  : float 
        slope   : float
    }

/// Piecewise cubic Hermite spline y = f(x) on [1,0], points sorted by px.
type TransferSpline = 
    {
        points : list<SplinePoint>
    }


[<ModelType>]
type Model =
    {
        images          : IndexList<Image>
        selectedImage   : Option<Index>
        sourceImagePath   : Option<string>
        sourceImageKind : SourceImageKind
        editImages      : Index list
        projectionOpacity : NumericInput
        boresightAdjustment : BoresightAdjustment
        cameraState     : OrbitState
        rgbRatioComposite    : RgbRatioComposite
        transferFunctionMapping : TransferFunctionMapping
        highlightAdjustment : HighlightAdjustment
        shadowAdjustment    : ShadowAdjustment
        midtoneContrastAdjustment : MidtoneContrastAdjustment
        blackWhiteClip  : BlackWhiteClip
        saturation   : Saturation
        brightness   : Brightness
        visualizationMode : VisualizationMode
        activeCategory : ActiveCategory
        loadCompleteSpectralProfile : bool
        pixelDetectionEnabled : bool
        pixelDetected : bool
        imageWidth : int
        imageHeight : int
        clickedPixel : Option<V2i>
        viewportSize : V2i
        greyscaleColorMap : ColorMap
        greyscaleBlackPoint : NumericInput
        greyscaleWhitePoint : NumericInput
        applyGreyscaleTransferFunction : bool
        savedBandExpressions : list<string>
        bandExpressionDraft : string
        bandNames : list<int * string>  
        renamingBand : Option<int>
        analysisOpen : bool
        transferSpline : TransferSpline
    }

module TransferSpline =
    let minGap = 0.005
    let maxSlope = 20.0

    let private clamp01 (v : float) = max 0.0 (min 1.0 v)

    let init : TransferSpline =
        {
            points = 
                [
                    { px = 0.0; py = 0.0; slope = 1.0 }
                    { px = 1.0; py = 1.0; slope = 1.0 }
                ]
        }

    /// takes an x value in [0,1] and returns the curve’s y value at that x, 
    /// using cubic Hermite interpolation between the control points.
    let evaluate (spline : TransferSpline) (x : float) : float =
        let pts = List.toArray spline.points
        let n = pts.Length

        if n = 0 then clamp01 x
        elif x <= pts.[0].px then clamp01 pts.[0].py // Left of the first point, it returns the first point’s y, so the curve is flat there.
        elif x >= pts.[n - 1].px then clamp01 pts.[n - 1].py // Right of the last point, it returns the last point’s y.
        else 
            let i = (pts |> Array.findIndex (fun p -> p.px > x)) - 1
            let p0 = pts.[i] 
            let p1 = pts.[i + 1]
            let h = p1.px - p0.px
            let t = (x - p0.px) / h // maps x onto 0..1 within the segment
            let t2 = t * t
            let t3 = t2 * t

            // Hermite basis functions
            let h00 =  2.0 * t3 - 3.0 * t2 + 1.0
            let h10 =        t3 - 2.0 * t2 + t
            let h01 = -2.0 * t3 + 3.0 * t2
            let h11 =        t3 -       t2

            clamp01 (h00 * p0.py + h10 * h * p0.slope + h01 * p1.py + h11 * h * p1.slope)

    let movePoint (index : int) (x : float) (y : float) (spline : TransferSpline) =
        let pts = List.toArray spline.points
        let n = pts.Length

        if index < 0 || index >= n then spline
        else   
            let lo = if index = 0   then 0.0 else pts.[index - 1].px + minGap
            let hi = if index = n - 1 then 1.0 else pts.[index + 1].px - minGap
            pts.[index] <- { pts.[index] with px = max lo (min hi x); py = clamp01 y }
            { spline with points = List.ofArray pts }

    let setTangent (index : int) (slope : float) (spline : TransferSpline) =
        let slope = max -maxSlope (min maxSlope slope)

        { spline with
            points = 
                spline.points
                |> List.mapi (fun i p -> if i = index then { p with slope = slope} else p) }

    let resetTangent (index : int) (spline : TransferSpline) =
        setTangent index 0.0 spline

    let addPoint (x : float) (y : float) (spline : TransferSpline) =
        let x = clamp01 x
        let tooClose = spline.points |> List.exists (fun p -> abs (p.px - x) < minGap)

        if tooClose then spline 
        else    
            let e = 0.01
            let slope = (evaluate spline (x + e) - evaluate spline (x - e))

            let newPoint = 
                { px = x; py = clamp01 y; slope = max -maxSlope (min maxSlope slope) }

            { spline with
                points = (newPoint :: spline.points) |> List.sortBy (fun p -> p.px) }

    let removePoint (index : int) (spline : TransferSpline) =
        if List.length spline.points <= 2 then spline
        else    
            { spline with
                points =
                    spline.points
                    |> List.indexed 
                    |> List.filter (fun (i, _) -> i <> index)
                    |> List.map snd         
            }


module HighlightAdjustment =
    let init : HighlightAdjustment =
        {
            amount = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.0 }
            tone = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.5 }
            radius = { Numeric.init with min = 0.0; max = 100.0; step = 1.0; value = 30.0 }
        }

module ShadowAdjustment =
    let init : ShadowAdjustment=
        {
            amount = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.0 }
            tone = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.5 }
            radius = { Numeric.init with min = 0.0; max = 100.0; step = 1.0; value = 30.0 }
        }

module MidtoneContrastAdjustment =
    let init : MidtoneContrastAdjustment=
        {
            gainFactor = { Numeric.init with min = -1.0; max = 1.0; step = 0.01; value = 0.0 }
        }

module BlackWhiteClip =
    let init : BlackWhiteClip =
        {
            blackClipPercentile = { Numeric.init with min = 0.0; max = 100.0; step = 0.01; value = 0.0 }
            whiteClipPercentile = { Numeric.init with min = 0.0; max = 100.0; step = 0.01; value = 0.0 }
        }

module Saturation =
    let init : Saturation =
        {
            gainFactor = { Numeric.init with min = -1.0; max = 1.0; step = 0.01; value = 0.0 }
        }

module Brightness = 
    let init : Brightness =
        {
            gainFactor = { Numeric.init with min = -1.0; max = 1.0; step = 0.01; value = 0.0 }
        }

module Luminance =
    let init : Luminance =
        {
            red = 0.2126
            green = 0.7152
            blue = 0.0722
        }

module Midtone =
    let init : Midtone =
        {
            low = (84.0/255.0)
            mid = ((84.0/255.0) + (168.0/255.0)) * 0.5
            high = (168.0/255.0)
        }

module Gamma =
    let init : Gamma =
        {
            exponent = 1.8
        }

module MinimumObjectSignal =
    let init : MinimumObjectSignal =
        {
            signal = 0.002
        }

module RgbRatioComposite = 
    let empty = 
        {
            redNumeratorBand = None
            greenNumeratorBand = None
            blueNumeratorBand = None
            redDenominatorBand = None
            greenDenominatorBand = None
            blueDenominatorBand = None
            gamma = { Numeric.init with min = 0.01; max = 5.0; step = 0.01; value = 1.0 }

            redExpression = { text = ""; error = None; parsed = None; info = None }
            greenExpression = { text = ""; error = None; parsed = None; info = None }
            blueExpression = { text = ""; error = None; parsed = None; info = None }
        }

    let private firstBand bandCount =
        if bandCount > 0 then Some 0 else None

    let private preferredBand preferredIndex fallbackIndex bandCount =
        if bandCount > preferredIndex then
            Some preferredIndex
        elif bandCount > fallbackIndex then
            Some fallbackIndex
        else
            firstBand bandCount

    let fromBandCount bandCount =
        let settings = empty           

        {
            // These defaults avoid R = band0 / band0 when possible.
            // The user can overwrite all six choices in the UI.
            redNumeratorBand =
                preferredBand 34 1 bandCount
            redDenominatorBand =
                preferredBand 30 0 bandCount

            greenNumeratorBand =
                preferredBand 21 1 bandCount
            greenDenominatorBand =
                preferredBand 17 0 bandCount

            blueNumeratorBand =
                preferredBand 10 1 bandCount
            blueDenominatorBand =
                preferredBand 5 0 bandCount

            gamma = settings.gamma

            redExpression = settings.redExpression
            greenExpression = settings.greenExpression
            blueExpression = settings.blueExpression
        }

    let expressionText (label : int -> string) (numerator : Option<int>) (denominator : Option<int>) =
        match numerator, denominator with
        | Some n, Some d -> sprintf "%s/%s" (label n) (label d)
        | Some n, None   -> label n
        | None, _        -> ""

    let expressionOf (numerator : Option<int>) (denominator : Option<int>) =
        match numerator, denominator with
        | Some n, Some d -> Some (BandExpression.Div (BandExpression.Band (n + 1), BandExpression.Band (d + 1)))
        | Some n, None   -> Some (BandExpression.Band (n + 1))
        | None, _        -> None

    let syncExpression (label : int -> string) (channel : RgbChannel) (composite : RgbRatioComposite) =
        let input numerator denominator : BandExpressionInput =
            { text = expressionText label numerator denominator
              error = None; parsed = expressionOf numerator denominator; info = None }
        match channel with
        | RgbChannel.Red ->
            { composite with redExpression = input composite.redNumeratorBand composite.redDenominatorBand }
        | RgbChannel.Green ->
            { composite with greenExpression = input composite.greenNumeratorBand composite.greenDenominatorBand }
        | RgbChannel.Blue ->
            { composite with blueExpression = input composite.blueNumeratorBand composite.blueDenominatorBand }

    let syncAllExpressions label (composite : RgbRatioComposite) =
        composite
        |> syncExpression label RgbChannel.Red
        |> syncExpression label RgbChannel.Green
        |> syncExpression label RgbChannel.Blue

    /// rewrite the text of all valid expressions after a rename/reset
    let relabelExpressions (label : int -> string) (c : RgbRatioComposite) =
        let relabel (input : BandExpressionInput) =
            match input.parsed with
            | Some e -> { input with text = BandExpression.printTop label e; error = None }
            | None   -> input
        { c with
            redExpression   = relabel c.redExpression
            greenExpression = relabel c.greenExpression
            blueExpression  = relabel c.blueExpression }

    let set channel role bandIndex composite =

        match channel, role with
        | RgbChannel.Red, RgbBandRole.Numerator ->
            { composite with redNumeratorBand = Some bandIndex }

        | RgbChannel.Red, RgbBandRole.Denominator ->
            { composite with redDenominatorBand = Some bandIndex }

        | RgbChannel.Green, RgbBandRole.Numerator ->
            { composite with greenNumeratorBand = Some bandIndex }

        | RgbChannel.Green, RgbBandRole.Denominator ->
            { composite with greenDenominatorBand = Some bandIndex }

        | RgbChannel.Blue, RgbBandRole.Numerator ->
            { composite with blueNumeratorBand = Some bandIndex }

        | RgbChannel.Blue, RgbBandRole.Denominator ->
            { composite with blueDenominatorBand = Some bandIndex }

module TransferFunctionMapping =
    let private wavelengthInput lo hi v =
        { Numeric.init with min = lo; max = hi; step = 1.0; value = v; format = "{0:0.0}" }

    let empty : TransferFunctionMapping =
        {
            wavelengthMin = wavelengthInput 0.0 0.0 0.0
            wavelengthMax = wavelengthInput 0.0 0.0 0.0
            availableSpan = None
            selectedBands = []
            usedNearestFallback = false
            gamma = { Numeric.init with min = 0.01; max = 5.0; step = 0.01; value = 1.0 }
        }

    let private finiteWavelength (b : Image) =
        b.wavelength |> Option.filter System.Double.IsFinite

    let spanOf (bands : list<Image>) =
        match bands |> List.choose finiteWavelength with
        | [] -> None
        | ws -> Some (List.min ws, List.max ws)

    /// bands with lo <= wavelength <= hi; if none, the band nearest the interval center
    let selectBands (lo : float) (hi : float) (bands : list<Image>) : list<int> * bool =
        let withW =
            bands |> List.choose (fun b -> finiteWavelength b |> Option.map (fun w -> b.bandIndex, w))
        match withW with
        | [] ->
            // no wavelength metadata at all: fall back to the first band
            (match bands with b :: _ -> [ b.bandIndex ], true | [] -> [], false)
        | _ ->
            match withW |> List.filter (fun (_, w) -> w >= lo && w <= hi) with
            | [] ->
                let c = 0.5 * (lo + hi)
                [ withW |> List.minBy (fun (_, w) -> abs (w - c)) |> fst ], true
            | inside ->
                (inside |> List.map fst |> List.sort), false

    let recompute (bands : list<Image>) (m : TransferFunctionMapping) =
        let sel, fb = selectBands m.wavelengthMin.value m.wavelengthMax.value bands
        Log.warn "recompute [%f, %f] -> %A (fallback=%b)" m.wavelengthMin.value m.wavelengthMax.value sel fb
        { m with selectedBands = sel; usedNearestFallback = fb }

    /// default interval = wavelength of the first band (same behaviour as today's single-band default)
    let initFor (bands : list<Image>) (m : TransferFunctionMapping) =
        match spanOf bands with
        | None -> recompute bands { m with availableSpan = None }
        | Some (lo, hi) ->
            let start =
                bands |> List.tryPick finiteWavelength |> Option.defaultValue lo
            { m with
                availableSpan = Some (lo, hi)
                wavelengthMin = wavelengthInput lo hi start
                wavelengthMax = wavelengthInput lo hi start }
            |> recompute bands

module ColorMap =
    let getColorMapFileName (map: ColorMap) =
        match map with
        | ColorMap.Magma -> "magma.png"
        | ColorMap.Plasma -> "plasma.png"
        | ColorMap.TwilightShifted -> "twilight_shifted.png"
        | ColorMap.Viridis -> "viridis.png"
        | ColorMap.PiYG -> "piyg.png"
        | ColorMap.Vanimo -> "vanimo.png"
        | _ -> "magma.png"

module BoresightAdjustment =
    let identity =
        {
            roll = { Numeric.init with min = -180.0; max = 180.0; value = 0.0 }
            pitch = { Numeric.init with min = -180.0; max = 180.0; value = 0.0 }
            yaw = { Numeric.init with min = -180.0; max = 180.0; value = 0.0 }
        }

type ImageMessage =
    | SetCustomMin of float
    | SetCustomMax of float
    | ResetCustomMinMax
    | SetColorMap of ColorMap
    | ToggleFalseColor
    | SetEXRChannel of Channel
    | SetDataTypeAndRange of DataType * float * float
    | Empty


type Message = 
    | OrbitCameraMessage of OrbitMessage
    | SelectImage of Index
    | EditImage of Index
    | LoadMultispectralImage of string
    | ImageMessage of Index * ImageMessage
    | SortEntriesByDistance
    | SortEntriesByDate
    | SetProjectionOpacity of Numeric.Action
    | SetRoll of Numeric.Action
    | SetYaw of Numeric.Action
    | SetPitch of Numeric.Action
    | SetVisualizationMode of VisualizationMode
    | SetActiveCategory of ActiveCategory
    | SetBandRatioBand of RgbChannel * RgbBandRole * Index
    | SetRgbGamma of Numeric.Action
    | SetAmountHighlight of Numeric.Action
    | SetToneHighlight of Numeric.Action
    | SetRadiusHighlight of Numeric.Action
    | SetAmountShadow of Numeric.Action
    | SetToneShadow of Numeric.Action
    | SetRadiusShadow of Numeric.Action
    | SetMidtoneContrastGainFactor of Numeric.Action
    | SetBlackClipPercentile of Numeric.Action
    | SetWhiteClipPercentile of Numeric.Action
    | SetSaturationGainFactor of Numeric.Action
    | SetBrightnessGainFactor of Numeric.Action
    | ResetHighlights
    | ResetShadows
    | ResetAdjustments
    | ToggleCompleteSpectralProfile
    | TogglePixelDetection
    | SetPixelDetected of bool
    | ImageClicked of position : V3d * viewportSize : V2i
    | SetGreyscaleColorMap of ColorMap
    | SetGreyscaleBlackPoint of Numeric.Action
    | SetGreyscaleWhitePoint of Numeric.Action
    | ToggleGreyscaleTransferFunction
    | SetBandRatioExpression of RgbChannel * string
    | SetBandExpressionDraft of string
    | AddSavedBandExpression
    | RemoveSavedBandExpression of int
    | StartRenameBand of int
    | CancelRenameBand
    | RenameBand of int * string
    | ResetBandName of int
    | SetWavelengthMin of Numeric.Action
    | SetWavelengthMax of Numeric.Action
    | SetTransferFunctionColorMap of ColorMap
    | ToggleAnalysisOpen
    | SplineMovePoint of int * float * float
    | SplineSetTangent of int * float
    | SplineResetTangent of int
    | SplineAddPoint of float * float
    | SplineRemovePoint of int
    | ResetSpline
    | Nop