namespace PRo3D.ImageMapping.Model

open Aardvark.UI.Primitives
open Adaptify

open PRo3D.InstrumentProjection

open FSharp.Data.Adaptive


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
type RgbComposite =
    {
        redNumeratorBand       : Option<int>
        greenNumeratorBand     : Option<int>
        blueNumeratorBand      : Option<int>
        redDenominatorBand     : Option<int>
        greenDenominatorBand   : Option<int>
        blueDenominatorBand    : Option<int>
        gamma                  : NumericInput
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
type BoresightAdjustment =
    {
        roll : NumericInput
        pitch : NumericInput
        yaw : NumericInput

    }

[<ModelType>]
type Model =
    {
        images          : IndexList<Image>
        selectedImage   : Option<Index>
        sourceImagePath   : Option<string>
        editImages      : Index list
        projectionOpacity : NumericInput
        boresightAdjustment : BoresightAdjustment
        cameraState     : OrbitState
        rgbComposite    : RgbComposite
        highlightAdjustment : HighlightAdjustment
        shadowAdjustment    : ShadowAdjustment
        midtoneContrastAdjustment : MidtoneContrastAdjustment
        blackWhiteClip  : BlackWhiteClip
        saturation   : Saturation
    }

module HighlightAdjustment =
    let init : HighlightAdjustment =
        {
            amount = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.0 }
            tone = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.5 }
            radius = { Numeric.init with min = 0.0; max = 100.0; step = 1.0; value = 50.0 }
        }

module ShadowAdjustment =
    let init : ShadowAdjustment=
        {
            amount = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.0 }
            tone = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.5 }
            radius = { Numeric.init with min = 0.0; max = 100.0; step = 1.0; value = 50.0 }
        }

module MidtoneContrastAdjustment =
    let init : MidtoneContrastAdjustment=
        {
            gainFactor = { Numeric.init with min = -1.0; max = 1.0; step = 0.01; value = 0.0 }
        }

module BlackWhiteClip =
    let init : BlackWhiteClip =
        {
            blackClipPercentile = { Numeric.init with min = 0.0; max = 100.0; step = 0.01; value = 2.0 }
            whiteClipPercentile = { Numeric.init with min = 0.0; max = 100.0; step = 0.01; value = 2.0 }
        }

module Saturation =
    let init : Saturation =
        {
            gainFactor = { Numeric.init with min = -1.0; max = 1.0; step = 0.01; value = 0.0 }
        }

module RgbComposite = 
    let empty = 
        {
            redNumeratorBand = None
            greenNumeratorBand = None
            blueNumeratorBand = None
            redDenominatorBand = None
            greenDenominatorBand = None
            blueDenominatorBand = None
            gamma = { Numeric.init with min = 0.01; max = 5.0; step = 0.01; value = 1.0 }
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
                preferredBand 10 1 bandCount
            redDenominatorBand =
                preferredBand 7 0 bandCount

            greenNumeratorBand =
                preferredBand 6 1 bandCount
            greenDenominatorBand =
                preferredBand 3 0 bandCount

            blueNumeratorBand =
                preferredBand 3 1 bandCount
            blueDenominatorBand =
                preferredBand 0 0 bandCount

            gamma = settings.gamma
        }

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
    | SetRgbBand of RgbChannel * RgbBandRole * Index
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
    | Nop