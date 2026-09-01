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
type RgbRatioComposite =
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
type BandMapping =
    {
        redBand : Option<int>
        greenBand : Option<int>
        blueBand : Option<int>
        gamma : NumericInput
    }

[<ModelType>]
type TransferFunctionMapping =
    {
        selectedBand : Option<int>
        gamma : NumericInput
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
        highlights : float
        shadows : float
    }

type VisualizationMode =
    | RgbComposite
    | RgbRatioComposite
    | SingleBandTransferFunction    

type SourceImageKind =
    | Multispectral
    | PlainRgbImage

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
        bandMapping : BandMapping
        transferFunctionMapping : TransferFunctionMapping
        highlightAdjustment : HighlightAdjustment
        shadowAdjustment    : ShadowAdjustment
        midtoneContrastAdjustment : MidtoneContrastAdjustment
        blackWhiteClip  : BlackWhiteClip
        saturation   : Saturation
        brightness   : Brightness
        visualizationMode : VisualizationMode
        loadCompleteSpectralProfile : bool
    }

module HighlightAdjustment =
    let init : HighlightAdjustment =
        {
            amount = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.80 }
            tone = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.70 }
            radius = { Numeric.init with min = 0.0; max = 100.0; step = 1.0; value = 25.0 }
        }

module ShadowAdjustment =
    let init : ShadowAdjustment=
        {
            amount = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.7 }
            tone = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 0.6 }
            radius = { Numeric.init with min = 0.0; max = 100.0; step = 1.0; value = 20.0 }
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
            highlights = 1.3
            shadows = 0.7
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

module BandMapping =
    let empty = 
        {
            redBand = None
            greenBand = None
            blueBand = None
            gamma = { Numeric.init with min = 0.01; max = 5.0; step = 0.01; value = 1.0 }
        }

    let set channel bandIndex mapping =
        match channel with
        | RgbChannel.Red ->
            { mapping with redBand = Some bandIndex }
        | RgbChannel.Green ->
            { mapping with greenBand = Some bandIndex }
        | RgbChannel.Blue ->
            { mapping with blueBand = Some bandIndex }

module TransferFunctionMapping =
    let empty : TransferFunctionMapping =
        {
            selectedBand = None
            gamma = { Numeric.init with min = 0.01; max = 5.0; step = 0.01; value = 1.0 }
        }
    let set bandIndex mapping =
        { mapping with selectedBand = Some bandIndex }

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
    | SetBandRatioBand of RgbChannel * RgbBandRole * Index
    | SetRgbMappingBand of RgbChannel * Index
    | SetTransferFunctionBand of Index
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
    | Nop