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
type RgbComposite =
    {
        redNumeratorBand     : Option<int>
        greenNumeratorBand   : Option<int>
        blueNumeratorBand    : Option<int>
        redDenominatorBand     : Option<int>
        greenDenominatorBand   : Option<int>
        blueDenominatorBand    : Option<int>
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
        let denominator = firstBand bandCount

        {
            // These defaults avoid R = band0 / band0 when possible.
            // The user can overwrite all six choices in the UI.
            redNumeratorBand =
                preferredBand 1 0 bandCount
            redDenominatorBand =
                denominator

            greenNumeratorBand =
                preferredBand 2 1 bandCount
            greenDenominatorBand =
                denominator

            blueNumeratorBand =
                preferredBand 3 2 bandCount
            blueDenominatorBand =
                denominator
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
        distance: float
        time: System.DateTime
    }

[<ModelType>]
type BoresightAdjustment =
    {
        roll : NumericInput
        pitch : NumericInput
        yaw : NumericInput

    }

module BoresightAdjustment =
    let identity =
        {
            roll = { Numeric.init with min = -180.0; max = 180.0; value = 0.0 }
            pitch = { Numeric.init with min = -180.0; max = 180.0; value = 0.0 }
            yaw = { Numeric.init with min = -180.0; max = 180.0; value = 0.0 }
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
    | Nop