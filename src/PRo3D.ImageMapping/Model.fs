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

[<ModelType>]
type RgbComposite =
    {
        redBand     : Option<int>
        greenBand   : Option<int>
        blueBand    : Option<int>
    }

module RgbComposite = 
    let empty = 
        {
            redBand = None
            greenBand = None
            blueBand = None
        }

    let fromBandCount bandCount =
        {
            redBand =
                if bandCount > 0 then Some 0 else None

            greenBand =
                if bandCount > 1 then Some 1
                elif bandCount > 0 then Some 0
                else None

            blueBand =
                if bandCount > 2 then Some 2
                elif bandCount > 0 then Some 1
                else None
        }

    let set channel bandIndex composite =
        match channel with
        | RgbChannel.Red ->
            { composite with redBand = Some bandIndex }
        | RgbChannel.Green ->
            { composite with greenBand = Some bandIndex }
        | RgbChannel.Blue ->
            { composite with blueBand = Some bandIndex}


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
    | SetRgbBand of RgbChannel * Index
    | Nop