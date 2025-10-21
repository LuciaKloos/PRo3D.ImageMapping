namespace PRo3D.ImageMapping.Model

open Aardvark.UI.Primitives
open Adaptify

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

module ColorMap =
    let getColorMapFileName (map: ColorMap) =
        match map with
        | ColorMap.Magma -> "magma.png"
        | ColorMap.Plasma -> "plasma.png"
        | ColorMap.TwilightShifted -> "twilight_shifted.png"
        | ColorMap.Viridis -> "viridis.png"
        | ColorMap.PiYG -> "PiYG.png"
        | ColorMap.Vanimo -> "vanimo.png"
        | _ -> "magma.png"

[<ModelType>]
type Model =
    {
        cameraState     : CameraControllerState
        colorMap        : ColorMap
        useFalseColor   : bool
        channelName     : string
        channelOptions  : List<string>
        dataType        : DataType
        defaultMinValue : float
        defaultMaxValue : float
        customMinValue : NumericInput
        customMaxValue : NumericInput
        texture : string
    }