namespace PRo3D.ImageMapping.Model

open Aardvark.UI.Primitives
open Adaptify

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


type Channel = 
    {
        idx : int
        name : Option<string>
    }

[<ModelType>]
type Image =
    {
        cameraState     : OrbitState
        colorMap        : ColorMap
        useFalseColor   : bool
        channel         : Channel
        channelOptions  : list<Channel>
        dataType        : DataType
        defaultMinValue : float
        defaultMaxValue : float
        customMinValue : NumericInput
        customMaxValue : NumericInput
        texture : string
    }

[<ModelType>]
type Model =
    {
        images          : IndexList<Image>
        selectedImage   : Option<Index>
        editImages      : Index list
    }

type ImageMessage =
    | OrbitCameraMessage of OrbitMessage
    | SetCustomMin of float
    | SetCustomMax of float
    | ResetCustomMinMax
    | SetTexture of string list
    | SetColorMap of ColorMap
    | ToggleFalseColor
    | SetEXRChannel of Channel
    | SetDataTypeAndRange of DataType * float * float
    | Empty

type Message = 
    | SelectImage of Index
    | EditImage of Index
    | LoadImagesDir of string
    | ImageMessage of Index * ImageMessage
    | SortEntriesByDistance
    | SortEntriesByDate



