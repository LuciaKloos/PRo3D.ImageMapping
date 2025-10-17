namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO
open Newtonsoft.Json.Linq

open Aardvark.GeoSpatial.Opc

type Message =
    | CameraMessage of FreeFlyController.Message
    | SetMin of float
    | SetMax of float
    | ResetMinMax
    | SetTexture of string list
    | SetColorMap of ColorMap
    | ToggleFalseColor
    | SetEXRChannel of string
    | Empty


module Shaders = 
    open FShade
    open Aardvark.Rendering.Effects

    let instrumentSampler = 
        sampler2d {
            texture uniform?InstrumentImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let colormapTextureSampler =
        sampler2d {
            texture uniform?ColormapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue
        member x.UseFalseColor : bool = uniform?UseFalseColor

    let hshColors (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X * 65000.0// 0-1 range
            let remappedClampedNormalizedX =
                ((min uniform.MaxValue (max uniform.MinValue hshValueX)) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remapClampNormalize =
                if uniform.UseFalseColor then
                    colormapTextureSampler.Sample(V2d (remappedClampedNormalizedX, 0.0))
                else 
                    V4d(
                        remappedClampedNormalizedX,
                        remappedClampedNormalizedX,
                        remappedClampedNormalizedX,
                        1.0
                    )
            return remapClampNormalize
        }


module App =

    let initialPath = ""

    let getMinMaxFromStatistics (filePath: string, channel: int) =
        if not (File.Exists(filePath)) then
            (0, 0)
        else 
            let j = JObject.Parse(File.ReadAllText(filePath))
            let rec loop (token:JToken) remaining =
                match remaining with
                | [] -> token.Value<int>()
                | k::rest ->
                    match token.Type with
                    | JTokenType.Object ->
                        let obj = token :?> JObject
                        match obj.TryGetValue(k) with
                            | true, t -> loop t rest
                            | _ -> token.Value<int>()
                    | JTokenType.Array ->
                        let obj = token.[channel] :?> JObject
                        match obj.TryGetValue(k) with
                            | true, t -> loop t rest
                            | _ -> token.Value<int>()
                    | _ -> token.Value<int>()

            (loop (j :> JToken) ["image_statistics"; "minimum"], loop (j :> JToken) ["image_statistics"; "maximum"])

    let getEXRChannelOptions (filePath: string) =
        if not ((File.Exists(filePath)) || (Path.GetExtension(filePath).ToLower() != ".exr")) then
            []
        else 
            let j = JObject.Parse(File.ReadAllText(filePath))
            match j.TryGetValue("channels") with
            | true, t -> 
                let channels = [ 0 .. t.Value<int>() - 1 ]
                channels |> List.map string
            | _ -> []
            

    let minValue = {
        value   = 0.0
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }

    let maxValue = {
        value   = 0.0
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }
    
    let initial = { 
        cameraState = FreeFlyController.initial;
        colorMap = ColorMap.Magma;
        useFalseColor = true;
        channelName = "0";
        channelOptions = [];
        defaultMinValue = minValue.value;
        defaultMaxValue = maxValue.value;
        customMinValue = minValue;
        customMaxValue = maxValue;
        texture = initialPath;
    }

    let update (m : Model) (msg : Message) =
        match msg with
            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

            | SetMin v -> 
                { m with customMinValue = {minValue with value = v} }
            | SetMax v -> 
                { m with customMaxValue = {maxValue with value = v} }
            | ResetMinMax ->
                { m with customMinValue = {minValue with value = m.defaultMinValue}; customMaxValue = {maxValue with value = m.defaultMaxValue} }
            | SetTexture (texture) ->
                let channelOptions = getEXRChannelOptions(texture[0] + ".json")
                let channelIdx =
                    if channelOptions.IsEmpty() then
                        0
                    else
                        match System.Int32.TryParse(channelOptions.[0]) with
                        | true, v -> v
                        | false, _ -> 0
                let (min, max) = getMinMaxFromStatistics(texture[0] + ".json", channelIdx)
                { m with 
                    texture = texture[0];
                    defaultMinValue = min;
                    defaultMaxValue = max;
                    customMinValue = {minValue with value = min};
                    customMaxValue = {maxValue with value = max};
                    channelName = string channelIdx;
                    channelOptions = channelOptions;
                }
            | SetColorMap (map : ColorMap) ->
                { m with colorMap = map }
            | SetEXRChannel (channelName: string) ->
                let channelIdx =     
                    match System.Int32.TryParse(channelName) with
                    | true, v -> v
                    | false, _ -> 0
                let (min, max) = getMinMaxFromStatistics(m.texture + ".json", channelIdx)
                { m with 
                    defaultMinValue = min;
                    defaultMaxValue = max;
                    customMinValue = {minValue with value = min};
                    customMaxValue = {maxValue with value = max};
                    channelName = string channelIdx;
                }
            | ToggleFalseColor ->
                { m with useFalseColor = not m.useFalseColor }
            | Empty ->
                m
    let view (m : AdaptiveModel) =

        let colormapTexture : aval<ITexture> =
            m.colorMap
            |> AVal.map (fun map ->
                FileTexture(Path.Combine(Path.Combine(Directory.GetCurrentDirectory(), "resources"), ColorMap.getColorMapFileName(map)), TextureParams.empty)
            )

        let imageTexture : aval<ITexture> =
            AVal.map2 (fun path_ channelName ->
                let path = if File.Exists(path_) then path_ else Path.Combine(Path.Combine(Directory.GetCurrentDirectory(), "resources"), "white_pixel.png")
                match Path.GetExtension(path).ToLower() with
                | ".exr" ->
                    let stream = File.OpenRead path
                    let exrTexture = TextureLoading.loadImageFromStream stream (ChannelReference.ChannelWithName channelName) (Some TextureLoading.TextureFormat.OpenEXR)
                    PixTexture2d(exrTexture, TextureParams.empty)
                | ".png"
                | _ ->
                    FileTexture(
                        path, 
                        TextureParams.empty
                    )
            ) m.texture m.channelName

        let instrumentVisualization = 
            
            Sg.fullScreenQuad
            |> Sg.noEvents
            |> Sg.texture "InstrumentImage" imageTexture
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.uniform "MinValue" (m.customMinValue.value |> AVal.map (fun v -> float v)) // float v / 65535.0
            |> Sg.uniform "MaxValue" (m.customMaxValue.value |> AVal.map (fun v -> float v))
            |> Sg.uniform "UseFalseColor" m.useFalseColor
            |> Sg.shader {
                do! (Shaders.hshColors)
            }


        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        let jsImportDialog = "top.aardvark.dialog.showOpenDialog({tile: 'Select image', filters: [{ name: 'Images (*.*)', extensions: ['tif', 'exr']},], properties: ['openFile']}).then(result => {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"        

        let content = 
            Html.table [ 
                Html.row "Texture:" 
                    [
                        button [
                            clazz "ui button tiny";
                            style "margin-left: 10px";
                            Dialogs.onChooseFiles (fun chosen -> SetTexture (chosen) );
                            clientEvent "onclick" (jsImportDialog)
                        ] [
                            text "Import"
                        ]
                    ]
                Html.row "EXR Channel:" [
                    div [style "color: white;"] [
                        Html.SemUi.dropDown' (AList.ofAVal m.channelOptions) m.channelName (fun value -> SetEXRChannel (value)) (fun option -> option)
                        // Html.SemUi.dropDown m.channel SetEXRChannel
                    ]
                ]
                Html.row "False Color:" [
                    text "Activate: " 
                    Html.SemUi.toggleBox m.useFalseColor ToggleFalseColor
                    br []
                    Html.SemUi.dropDown m.colorMap SetColorMap
                ]
                Html.row "Minimum:" [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.customMinValue.value) SetMin
                    br []
                    Numeric.view' [Slider] m.customMinValue
                    |> UI.map (fun action -> 
                        match action with
                        | Numeric.Action.SetValue v ->
                            SetMin v
                        | _ ->
                            Empty
                        )
                    ]
                Html.row "Maximum:"  [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.customMaxValue.value) SetMax
                    br []
                    div [style "width: 100%"] [
                        Numeric.numericField' m.customMaxValue Slider
                        |> UI.map (fun action -> 
                            match action with
                            | Numeric.Action.SetValue v ->
                                SetMax v
                            | _ ->
                                Empty
                            )
                        ]
                    ] 
                Html.row "" [button [clazz "ui inverted button"; onClick (fun _ -> ResetMinMax)] [
                        text "Reset"
                    ]
                ]
            ]

        let accordion text' icon active content' =
                let title = if active then "title active inverted" else "title inverted"
                let content = if active then "content active" else "content"
               // let arrow = if active then 
                                    
                onBoot "$('#__ID__').accordion();" (
                    div [clazz "ui inverted segment"] [
                        div [clazz "ui inverted accordion fluid"] [
                            div [clazz title; style "background-color: #282828"] [
                                    i [clazz ("dropdown icon")] []
                                    text text'                                
                                    div [style "float:right"] [i [clazz (icon + " icon")] []]
                                
                            ]
                            div [clazz content;  style "overflow-y : auto; "] content' //max-height: 35%
                        ]
                    ]
                )

        require Html.semui (
            body [] [
                renderControl (AVal.constant (Camera.create cameraView frustum')) att instrumentVisualization
                div [style "position: fixed; left: 20px; top: 20px; width: 400px"] [
                    accordion "Texture Mapping" "file image outline" false [ content ]
                ]
            ])

    let app () =
        {
            initial = initial
            update = update
            view = view
            threads = fun m -> m.cameraState |> FreeFlyController.threads |> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }