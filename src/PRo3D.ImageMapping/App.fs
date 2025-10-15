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

open Dialogs

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | SetMin of float
    | SetMax of float
    | ResetMinMax
    | SetTexturePath of string list
    | SetColorMap of ColorMap
    | SetImageType of ImageType
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

    let hshColors (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X * 65000.0 // 0-1 range

            let remapClampNormalize =
                colormapTextureSampler.Sample(V2d (((min uniform.MaxValue (max uniform.MinValue hshValueX)) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue), 0.0))
               
            return remapClampNormalize
        }


module App =

    let initialPath = @"C:\Users\pichler\Documents\Code\PRo3D\ImageMapping.Data\AF1_converted\1A\AF1_0CRQMP_250312T115031_1A.tif"

    let getMinMaxFromStatistics (filePath: string) =
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
                    let obj = token.[0] :?> JObject
                    match obj.TryGetValue(k) with
                        | true, t -> loop t rest
                        | _ -> token.Value<int>()
                | _ -> token.Value<int>()

        (loop (j :> JToken) ["image_statistics"; "minimum"], loop (j :> JToken) ["image_statistics"; "maximum"])

    let (min, max) = getMinMaxFromStatistics(initialPath + ".json")

    let minValue = {
        value   = min
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }

    let maxValue = {
        value   = max
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }
    
    let initial = { 
        cameraState = FreeFlyController.initial;
        colorMap = ColorMap.Magma;
        imageType = ImageType.AFC;
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
            | SetTexturePath texture ->
                let (min, max) = getMinMaxFromStatistics(texture[0] + ".json")
                { m with texture = texture[0]; defaultMinValue = min; defaultMaxValue = max; customMinValue = {minValue with value = min}; customMaxValue = {maxValue with value = max} }
            | SetColorMap (map : ColorMap) ->
                { m with colorMap = map }
            | SetImageType (t : ImageType) ->
                { m with imageType = t }
            | Empty ->
                m
    let view (m : AdaptiveModel) =

        let colormapTexture : aval<ITexture> =
            m.colorMap
            |> AVal.map (fun map ->
                FileTexture(Path.Combine(Path.Combine(Directory.GetCurrentDirectory(), "resources"), ColorMap.getColorMapFileName(map)), TextureParams.empty)
            )

        let imageTexture : aval<ITexture> =
            m.texture
            |> AVal.map (fun path ->
                FileTexture(path, TextureParams.empty)
            )

        let instrumentVisualization = 
            
            Sg.fullScreenQuad
            |> Sg.noEvents
            //|> Sg.fileTexture "InstrumentImage" @"C:\Users\sophiechen\Documents\WORK\VRVis\PRo3D\ImageMapping.Data\AF1_converted\1A\AF1_0CRQMP_250312T115031_1A.tif" true
            |> Sg.texture "InstrumentImage" imageTexture
            |> Sg.uniform "MinValue" (m.customMinValue.value |> AVal.map (fun v -> float v)) // float v / 65535.0
            |> Sg.uniform "MaxValue" (m.customMaxValue.value |> AVal.map (fun v -> float v))
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.shader {
                do! (Shaders.hshColors)
            }


        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        let jsImportDialog = "top.aardvark.dialog.showOpenDialog({tile: 'Select AFC / TIRI image', filters: [{ name: 'Images (*.tif)', extensions: ['tif']},], properties: ['openFile']}).then(result => {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"        
        
        let imageTypeContent = 
            div [style "color: white;"] [
                text "Image Type: "
                Html.SemUi.dropDown m.imageType SetImageType
            ]

        let afcTiriContent = Html.table [ 
                            Html.row "Texture:" 
                                [
                                    button [
                                        clazz "ui button tiny";
                                        style "margin-left: 10px";
                                        Dialogs.onChooseFiles (fun chosen -> SetTexturePath chosen );
                                        clientEvent "onclick" (jsImportDialog)
                                    ] [
                                        text "Import"
                                    ]
                                ]
                            Html.row "False Color:" [Html.SemUi.dropDown m.colorMap SetColorMap]
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

        (*
        let content = 
            match m.imageType with
            | ImageType.AFC -> afcTiriContent
            | _ -> afcTiriContent*)

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
                    accordion "Texture Mapping" "file image outline" false [ imageTypeContent; afcTiriContent ]
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