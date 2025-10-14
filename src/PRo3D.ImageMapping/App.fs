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
    | SetDefaultMinMax of float * float
    | SetTexturePath of string list
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

    let hshColors ((imageMin : int, imageMax : int)) (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X * 65000.0 // 0-1 range
            let hshValueY = instrumentSampler.Sample(v.tc).Y * 65000.0 // 0-1 range
            let hshValueZ = instrumentSampler.Sample(v.tc).Z * 65000.0 // 0-1 range

            let remapClampNormalize =
                colormapTextureSampler.Sample(V2d (((min uniform.MaxValue (max uniform.MinValue hshValueX)) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue), 0.0))
               
            return remapClampNormalize
        }


module App =

    let initialPath = "pfad.tif"

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
        currentModel = Box;
        cameraState = FreeFlyController.initial;
        defaultMinValue = minValue.value;
        defaultMaxValue = maxValue.value;
        setMinValue = minValue;
        setMaxValue = maxValue;
        texture = initialPath;
    }

    let update (m : Model) (msg : Message) =
        match msg with
            | ToggleModel -> 
                match m.currentModel with
                    | Box -> { m with currentModel = Sphere }
                    | Sphere -> { m with currentModel = Box }

            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

            | SetMin v -> 
                { m with setMinValue = {minValue with value = v} }
            | SetMax v -> 
                { m with setMaxValue = {maxValue with value = v} }
            | ResetMinMax ->
                { m with setMinValue = {minValue with value = m.defaultMinValue}; setMaxValue = {maxValue with value = m.defaultMaxValue} }
            | SetDefaultMinMax (min, max) ->
                { m with defaultMinValue = min; defaultMaxValue = max }
            | SetTexturePath texture ->
                let (min, max) = getMinMaxFromStatistics(texture[0] + ".json")
                { m with texture = texture[0]; defaultMinValue = min; defaultMaxValue = max; setMinValue = {minValue with value = m.defaultMinValue}; setMaxValue = {maxValue with value = m.defaultMaxValue} }
            | Empty ->
                m
    let view (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        let sg =
            m.currentModel |> AVal.map (fun v ->
                match v with
                    | Box -> Sg.box (AVal.constant C4b.Red) (AVal.constant (Box3d(-V3d.III, V3d.III)))
                    | Sphere -> Sg.sphere 5 (AVal.constant C4b.Green) (AVal.constant 1.0)
            )
            |> Sg.dynamic
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }

        let transferFunctionName : aval<string> = AVal.constant "magma"
        let colormapTexture : aval<ITexture> =
            transferFunctionName
            |> AVal.map (fun path ->
                FileTexture(@"..\ressources\magma.png", TextureParams.empty)
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
            |> Sg.uniform "MinValue" (m.setMinValue.value |> AVal.map (fun v -> float v)) // float v / 65535.0
            |> Sg.uniform "MaxValue" (m.setMaxValue.value |> AVal.map (fun v -> float v))
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.shader {
                do! (Shaders.hshColors (min, max))
            }


        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        let jsImportDialog = "top.aardvark.dialog.showOpenDialog({tile: 'Select AFC / TIRI image', filters: [{ name: 'Images (*.tif)', extensions: ['tif']},], properties: ['openFile']}).then(result => {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"        


        require Html.semui (
            body [] [
                renderControl (AVal.constant (Camera.create cameraView frustum')) att instrumentVisualization
                //FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg

                div [style "position: fixed; left: 20px; top: 20px"] [
                    button [
                        clazz "ui button tiny";
                        style "margin-left: 10px";
                        Dialogs.onChooseFiles (fun chosen -> SetTexturePath chosen );
                        clientEvent "onclick" (jsImportDialog)
                    ] [
                        text "Import Texture"
                    ]
                    Html.table [ 
                        Html.row "Minimum:" [
                            SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.setMinValue.value) SetMin
                            br []
                            Numeric.view' [Slider] m.setMinValue
                            |> UI.map (fun action -> 
                                match action with
                                | Numeric.Action.SetValue v ->
                                    SetMin v
                                | _ ->
                                    Empty
                                )
                            ]
                        
                        Html.row "Maximum:"  [
                            SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.setMaxValue.value) SetMax
                            br []
                            div [style "width: 100%"] [
                                Numeric.numericField' m.setMaxValue Slider
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
                    ]]
                ]
            )

    let app () =
        {
            initial = initial
            update = update
            view = view
            threads = fun m -> m.cameraState |> FreeFlyController.threads |> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }