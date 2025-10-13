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

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | SetMin of float
    | SetMax of float
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

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue

    let hshColors ((imageMin : int, imageMax : int)) (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X // 0-1 range
            let hshValueY = instrumentSampler.Sample(v.tc).Y // 0-1 range
            let hshValueZ = instrumentSampler.Sample(v.tc).Z // 0-1 range

            let remapClampNormalize (a: float) (b: float) (x: float) (y: float) (z: float) =
                V4d(
                    ((min uniform.MaxValue (max uniform.MinValue (a + x * (b - a)))) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue),
                    ((min uniform.MaxValue (max uniform.MinValue (a + y * (b - a)))) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue),
                    ((min uniform.MaxValue (max uniform.MinValue (a + z * (b - a)))) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue),
                    1.0
                )
               
            return remapClampNormalize (float imageMin) (float imageMax) hshValueX hshValueY hshValueZ 
        }


module App =

    let minValue = {
        value   = 16.0 
        min     = 16.0
        max     = 4095.0
        step    = 1
        format  = "{0:0.00}"
    }

    let maxValue = {
        value   = 4095 
        min     = 16.0
        max     = 4095.0
        step    = 1
        format  = "{0:0.00}"
    }
    
    let initial = { currentModel = Box; cameraState = FreeFlyController.initial; minValue = minValue; maxValue = maxValue; }

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

    let update (m : Model) (msg : Message) =
        match msg with
            | ToggleModel -> 
                match m.currentModel with
                    | Box -> { m with currentModel = Sphere }
                    | Sphere -> { m with currentModel = Box }

            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

            | SetMin v -> 
                { m with minValue = {minValue with value = v} }
            | SetMax v -> 
                { m with maxValue = {maxValue with value = v} }
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

        let minMax = getMinMaxFromStatistics(@"pfad.tif.json")

        let instrumentVisualization = 
            
            Sg.fullScreenQuad
            |> Sg.noEvents
            |> Sg.fileTexture "InstrumentImage" @"pfad.tif" true
            |> Sg.uniform "MinValue" (m.minValue.value |> AVal.map (fun v -> float v)) // float v / 65535.0
            |> Sg.uniform "MaxValue" (m.maxValue.value |> AVal.map (fun v -> float v))
            |> Sg.shader {
                do! (Shaders.hshColors minMax)
            }


        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        body [] [
            renderControl (AVal.constant (Camera.create cameraView frustum')) att instrumentVisualization
            //FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg

            div [style "position: fixed; left: 20px; top: 20px"] [
                //SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.minValue) SetMin
                //SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.maxValue) SetMax
                Numeric.view' [Slider] m.minValue
                |> UI.map (fun action -> 
                    match action with
                    | Numeric.Action.SetValue v ->
                        SetMin v
                    | _ ->
                        Empty
                    )
                Numeric.view' [Slider] m.maxValue
                |> UI.map (fun action -> 
                    match action with
                    | Numeric.Action.SetValue v ->
                        SetMax v
                    | _ ->
                        Empty
                    )
                ]
            ]

    let app () =
        {
            initial = initial
            update = update
            view = view
            threads = fun m -> m.cameraState |> FreeFlyController.threads |> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }