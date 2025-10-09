namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | SetMin of float


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

    let hshColors (v : Vertex) = 
        fragment {
            let hshValue = instrumentSampler.Sample(v.tc).X // 0-1 range
            let minRange = uniform.MinValue
            let maxRange = 0.8
            let mapped = (hshValue - minRange) / (maxRange - minRange)
            return V4d(mapped, mapped, mapped, 1.0)
            //return V4d(1.0,0.0,0.0,1.0)
        }

module App =
    
    let initial = { currentModel = Box; cameraState = FreeFlyController.initial; minValue = 0 }

    let update (m : Model) (msg : Message) =
        match msg with
            | ToggleModel -> 
                match m.currentModel with
                    | Box -> { m with currentModel = Sphere }
                    | Sphere -> { m with currentModel = Box }

            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

            | SetMin v -> 
                { m with minValue = int v }
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

        let instrumentVisualization = 
            Sg.fullScreenQuad
            |> Sg.noEvents
            |> Sg.fileTexture "InstrumentImage" @"C:\pro3ddata\HERA\20250428_HSH_layers\1_Fits2Mbi\HSH_0CRS63_250312T121545_1A_782.tif" true
            |> Sg.uniform "MinValue" (m.minValue |> AVal.map (fun v -> float v / 65535.0))
            |> Sg.shader {
                do! Shaders.hshColors
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
                button [onClick (fun _ -> ToggleModel)] [text "Toggle Model"]
                br []
                SimplePrimitives.numeric { min = 0.0; max = 65535; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.minValue |> AVal.map float) SetMin
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