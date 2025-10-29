namespace PRo3D.Core

open System
open MBrace.FsPickler
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Data.Opc
open Aardvark.GeoSpatial.Opc
open Aardvark.Rendering
open PRo3D.SPICE
open Aardvark.GeoSpatial.Opc.Load
open Aardvark.Data
open Aardvark.SceneGraph

module MarsSurface = 

    module Shader = 
        open FShade
        open Aardvark.Rendering.Effects
        let blend (v : Vertex) = 
            fragment {
                let c = V4d(v.c.XYZ, 0.3)
                return c
            }

    let getMarsSurfaceSg (runtime : IRuntime) (framebufferSignature : IFramebufferSignature) (scene : OpcScene) 
                         (currentProjection : aval<Option<Trafo3d>>) (referenceFrame : aval<string>) (supportBody : aval<string>) 
                         (observer : aval<string>) (time : aval<DateTime>) =
        let runner = 
            match runtime with
            | :? Aardvark.Rendering.GL.Runtime as r -> r.CreateLoadRunner 1
            | _ -> failwith "must run with gl runtime."
        let serializer = FsPickler.CreateBinarySerializer()

        let trafo = 
            Rendering.fullTrafo referenceFrame supportBody "MARS" (Some "IAU_MARS") observer time

        let createSg (sunLightEnabled : aval<bool>) (body : string) (bodyFrame : string) (hierarchies : seq<string>) =

            let bodyPos = Rendering.getPosition referenceFrame supportBody body observer time
            let sunLightDirection = 
                let sunPos = Rendering.getPosition referenceFrame (AVal.constant "EARTH") "Sun" observer time
                (sunPos, bodyPos)
                ||> AVal.map2 (fun sunPos bodyPos -> 
                    match sunPos, bodyPos with
                    | Some sunPos, Some bodyPos -> sunPos - bodyPos |> Vec.normalize
                    | _ -> V3d.Zero
                )


            hierarchies
            |> Seq.toList 
            |> List.map (fun basePath -> 
                let h = PatchHierarchy.load serializer.Pickle serializer.UnPickle (OpcPaths.OpcPaths basePath)
                let t = PatchLod.toRoseTree h.tree

                //let imageProjection = firstProjection |> AVal.map Option.Some
                let localImageProjectionTrafos = Array.empty |> AVal.constant
                let sunLight = sunLightDirection |> AVal.map Option.Some

                //let additionalUniforms = 
                //    PRo3D.Core.ImageProjectionOpcExtensions.projectionUniformMap imageProjection localImageProjectionTrafos sunLight (AVal.constant true)
                let additionalUniforms = PRo3D.Core.ImageProjectionOpcExtensions.projectionUniformMap 

                let n =
                    Aardvark.GeoSpatial.Opc.PatchLod.PatchNode(
                                framebufferSignature, runner, basePath, scene.lodDecider, true, true, ViewerModality.XYZ, 
                                PatchLod.CoordinatesMapping.Local, true, PRo3D.Core.OpcRenderingExtensions.captureContext, additionalUniforms,
                                t,
                                None, None, PixImagePfim.Loader
                    )

                n
                |> Sg.trafo (trafo |> AVal.map (Option.defaultValue Trafo3d.Identity))
                |> Sg.applyBody (AVal.constant (Some "MARS"))
                |> Sg.applyProjectedImages' (
                    fun s -> 
                        s |> AVal.map (fun _ -> 
                            Some {
                                imageProjection = currentProjection
                                localImageProjectionTrafos = localImageProjectionTrafos
                                sunDirection  = sunLight
                                sunLightEnabled = sunLightEnabled
                            }
                        )
                    )
            ) 
 
        let mola = 
            createSg (AVal.constant true) "MARS" "IAU_MARS" scene.patchHierarchies 
            |> Sg.ofSeq 


        let afterMain = RenderPass.after "Main" RenderPassOrder.Arbitrary RenderPass.main
        Sg.ofList [
            mola; 
        ]
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.diffuseTexture
        }
        //|> Sg.pass afterMain
        //|> Sg.blendMode' BlendMode.Blend