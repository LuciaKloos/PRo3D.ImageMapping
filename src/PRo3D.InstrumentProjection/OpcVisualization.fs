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
open PRo3D.InstrumentVisualization

module MarsSurface = 

    module Shader = 
        open FShade
        open Aardvark.Rendering.Effects
        let blend (v : Vertex) = 
            fragment {
                let c = V4d(v.c.XYZ, 0.3)
                return c
            }

        let molaGrayScale = 
            sampler2d {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagMipPoint
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
            }

        let colorMap = 
            sampler2d {
                texture uniform?ColorMap
                filter Filter.MinMagMipPoint
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
            }

        let molaColor (v : Vertex) = 
            fragment {
                let color = colorMap.Sample(V2d(v.c.X * 1.5, 0.5))
                return color
            }

        let moreContrast (v : Vertex) = 
            fragment {
                // basic contrast boost around mid-grey (0.5)
                // tweak constants below to adjust effect
                let contrast = 1.3    // >1 increases contrast, <1 decreases
                let brightness = 0.0   // additive brightness offset
                // read input color (already provided by previous pipeline stages)
                let r = ((v.c.X - 0.5) * contrast) + 0.5 + brightness
                let g = ((v.c.Y - 0.5) * contrast) + 0.5 + brightness
                let b = ((v.c.Z - 0.5) * contrast) + 0.5 + brightness

                // clamp to [0,1]
                let rc = min 1.0 (max 0.0 r)
                let gc = min 1.0 (max 0.0 g)
                let bc = min 1.0 (max 0.0 b)

                return V4d(rc, gc, bc, v.c.W)
            }

    let getMarsSurfaceSg (runtime : IRuntime) (framebufferSignature : IFramebufferSignature) (scene : OpcScene) (projectedImageProperties : VisualizationProperties) 
                         (currentProjection : aval<Option<Trafo3d>>) (referenceFrame : aval<string>) (supportBody : aval<string>) 
                         (observer : aval<string>) (time : aval<DateTime>) (projectImage : string -> aval<Option<Trafo3d>>) 
                         (projectedTexture : aval<ITexture>) (colorMap : aval<ITexture>)=
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
                |> Sg.trafo (trafo |> AVal.map (fun v -> Option.defaultValue Trafo3d.Identity v))
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
        |> Sg.applyProjectedImage projectImage
        |> Sg.applyPlanet "mars"
        |> Sg.shader {
            do! ImageProjection.Shaders.stableImageProjectionTrafo
            do! ImageProjection.Shaders.generateNormal
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.diffuseTexture
            do! Shader.molaColor
            do! Shader.moreContrast
            do! ImageProjection.Shaders.stableImageProjection
        }
        |> InstrumentImageVisualization.applyProperties { projectedImageProperties with instrumentImage = projectedTexture }
        |> Sg.texture "ProjectedTexture" projectedTexture
        |> Sg.texture "ColorMap" colorMap
        //|> Sg.pass afterMain
        //|> Sg.blendMode' BlendMode.Blend