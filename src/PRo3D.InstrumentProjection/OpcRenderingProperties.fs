namespace PRo3D.Core

open Aardvark.Base

open FSharp.Data.Adaptive
open System.Collections.Generic
open Aardvark.GeoSpatial.Opc.PatchLod
open Aardvark.GeoSpatial.Opc

[<AutoOpen>]
module SgExtensions =

    module Sg = 

        open Aardvark.Base.Ag
        open Aardvark.SceneGraph
        open Aardvark.SceneGraph.Semantics
        open Aardvark.UI

        type BodyApplicator(child : ISg, body : aval<Option<string>>) =
            inherit Sg.AbstractApplicator(child)
            member x.Body = body
        

        [<Rule>]
        type BodySem() =
            member x.Body(app : BodyApplicator, scope : Ag.Scope) =
                app.Child?Body <- app.Body
            member x.Body(s : Root<ISg>, scope : Ag.Scope) =
                let empty : aval<Option<string>> = AVal.constant None
                s.Child?Body <- AVal.constant empty

        type ProjectedImages = 
            {
                imageProjection : aval<Option<Trafo3d>>
                localImageProjectionTrafos : aval<array<Trafo3d>>
                sunDirection : aval<Option<V3d>>
                sunLightEnabled : aval<bool>
            }

        type ProjectedImageApplicator(child : ISg, images : aval<Option<string>> -> aval<Option<ProjectedImages>>) =
            inherit Sg.AbstractApplicator(child)
            member x.Images = images

        [<Rule>]
        type ProjectedImageSem() =
            member x.ProjectedImages(app : ProjectedImageApplicator, scope : Ag.Scope) =
                app.Child?ProjectedImages <- app.Images

        let applyBody (s : aval<Option<string>>) (sg : ISg) = 
            BodyApplicator(sg, s) :> ISg

        let applyProjectedImages' (s : aval<Option<string>> -> aval<Option<ProjectedImages>>) (sg : ISg) = 
            ProjectedImageApplicator(sg, s) :> ISg

        let applyProjectedImages (s : aval<Option<string>> -> aval<Option<ProjectedImages>>) (sg : ISg<_>) = 
            ProjectedImageApplicator(sg, s) 
            |> Sg.noEvents


module OpcRenderingExtensions =
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph.Semantics
    open SgExtensions.Sg

    type Ag.Scope with
        member x.FootprintVP : aval<M44d> = x?FootprintVP
        member x.ProjectedImages : aval<Option<string>> -> aval<Option<ProjectedImages>> = x?ProjectedImages
        member x.Body : aval<Option<string>> = x?Body

    type Context = 
        { 
            footprintVP : aval<M44d> 
            modelTrafo: aval<Trafo3d>
            projectedImages : aval<Option<Sg.ProjectedImages>>
            texturesScope : obj
            agScope : Ag.Scope
        }

    let captureContext (n : PatchNode) (s : Ag.Scope) =
        let modelTrafo = s.ModelTrafo
        let body = s.Body
        let projectedImages = s.ProjectedImages s.Body

        {   footprintVP = AVal.constant M44d.Identity; texturesScope = Unchecked.defaultof<_>; 
            modelTrafo = modelTrafo;
            projectedImages = projectedImages
            agScope = s 
        }  :> obj

namespace PRo3D.Core

open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.SceneGraph.Semantics


        

module ImageProjectionOpcExtensions = 

    let projectionUniformMap : Map<string, obj -> Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch -> IAdaptiveValue> =
        Map.ofList [
            "ProjectedImagesLocalTrafos", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) -> 
                let context = scope |> unbox<OpcRenderingExtensions.Context> 
                context.projectedImages |> AVal.bind (function 
                    | None -> AVal.constant Array.empty<M44f>
                    | Some p ->
                        (p.localImageProjectionTrafos, context.modelTrafo)
                        ||> AVal.map2 (fun arr modelTrafo ->  
                            arr |> Array.map (fun (vp : Trafo3d) -> 
                                // first to body space, then through projection
                                vp.Forward  * patch.info.Local2Global.Forward  |> M44f
                            )
                        )
                ) :> IAdaptiveValue
            )
            "ProjectedImagesLocalTrafosCount", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) -> 
                let context = scope |> unbox<OpcRenderingExtensions.Context>
                context.projectedImages |> AVal.bind (function
                    | None -> AVal.constant 0 
                    | Some p -> 
                        (p.localImageProjectionTrafos |> AVal.map Array.length)
                ) :> IAdaptiveValue
            )
            "ProjectedImageModelViewProjValid", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) -> 
                let context = scope |> unbox<OpcRenderingExtensions.Context>
                context.projectedImages |> AVal.bind (function
                    | None -> AVal.constant false
                    | Some p -> 
                        p.imageProjection |> AVal.map Option.isSome 
                ) :> IAdaptiveValue
            )
            "ProjectedImageModelViewProj", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) -> 
                let context = scope |> unbox<OpcRenderingExtensions.Context>
                context.projectedImages |> AVal.bind (function 
                    | None -> AVal.constant M44d.Identity
                    | Some p -> 
                        (p.imageProjection, context.modelTrafo) ||> AVal.map2 (fun vp m -> 
                            match vp with
                            | Some vp -> 
                                vp.Forward * patch.info.Local2Global.Forward
                            | None -> 
                                M44d.Identity
                        ) 
                ) :> IAdaptiveValue
            )
            "ApproximateBodyNormalLocalSpace", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) ->
                patch.info.Local2Global.Backward.TransformDir(patch.info.GlobalBoundingBox.Center.Normalized).Normalized |> AVal.constant :> IAdaptiveValue
            )
            "SunDirectionWorld", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) -> 
                let context = scope |> unbox<OpcRenderingExtensions.Context>
                context.projectedImages |> AVal.bind (function 
                    | None -> V3d.OOO |> AVal.constant 
                    | Some d -> 
                        d.sunDirection |> AVal.map (Option.defaultValue V3d.Zero)
                ) :> IAdaptiveValue
            )
            "SunLightEnabled", (fun scope (patch : Aardvark.GeoSpatial.Opc.PatchLod.RenderPatch) ->
                let context = scope |> unbox<OpcRenderingExtensions.Context>
                context.projectedImages |> AVal.bind (function 
                | None -> false |> AVal.constant
                | Some p -> 
                    (p.sunLightEnabled, p.sunDirection) 
                    ||> AVal.map2 (fun enabled dir -> Option.isSome dir && enabled) 
                ) :> IAdaptiveValue
            )
        ]

