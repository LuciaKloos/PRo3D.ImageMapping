namespace PRo3D.InstrumentProjection

open System
open System.IO

open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.SceneGraph
open Aardvark.Rendering.Text
open Aardvark.Geometry
open Aardvark.FontProvider


open PRo3D.Extensions
open PRo3D.Extensions.FSharp
open PRo3D.SPICE
open PRo3D.Core
open PRo3D.Core.InstrumentMetadata
open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentData
open PRo3D.InstrumentVisualization


[<Struct>]
type RelState = 
    {
        pos : V3d
        vel : V3d
        rot : M33d
    }


module Visualization =

    let createSceneGraph (projectedImageProperties : VisualizationProperties) (currentProjectedImage : aval<Option<string * Option<ParsedMetadata>>>) (referenceFrame : aval<string>) (supportBody : aval<string>) (observer : aval<string>) (time : aval<DateTime>) =

        let farPlaneMars = 30101626.50 * 1000.0

        let instruments =
            let frustum = Frustum.perspective 5.5306897076421 1000.0 farPlaneMars 1.0
            Map.ofList [
                "HERA_AFC-1", frustum
                "HERA_AFC-2", frustum
                "HERA_HSH", Frustum.perspective 15.23999 1000.0 farPlaneMars (409.0 / 217.0)
            ]

        let projectImage (planet : string) = 
            (currentProjectedImage, observer, time) 
            |||> AVal.map3 (fun img observer time -> 
                match img with
                | _ -> 
                    let p = {
                        target = InstrumentImages.CameraFocus.FocusBody "MARS"
                        cameraSource =  InstrumentImages.CameraSource.InBody "HERA"
                        instrumentReferenceFrame = "HERA_AFC-1"
                        instrumentName = "HERA_AFC-1"
                        supportBody = "SUN"
                        time = time
                    }
                    InstrumentProjection.projectOnto "IAU_MARS"  observer instruments p
            )

        let projectedTexture = 
            currentProjectedImage |> AVal.bind (fun img -> 
                match img with
                | Some (img, Some _) -> 
                    match MultiBandReader.tryReadMultiBandTiff img false with
                    | Result.Ok img -> 
                        let images = InstrumentImageTextures.instrumentImageToTexture true img 
                        match Array.tryItem 0 images with
                        | Some img -> 
                            PixTexture2d(img.pi, TextureParams.empty) :> ITexture |> AVal.constant
                        | _ -> 
                            Log.warn "channel of out of bounds"
                            DefaultTextures.checkerboard
                    | _ -> 
                        Log.warn "could not load texture"
                        DefaultTextures.checkerboard
                | _ -> 
                    DefaultTextures.checkerboard
            )

        let marsProxy = 
            let marsTrafo = 
                Rendering.fullTrafo referenceFrame supportBody "MARS" (Some "IAU_MARS") observer time
                |> AVal.map (Option.defaultValue Trafo3d.Identity)

            let marsTexture = 
                let getImageStream () = 
                    typeof<RelState>.Assembly.GetManifestResourceStream("PRo3D.InstrumentProjection.resources.marswikiAnnotated.jpg")
                StreamTexture(getImageStream)

            let sphericalUnitBody (scale : float) = 
                PolyMeshPrimitives.Sphere(30, 1.0, C4b.White, DefaultSemantic.DiffuseColorCoordinates, DefaultSemantic.DiffuseColorUTangents, DefaultSemantic.DiffuseColorVTangents)
                                    .GetIndexedGeometry()

                |> Sg.ofIndexedGeometry

            sphericalUnitBody 1.0
            |> Sg.diffuseTexture' marsTexture
            |> Sg.applyProjectedImage projectImage
            |> Sg.applyPlanet "mars"
            |> Sg.scale (3389.5 * 1000.0) // mars radius in km
            |> Sg.trafo marsTrafo
            |> Sg.shader {
                do! Shaders.genAndFlipTextureCoord
                do! ImageProjection.Shaders.useVertexNormals
                do! ImageProjection.Shaders.stableImageProjectionTrafo
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.stableHeadlight
                do! ImageProjection.Shaders.stableImageProjection
            }
            |> InstrumentImageVisualization.applyProperties { projectedImageProperties with instrumentImage = projectedTexture }
            |> Sg.uniform' "ProjectedImageModelViewProjValid" true
            |> Sg.texture "ProjectedTexture" projectedTexture

        marsProxy