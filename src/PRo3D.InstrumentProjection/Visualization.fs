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
open Aardvark.GeoSpatial.Opc

open PRo3D.Extensions
open PRo3D.Extensions.FSharp
open PRo3D.SPICE
open PRo3D.Core
open PRo3D.Core.InstrumentMetadata
open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentData
open PRo3D.InstrumentVisualization


type Self = Self

type Channel = {
    idx : int
    name : Option<string>
}

module Visualization =

    let createProjectedExrTexture (path : string) (channel : int) : aval<ITexture> = 
        let stream = File.OpenRead path
        let exrTexture = TextureLoading.loadImageFromStream stream (ChannelReference.ChannelWithIndex channel) (Some TextureLoading.TextureFormat.OpenEXR)
        PixTexture2d(exrTexture, TextureParams.empty) :> ITexture |> AVal.constant

    let createProjectedTiffTexture (path : string) (channel : int) : aval<ITexture> = 
        match MultiBandReader.tryReadMultiBandTiff path false with
        | Result.Ok img -> 
            let images = InstrumentImageTextures.instrumentImageToTexture true img 
            match Array.tryItem channel images with
            | Some img -> 
                PixTexture2d(img.pi, TextureParams.empty) :> ITexture |> AVal.constant
            | _ -> 
                Log.warn "channel of out of bounds"
                DefaultTextures.checkerboard
        | _ -> 
            Log.warn "could not load texture"
            DefaultTextures.checkerboard

    let createProjectedTexture (currentProjectedImage : aval<Option<string * ParsedMetadata>>) (channel: aval<Channel>) : aval<ITexture> = 
        AVal.bind2 (fun img  c -> 
            match img with
            | Some (img : string, (Some mbi, _)) -> 
                match Path.GetExtension(img).ToLower() with
                | ".tiff" | ".tif" -> createProjectedTiffTexture img c.idx
                | ".exr" -> createProjectedExrTexture img c.idx
                | _ -> DefaultTextures.checkerboard
            | _ -> 
                DefaultTextures.checkerboard
        ) currentProjectedImage channel

    let creatProjectionFunction (observer : aval<string>) (time : aval<DateTime>) (referenceFrame : aval<string>) 
                                (currentProjectedImage : aval<Option<string * ParsedMetadata>>) (projection : aval<InstrumentProjection>) =

    
        let farPlaneMars = 30101626.50 * 1000.0
        let instruments =
            let frustum = Frustum.perspective 5.5306897076421 1000.0 farPlaneMars 1.0
            let hsh = Frustum.perspective 15.23999 1000.0 farPlaneMars (217.0 / 409.0)
            let hsh2 = Frustum.perspective 15.23999  1000.0 farPlaneMars (409.0 / 217.0)
            let hsh3 = Frustum.perspective 9.9 1000.0 farPlaneMars (217.0 / 409.0)
            Map.ofList [
                "HERA_AFC-1", frustum
                "HERA_AFC-2", frustum
                "HERA_HSH", hsh2
            ]

        let projectImage (targetPlanet : string) = 
                AVal.custom (fun t -> 
                    let img = currentProjectedImage.GetValue t
                    match img with
                    | Some (_, (Some mbi,_)) -> 
                        let observer = observer.GetValue t
                        let time = time.GetValue t
                        let referenceFrame = referenceFrame.GetValue t
                        let projection = projection.GetValue t
                        let p = {
                            projection with
                                time = time
                            }
                        let t = InstrumentProjection.projectOntoQuat referenceFrame observer instruments p (-mbi.targetPos * 1000.0) mbi.sc_quat
                        let spice = InstrumentProjection.projectOnto referenceFrame observer instruments p
                        spice
                    | _  -> 
                        None
                )

        projectImage

   

    let createSceneGraph
        (projectedImageProperties : VisualizationProperties)
        (referenceFrame : aval<string>)
        (supportBody : aval<string>)
        (observer : aval<string>)
        (time : aval<DateTime>)
        (projectPrimaryImage : string -> aval<Option<Trafo3d>>)
        (projectedRedTexture : aval<ITexture>)
        (projectedGreenTexture : aval<ITexture>)
        (projectedBlueTexture : aval<ITexture>)
        (redMin : aval<float>)
        (redMax : aval<float>)
        (greenMin : aval<float>)
        (greenMax : aval<float>)
        (blueMin : aval<float>)
        (blueMax : aval<float>)
        (rgbDataType : aval<int>)
        (rgbProjectionDebug : aval<bool>)
        (primaryProjectionEnabled : aval<bool>) =

        let marsProxy =
            let marsTrafo =
                Rendering.fullTrafo
                    referenceFrame
                    supportBody
                    "MARS"
                    (Some "IAU_MARS")
                    observer
                    time
                |> AVal.map (Option.defaultValue Trafo3d.Identity)

            let marsTexture =
                let getImageStream () =
                    typeof<Self>.Assembly.GetManifestResourceStream(
                        "PRo3D.InstrumentProjection.resources.marswikiAnnotated.jpg"
                    )

                StreamTexture(getImageStream)

            let sphericalUnitBody (scale : float) =
                PolyMeshPrimitives.Sphere(
                    30,
                    1.0,
                    C4b.White,
                    DefaultSemantic.DiffuseColorCoordinates,
                    DefaultSemantic.DiffuseColorUTangents,
                    DefaultSemantic.DiffuseColorVTangents
                ).GetIndexedGeometry()
                |> Sg.ofIndexedGeometry

            sphericalUnitBody 1.0
            |> Sg.diffuseTexture' marsTexture
            |> PRo3D.Core.Sg.applyProjectedImage projectPrimaryImage
            |> Sg.applyPlanet "mars"
            |> Sg.scale (3389.5 * 1000.0)
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
            // applyProperties provides ProjectedImageOpacity.
            |> InstrumentImageVisualization.applyProperties {
                projectedImageProperties with
                    instrumentImage = projectedRedTexture
            }
            |> Sg.uniform'
                "ProjectedImageModelViewProjValid"
                primaryProjectionEnabled
            |> Sg.texture
                "ProjectedRedTexture"
                projectedRedTexture
            |> Sg.texture
                "ProjectedGreenTexture"
                projectedGreenTexture
            |> Sg.texture
                "ProjectedBlueTexture"
                projectedBlueTexture
            |> Sg.uniform "RedMinValue" redMin
            |> Sg.uniform "RedMaxValue" redMax
            |> Sg.uniform "GreenMinValue" greenMin
            |> Sg.uniform "GreenMaxValue" greenMax
            |> Sg.uniform "BlueMinValue" blueMin
            |> Sg.uniform "BlueMaxValue" blueMax
            |> Sg.uniform "RgbDataType" rgbDataType
            |> Sg.uniform "RgbProjectionDebug" rgbProjectionDebug

        marsProxy