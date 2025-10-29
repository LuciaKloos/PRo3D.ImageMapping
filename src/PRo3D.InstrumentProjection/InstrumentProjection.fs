namespace PRo3D.SPICE

open System

open Aardvark.Base
open Aardvark.Rendering

open PRo3D.Extensions
open PRo3D.Extensions.FSharp
open PRo3D.Core
open PRo3D.Core.InstrumentMetadata

module InstrumentImages = 

    open Aardvark.Rendering

    type Extrinsics = 
        | Plain of CameraView

    type Intrinsics = 
        | Plain of Frustum

    type ImageData = 
        | FilePath of string

    type ProjectedImage =
        {
            intrinsics : Intrinsics
            extrinsics : Extrinsics
            image      : Option<ImageData>
        }

    type CameraFocus = 
        | FocusBody of focusedBody : string

    type CameraSource =
        | InBody of body : string

    type Intrinsics with
        member x.ProjTrafo = 
            match x with
            | Intrinsics.Plain frustum -> Frustum.projTrafo frustum

type InstrumentProjection = 
    {
        instrumentReferenceFrame : string
        target : InstrumentImages.CameraFocus
        cameraSource : InstrumentImages.CameraSource
        instrumentName : string
        supportBody : string
        time : DateTime
    }

module InstrumentProjection =

    let getLookAt (viewerBody : string) (observer : string) (referenceFrame : string) (supportBody : string) (time : DateTime) =
        let afc1Pos = CooTransformation.getRelState viewerBody supportBody observer time referenceFrame
        match afc1Pos with    
        | Some targetState -> 
            let rot = targetState.rot
            let t = Trafo3d.FromBasis(-rot.C1, rot.C0, rot.C2, targetState.pos)
            CameraView.ofTrafo t.Inverse |> Some 
            CameraView.lookAt targetState.pos V3d.OOO V3d.OOI |> Some
        | _ -> 
            None

    let projectOnto (referenceFrame : string) (observer : string) (instruments : Map<string, Frustum>) (p : InstrumentProjection) = 
        let bodyToWorld = CooTransformation.getRotationTrafo referenceFrame p.instrumentReferenceFrame p.time
        match bodyToWorld, p.target, p.cameraSource, Map.tryFind p.instrumentName instruments with
        | Some bodyToWorld, InstrumentImages.FocusBody target, InstrumentImages.InBody source, Some frustum -> 
            match getLookAt source observer p.instrumentReferenceFrame p.supportBody p.time with
            | Some view ->
                bodyToWorld * CameraView.viewTrafo view * (Frustum.projTrafo frustum) |> Some
            | None -> None
        | _ -> None