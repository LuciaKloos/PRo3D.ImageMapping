namespace PRo3D.SPICE

open System

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering

open PRo3D.Extensions
open PRo3D.Extensions.FSharp


module Camera =

    let getLookAt (viewerBody : string) (observer : string) (referenceFrame : string) (supportBody : string) (time : DateTime) =
        let afc1Pos = CooTransformation.getRelState viewerBody supportBody observer time referenceFrame
        let camToSpace = CooTransformation.getRotationTrafo "HERA_AFC-1" referenceFrame time
        match afc1Pos, camToSpace with    
        | Some targetState, Some toSpace -> 
            let rot = targetState.rot
            let t = toSpace * Trafo3d.FromBasis(rot.C0, rot.C1, rot.C2, targetState.pos)
            let right = toSpace.Forward.C0.XYZ
            let forward = rot.C2
            let up = right.Cross(forward)
            let t = Trafo3d.FromBasis(right, up, forward, targetState.pos)
            let t = Trafo3d.FromBasis(-rot.C1, rot.C0, rot.C2, targetState.pos)
            //let t = Trafo3d.FromBasis(rot.C0, rot.C1, rot.C2, targetState.pos)
            //CameraView.ofTrafo t.Inverse |> Some 
            CameraView.lookAt targetState.pos V3d.Zero V3d.OOI |> Some
        | _ -> 
            None