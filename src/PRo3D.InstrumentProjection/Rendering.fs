namespace PRo3D.SPICE

open System

open FSharp.Data.Adaptive
open Aardvark.Base

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

module Rendering =


    let getRelState (referenceFrame : aval<string>) (supportBody : aval<string>) (body : string) (observer : aval<string>) (time : aval<DateTime>) =
        AVal.custom (fun t -> 
            let observer = observer.GetValue(t)
            let time = time.GetValue(t)
            let referenceFrame = referenceFrame.GetValue(t)
            let supportBody = supportBody.GetValue(t)
            CooTransformation.getRelState body supportBody observer time referenceFrame
        )

    let getPosition (referenceFrame : aval<string>) (supportBody : aval<string>) (body : string) (observer : aval<string>) (time : aval<DateTime>) = 
        let getPos (o : CooTransformation.RelState) = o.pos
        getRelState referenceFrame supportBody body observer time |> AVal.map (Option.map getPos)


    let fullTrafo  (referenceFrame : aval<string>) (supportBody : aval<string>) (body : string) (bodyFrame : Option<string>) (observer : aval<string>) (time : aval<DateTime>) =
        let rotation = 
            match bodyFrame with
            | None -> AVal.constant None
            | Some frame -> 
                (referenceFrame, time) ||> AVal.map2 (fun referenceFrame time -> CooTransformation.getRotationTrafo frame referenceFrame time)
        let pos = getRelState referenceFrame supportBody body observer time
        (rotation, pos) ||> AVal.map2 (fun rot relState -> 
            match rot, relState with
            | Some rot, Some relState -> 
                Some (rot * Trafo3d.Translation relState.pos)
            | None, Some relState -> 
                relState.pos |> Trafo3d.Translation |> Some
            | _ -> 
                None
        )