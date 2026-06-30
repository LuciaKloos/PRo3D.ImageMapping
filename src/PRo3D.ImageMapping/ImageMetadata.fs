namespace PRo3D.ImageMapping

open System.IO
open System.Text.Json
open Aardvark.Base

module ImageMetadata =

    let tryGetProperty (name : string) (element : JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>
        if element.TryGetProperty(name, &property) then Some property else None

    let tryGetString (name : string) (element : JsonElement) =
        match tryGetProperty name element with
        | Some property when property.ValueKind = JsonValueKind.String ->
            property.GetString() |> Option.ofObj
        | _ ->
            None

    let tryGetInt (name : string) (element : JsonElement) =
        match tryGetProperty name element with
        | Some property when property.ValueKind = JsonValueKind.Number ->
            let mutable value = 0
            if property.TryGetInt32(&value) then Some value else None
        | _ ->
            None

    let tryGetDouble (name : string) (element : JsonElement) =
        match tryGetProperty name element with
        | Some property when property.ValueKind = JsonValueKind.Number ->
            let mutable value = 0.0
            if property.TryGetDouble(&value) then Some value else None
        | _ ->
            None
