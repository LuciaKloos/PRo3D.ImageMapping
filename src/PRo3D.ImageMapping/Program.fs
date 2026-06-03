open PRo3D.ImageMapping

open Aardium
open Aardvark.UI
open System
open System.IO
open Suave
open Aardvark.Base
open PRo3D.SPICE



let private tryGetCommandLineValue (name : string) (argv : string[]) =
    let equalsPrefix = name + "="

    let rec loop i =
        if i >= argv.Length then
            None
        else
            let arg = argv.[i]

            if arg.Equals(name, StringComparison.OrdinalIgnoreCase) then
                if i + 1 < argv.Length then
                    Some argv.[i + 1]
                else
                    failwithf "Missing value after command-line argument '%s'." name

            elif arg.StartsWith(equalsPrefix, StringComparison.OrdinalIgnoreCase) then
                Some(arg.Substring(equalsPrefix.Length))

            else
                loop (i + 1)

    loop 0


let private getSpiceFileName (argv : string[]) =
    match tryGetCommandLineValue "--spice" argv with
    | Some path when not (String.IsNullOrWhiteSpace path) ->
        Path.GetFullPath path

    | _ ->
        failwith
            "Missing SPICE meta-kernel path. Start the program with: --spice \"C:\\path\\to\\spice\\kernels\\mk\\hera_ops.tm\""

[<EntryPoint>]
let main args =
    Aardvark.Init()
    Aardium.init()

    let spiceFileName = getSpiceFileName args
    use _ = SPICE.init spiceFileName

    // create the opengl application for rendering
    let app = new Aardvark.Application.Slim.OpenGlApplication()
    // create the media application
    let mediaApp = App.app()

    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start mediaApp)
    ] |> ignore
    
    Aardium.run {
        title "PRo3D.ImageMapping Tool"
        width 1024
        height 768
        debug true
        url "http://localhost:4321/"
    }

    0
