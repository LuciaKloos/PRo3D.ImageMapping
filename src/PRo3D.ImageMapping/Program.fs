open PRo3D.ImageMapping

open Aardium
open Aardvark.UI
open Suave
open Aardvark.Base





[<EntryPoint>]
let main args =
    Aardvark.Init()
    Aardium.init()

    // create the opengl application for rendering
    let app = new Aardvark.Application.Slim.OpenGlApplication()
    // create the media application
    let mediaApp = App.app()

    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start mediaApp)
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1024
        height 768
        debug true
        url "http://localhost:4321/"
    }

    0
