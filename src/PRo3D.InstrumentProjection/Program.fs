
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
open PRo3D.InstrumentProjection


module Time =

    let toUtcFormat (d : DateTime) = 
        d.ToUniversalTime()
         .ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'");

type Font = GoogleFontProvider<"Roboto Mono">
          
type CameraMode =
    | FreeFly
    | Orbit

module InstrumentProjectionViewer = 

    [<EntryPoint>]
    let main argv =

        Aardvark.Init()

        use app = new OpenGlApplication()
        use win = app.CreateGameWindow(8)

        use _ = 
            let logPath = Path.Combine(".", "logs", "CooTrafo.Log")
            Log.line "log path for coo trafo: %s" logPath
            let r = CooTransformation.Init(true, logPath, 10, 10)
            if r <> 0 then failwith "could not initialize CooTransformation lib."
            { new IDisposable with member x.Dispose() = CooTransformation.DeInit() }


        let spiceFileName = Path.GetFullPath(Path.combine [ ".."; ".."; ".."; ".."; "./spice/kernels/mk/hera_ops.tm"])
        System.Environment.CurrentDirectory <- Path.GetFullPath(Path.GetDirectoryName(spiceFileName))

        if not (File.Exists spiceFileName) then
            failwith "spice kernel does not exist."

        let r = CooTransformation.AddSpiceKernel(Path.GetFullPath(spiceFileName))
        if r <> 0 then failwith "could not add spice kernel"


        let observer = cval "MARS" //"HERA_AFC-1" 
        let supportBody = cval "SUN"
        let referenceFrame = cval "ECLIPJ2000"
        let time = 
            let startTime = "2025-03-12 11:50:30.000Z"
            cval (DateTime.Parse(startTime))


        let initialView = (Camera.getLookAt "HERA" observer.Value referenceFrame.Value "SUN" time.Value).Value |> cval
        let speed = 7900.0 * 100.0 |> cval
        let cameraMode = cval CameraMode.Orbit
        let farPlaneMars = 30101626.50 * 1000.0
        let frustum = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 100.0 farPlaneMars (float s.X / float s.Y))

        let view = 
            adaptive {
                let! mode = cameraMode
                let! initialView = initialView
                match mode with
                | CameraMode.FreeFly -> 
                    let! currentSpeed = speed
                    return! 
                        DefaultCameraController.controlExt (float currentSpeed) win.Mouse win.Keyboard win.Time initialView
                | CameraMode.Orbit -> 
                    return!
                        AVal.integrate 
                            initialView win.Time [
                                DefaultCameraController.controlZoomWithSpeed speed win.Mouse
                                DefaultCameraController.controllScrollWithSpeed speed win.Mouse win.Time
                                DefaultCameraController.controlOrbitAround win.Mouse (AVal.constant <| V3d.Zero)
                            ]
            }


        let currentProjectedImageIdx = cval 0

        let instrumentImages = 
            InstrumentMetadata.discoverInstrumentFolder @"C:\pro3ddata\HERA\Workshop2\EOX_PRo3D-GIS_Data\TIFF\Mars-Swing-By\Mars-Swing-By\HSH-1B\1B"
            |> Seq.toArray

        let currentProjectedImage = 
            currentProjectedImageIdx 
            |> AVal.map (fun idx -> 
                if idx < 0 || idx >= instrumentImages.Length then 
                    None
                else
                    Some instrumentImages.[idx]
            )

       


        let scene = 
            Sg.ofList [
                Visualization.createSceneGraph currentProjectedImage referenceFrame supportBody observer time
            ]
            |> Sg.viewTrafo (AVal.map CameraView.viewTrafo view)
            |> Sg.projTrafo (AVal.map Frustum.projTrafo frustum)


        let font = Font.Font
        let aspect = win.Sizes |> AVal.map (fun s -> float s.X / float s.Y)

        let aspectScaling = aspect |> AVal.map (fun aspect -> Trafo3d.Scale(V3d(1.0, aspect, 1.0)))

        let info = 
            let content = time |> AVal.map (fun t -> sprintf "%s" (CooTransformation.Time.toUtcFormat t))
            Sg.text font C4b.Gray content 
            |> Sg.trafo (aspectScaling |> AVal.map (fun s -> Trafo3d.Scale(0.1) * s * Trafo3d.Translation(-0.95,-0.95,0.0)))
     
        let help = 
            let content =
                observer |> AVal.map (fun o -> 
                    String.concat Environment.NewLine [
                        "<c>    : switch camera mode"
                        "<t>    : reset time"
                        $"<n>    : switch observer ({o})."
                    ]
                )
            Sg.text font C4b.Gray content
            |> Sg.trafo (aspectScaling |> AVal.map (fun s -> Trafo3d.Scale(0.02) * s * Trafo3d.Translation(-0.95, 0.90,0.0)))

        let renderTask =
            Sg.ofList [scene; info; help]
            |> Sg.compile win.Runtime win.FramebufferSignature

        let mutable paused = true
        let s = 
            let sw = Diagnostics.Stopwatch.StartNew()
            let mutable lastFrame = None
            win.AfterRender.Add(fun _ -> 
                transact (fun _ -> 

                    let dt = 
                        match lastFrame with
                        | None -> TimeSpan.Zero
                        | Some l -> sw.Elapsed - l
                    if not paused then
                        time.Value <- time.Value + dt * 200.0

                    //let frustum = instruments.["HERA_AFC-1"]
                    //let view = (getLookAt "HERA" observer.Value referenceFrame.Value "SUN" time.Value).Value
                    ////customObservationCamera.Value <- Some (Camera.create view frustum)
                    ////initialView.Value <- view
                    ////animationStep()
                    lastFrame <- Some sw.Elapsed
                )
            )

        win.Keyboard.KeyDown(Keys.Space).Values.Add(fun _ -> 
            paused <- not paused
        )

        win.RenderTask <- renderTask
        win.Run()

        0