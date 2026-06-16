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
open PRo3D.InstrumentVisualization
open Aardvark.GeoSpatial.Opc.Configurations
open Aardvark.GeoSpatial.Opc

module Time =

    let toUtcFormat (d : DateTime) = 
        d.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'")

type Font = GoogleFontProvider<"Roboto Mono">
          
type CameraMode =
    | FreeFly
    | Orbit

module InstrumentProjectionViewer = 

    let tryGetCommandLineArg (name: string) (argv : string[]) =
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
                    Some (arg.Substring(equalsPrefix.Length))
                else
                    loop (i + 1)
        loop 0

    let getSpiceFileName (argv : string[]) =
        match tryGetCommandLineArg "--spice" argv with
        | Some path when not (String.IsNullOrWhiteSpace path) -> 
            Path.GetFullPath path
        | _ ->
            failwith "Missing SPICE meta-kernel path. Start the application with --spice \"path/to/spice/kernels/mk/hera_ops.tm\""
         
    [<EntryPoint>]
    let main argv =

        Aardvark.Base.Report.Verbosity <- 100000
        Aardvark.Init()

        use app = new OpenGlApplication()
        use win = app.CreateGameWindow(8)

        use _ = 
            let logPath = Path.Combine(".", "logs", "CooTrafo.Log")
            Log.line "log path for coo trafo: %s" logPath
            let r = CooTransformation.Init(true, logPath, 10, 10)
            if r <> 0 then failwith "could not initialize CooTransformation lib."
            { new IDisposable with member x.Dispose() = CooTransformation.DeInit() }

        let spiceFileName = getSpiceFileName argv
        if not (File.Exists spiceFileName) then
            failwithf "SPICE meta-kernel file does not exist: %s" spiceFileName

        let spiceDirectory = 
            let dir = Path.GetDirectoryName(spiceFileName)
            if String.IsNullOrWhiteSpace dir then 
                failwithf "Could not determine directory of SPICE meta-kernel file: %s" spiceFileName
            Path.GetFullPath dir 

        System.Environment.CurrentDirectory <- spiceDirectory

        Log.line "Using SPICE meta-kernel: %s" spiceFileName
        Log.line "SPICE working directory: %s" System.Environment.CurrentDirectory

        let r = CooTransformation.AddSpiceKernel spiceFileName
        if r <> 0 then 
            failwithf "could not add SPICE meta-kernel: %s" spiceFileName

        let observer = cval "MARS" //"HERA_AFC-1" 
        let supportBody = cval "SUN"
        let referenceFrame = cval "ECLIPJ2000"
        let referenceFrame = cval "J2000"
        let referenceFrame = cval "IAU_MARS"
        let initialTime = 
            let startTime = "2025-03-12 11:50:30.000Z"
            cval (DateTime.Parse(startTime))


        let initialView = (Camera.getLookAt "HERA" observer.Value referenceFrame.Value "SUN" initialTime.Value).Value |> cval
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



        let instrumentImages, projection, initialImage = 
            let p = {
                        target = InstrumentImages.CameraFocus.FocusBody "MARS"
                        cameraSource =  InstrumentImages.CameraSource.InBody "HERA"
                        instrumentReferenceFrame = "HERA_AFC-1"
                        instrumentName = "HERA_AFC-1"
                        supportBody = "SUN"
                        time = DateTime.Now
                        boresightAdjustment = None
                    }
            let images = 
                InstrumentMetadata.discoverInstrumentFolder @"C:\pro3ddata\HERA\Workshop2\EOX_PRo3D-GIS_Data\TIFF\Mars-Swing-By\Mars-Swing-By\AFC1-1B\1B"
                |> Seq.toArray
            images, p, Some "AF1_0CRS8F_250312T121701_1B_AFC1.tif"


        let instrumentImages, projection, initialImage = 
            let p = {
                        target = InstrumentImages.CameraFocus.FocusBody "MARS"
                        cameraSource =  InstrumentImages.CameraSource.InBody "HERA"
                        instrumentReferenceFrame = "HERA_HSH"
                        instrumentName = "HERA_HSH"
                        supportBody = "SUN"
                        time = DateTime.Now
                        boresightAdjustment = None
                    }
            let images = 
                InstrumentMetadata.discoverInstrumentFolder @"C:\pro3ddata\HERA\Workshop2\EOX_PRo3D-GIS_Data\TIFF\Mars-Swing-By\Mars-Swing-By\HSH-1B\1B"
                |> Seq.toArray
            images, p, Some "HSH_0CRS63_250312T121545_1B_Stacked.tif"


        let currentProjectedImageIdx = 
            match initialImage with
            | None -> cval 0
            | Some img -> 
                let idx = 
                    instrumentImages 
                    |> Array.tryFindIndex (fun (path, _) -> Path.GetFileName(path) = img)
                match idx with
                | Some i -> cval i
                | None -> cval 0

        let currentProjectedImage = 
            currentProjectedImageIdx 
            |> AVal.map (fun idx -> 
                if idx < 0 || idx >= instrumentImages.Length then 
                    None
                else
                    Some instrumentImages.[idx]
            )

        let time = 
            (initialTime, currentProjectedImage)
            ||> AVal.map2 (fun time img ->
                match img with
                | Some (img, (Some mbi, _)) -> mbi.obs_date
                | _ -> time
            )

       
        let minMax = 
            currentProjectedImage |> AVal.map (fun img -> 
                match img with
                | Some (_, (_, Some imgMeta) : ParsedMetadata) -> 
                    Range1d(imgMeta.image_statistics[0].minimum, imgMeta.image_statistics[0].maximum)
                | _ -> 
                    Range1d.Unit
            )

        let projectionOpacity = 
            cval 1.0

        let showProxy = cval true

        let colorMap = InstrumentImageVisualization.getColorMapTexture "magma.png" |> Some |> AVal.constant
        let heightColorMap = InstrumentImageVisualization.getColorMapTexture "coolwarm.png" |> AVal.constant

        let imageSettings = 
            { 
                VisualizationProperties.empty with 
                    projectionOpacity = projectionOpacity
                    visualizationRange = minMax
                    colorMapping = colorMap
            }

        let projectImage = Visualization.creatProjectionFunction observer time referenceFrame currentProjectedImage (AVal.constant projection)
        let projectedTexture = Visualization.createProjectedTexture currentProjectedImage (AVal.constant { idx = 0; name = None})

        // The standalone InstrumentProjection viewer still uses one band.
        // Repeat it across R, G, and B to preserve a grayscale result.
        let projectedMin =
            minMax
            |> AVal.map (fun range -> range.Min)

        let projectedMax =
            minMax
            |> AVal.map (fun range -> range.Max)

        // UInt16 = 1 in the RGB normalization shader.
        let projectedDataType =
            AVal.constant 1

        // Keep the 2x2 RGB diagnostic disabled in this legacy viewer.
        let rgbProjectionDebug =
            AVal.constant false

        let opc =
            let molaOpcs =
                Seq.delay (fun _ -> 
                    System.IO.Directory.GetDirectories(@"C:\pro3ddata\MOLA") 
                )
            let mola =
                { 
                    useCompressedTextures = true
                    preTransform     = Trafo3d.Identity
                    patchHierarchies = molaOpcs
                    boundingBox      = Box3d.Parse("[[1042657.138109462, 3023778.035968372, -472791.711967824], [1492041.915577915, 3230435.734121298, -231.611523378]]") 
                    near             = 0.1
                    far              = 10000.0
                    speed            = 5.0
                    lodDecider       =  DefaultMetrics.mars2 
                }
            let currentProjection = projectImage "MARS"
            MarsSurface.getMarsSurfaceSg win.Runtime win.FramebufferSignature mola imageSettings currentProjection referenceFrame supportBody observer time projectImage projectedTexture heightColorMap


        let scene = 
            Sg.ofList [
                (Visualization.createSceneGraph
                    imageSettings
                    referenceFrame
                    supportBody
                    observer
                    time
                    projectImage
                    projectedTexture

                    // Projection enabled
                    (AVal.constant true)
                |> Sg.onOff showProxy);
                opc |> Sg.onOff (AVal.map not showProxy)
            ]
            |> Sg.viewTrafo (AVal.map CameraView.viewTrafo view)
            |> Sg.projTrafo (AVal.map Frustum.projTrafo frustum)


        let font = Font.Font
        let aspect = win.Sizes |> AVal.map (fun s -> float s.X / float s.Y)

        let aspectScaling = aspect |> AVal.map (fun aspect -> Trafo3d.Scale(V3d(1.0, aspect, 1.0)))

        let info = 
            // format time without converting to UTC (keep original local/represented time)
            let content = time |> AVal.map (fun t -> sprintf "%s" (t.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'")))
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
                        initialTime.Value <- initialTime.Value + dt * 200.0

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

        win.Keyboard.KeyDown(Keys.N).Values.Add(fun _ -> 
            transact (fun _ -> 
                currentProjectedImageIdx.Value <- (currentProjectedImageIdx.Value + 1) % instrumentImages.Length
            )
        )

        win.Keyboard.KeyDown(Keys.OemPlus).Values.Add(fun _ -> 
            transact (fun _ -> 
                projectionOpacity.Value <- clamp 0.0 1.0 (projectionOpacity.Value + 0.1)
                Log.line "opacity: %A" projectionOpacity.Value
            )
        )

        win.Keyboard.KeyDown(Keys.OemMinus).Values.Add(fun _ -> 
            transact (fun _ -> 
                projectionOpacity.Value <- clamp 0.0 1.0 (projectionOpacity.Value - 0.1)
                Log.line "opacity: %A" projectionOpacity.Value
            )
        )

        win.Keyboard.KeyDown(Keys.P).Values.Add(fun _ -> 
            transact (fun _ -> 
                showProxy.Value <- not showProxy.Value
            )
        )

        win.RenderTask <- renderTask
        win.Run()

        0