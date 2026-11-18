namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.SceneGraph 
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO

open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE

open PRo3D.ImageMapping.ImageDefaults
open PRo3D.ImageMapping.NetCdfLoader
open PRo3D.ImageMapping.MbiLoader
open PRo3D.ImageMapping.TiffLoader
open PRo3D.ImageMapping.RgbComposite
open PRo3D.ImageMapping.BandHandler

module CoreSg = Aardvark.SceneGraph.SgFSharp.Sg

module Image =

    let loadDataset (path : string) : list<Image> =
        match tryResolveNcPathToLoad path with
        | Some ncPath ->
            loadNcBands ncPath

        | None ->
            match tryReadMbiBands path with
            | Some _ ->
                loadMbiBands path

            | None ->
                loadTiffBands path

    let createBandRatioTexture
        (images : alist<AdaptiveImage>)
        (bandRatioRenderSettings : BandRatioRenderSettings)
        : aval<ITexture> =

        let adaptiveImages =
            AList.toAVal images

        AVal.custom (fun token ->

            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            let redExpression = bandRatioRenderSettings.redExpression.GetValue token
            let greenExpression = bandRatioRenderSettings.greenExpression.GetValue token
            let blueExpression = bandRatioRenderSettings.blueExpression.GetValue token

            let gammaValue =
                bandRatioRenderSettings.gamma.GetValue token

            match
                sources,
                redExpression,
                greenExpression,
                blueExpression
            with
            | [], _, _, _ ->
                DefaultTextures.checkerboard.GetValue()

            | _,
              Some redExpression,
              Some greenExpression,
              Some blueExpression
              ->

                match
                    createRgbRatioCompositeFromExpressions
                        sources
                        (redExpression, greenExpression, blueExpression)
                        gammaValue
                with
                | Result.Ok image ->
                    PixTexture2d(
                        PixImageMipMap [|
                            image :> PixImage
                        |],
                        false
                    ) :> ITexture

                | Result.Error error ->
                    Log.warn
                        "Could not create RGB composite: %s"
                        error

                    DefaultTextures.checkerboard.GetValue()

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    let createRawGreyscaleTexture
        (sourcePath : aval<Option<string>>)
        : aval<ITexture> =

        AVal.custom (fun token ->
            match sourcePath.GetValue token with
            | Some path when File.Exists path ->
                let source =
                    PixImage<byte>(path).ToPixImage<byte>(Col.Format.Gray)
                let input = source.GetMatrix<byte>()
                let output = PixImage<float32>(Col.Format.Gray, source.Size)
                let mutable pixels = output.GetMatrix<float32>()

                for y in 0 .. source.Size.Y - 1 do
                    for x in 0 .. source.Size.X - 1 do
                        pixels.[x, y] <- float32 input.[x, y] / 255.0f

                PixTexture2d(
                    PixImageMipMap [| output :> PixImage |],
                    false
                ) :> ITexture

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    let createGreyscaleColormapTexture
        (colorMap : aval<ColorMap>)
        : aval<ITexture> =

        AVal.custom (fun token ->
            let selectedMap = colorMap.GetValue token
            let palette = PixImage<byte>(Col.Format.RGBA, V2i(256, 1))
            let mutable  pixels = palette.GetMatrix<C4b>()

            for x in 0 .. 255 do
                pixels.[x, 0] <-
                    RgbComposite.sampleColorMap selectedMap (float x / 255.0)

            PixTexture2d(
                PixImageMipMap [| palette :> PixImage |],
                false
            ) :> ITexture
        )
            
    let createSplineTexture
        (spline : aval<TransferSpline>)
        (samples : int)
        : aval<ITexture> =

        spline
        |> AVal.map (fun s ->
            let image = PixImage<float32>(Col.Format.Gray, V2i(samples, 1))
            let data = image.Volume.Data

            for i in 0 .. samples - 1 do
                let x = float i / float (samples - 1)
                data.[i] <- float32 (TransferSpline.evaluate s x)

            PixTexture2d(PixImageMipMap [| image :> PixImage |], false) :> ITexture)

    // Read the selected band into a raw-value texture.
    let createSelectedBandTexture
        (images : alist<AdaptiveImage>)
        (transferFunctionRenderSettings : TransferFunctionRenderSettings)
        : aval<ITexture> =

        let adaptiveImages = AList.toAVal images

        AVal.custom (fun token ->
            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            match transferFunctionRenderSettings.selectedBands.GetValue token with
            | [] ->
                DefaultTextures.checkerboard.GetValue()

            | selectedBands ->
                Log.warn "TF texture from bands %A" selectedBands
                match readAveragedBandPayload sources selectedBands with
                | Result.Error error ->
                    Log.warn "Could not load selected band texture: %s" error
                    DefaultTextures.checkerboard.GetValue()

                | Result.Ok band ->
                    let image =
                        PixImage<float32>(
                            Col.Format.Gray,
                            V2i(band.width, band.height)
                        )

                    let data = image.Volume.Data
                    let v = band.values

                    for i in 0 .. v.Length - 1 do
                        data.[i] <- if Double.IsFinite v.[i] then float32 v.[i] else 0.0f

                    PixTexture2d(
                        PixImageMipMap [| image :> PixImage |],
                        false
                    ) :> ITexture
        )

    // makes a horizontal lookup texture from the selected colormap
    // Create a 256 × 1 lookup texture for the selected band's colormap.
    let createColormapTexture
        (images : alist<AdaptiveImage>)
        (transferFunctionRenderSettings : TransferFunctionRenderSettings)
        : aval<ITexture> =

        let adaptiveImages = AList.toAVal images

        AVal.custom (fun token ->
            let selectedBands =
                transferFunctionRenderSettings.selectedBands.GetValue token

            let selectedImage = tryFindReferenceImage (adaptiveImages.GetValue token) token selectedBands

            match selectedImage with
            | None  ->
                DefaultTextures.checkerboard.GetValue()

            | Some image ->
                let colorMap = image.colorMap.GetValue token
                let palette = PixImage<byte>(Col.Format.RGBA, V2i(256, 1))
                let mutable pixels = palette.GetMatrix<C4b>()

                for x in 0 .. 255 do
                    let t = float x / 255.0
                    pixels.[x, 0] <- RgbComposite.sampleColorMap colorMap t

                PixTexture2d(
                    PixImageMipMap [| palette :> PixImage |],
                    false
                ) :> ITexture
        )

    let createStretchedGreyscaleTexture
        (sourcePath : aval<Option<string>>)
        (blackPoint : aval<float>)
        (whitePoint : aval<float>)
        (applyColormap : aval<bool>)
        (colormap : aval<ColorMap>)
        : aval<ITexture> =

        AVal.custom (fun token ->
            match sourcePath.GetValue token with
            | Some path when File.Exists path ->
                let source = PixImage<byte>(path)
                let input = source.GetMatrix<byte>()
                let output = PixImage<byte>(Col.Format.RGBA, source.Size)
                let mutable pixels = output.GetMatrix<C4b>() 

                let black = blackPoint.GetValue token
                let white = max (black + 1.0) (whitePoint.GetValue token)

                let applyColormap = applyColormap.GetValue token
                let selectedColorMap = colormap.GetValue token

                for y in 0 .. source.Size.Y - 1 do
                    for x in 0 .. source.Size.X - 1 do
                        let grey = float input.[x, y]
                        let stretched =
                            255.0 * ImageMath.clamp01 ((grey - black) / (white - black))
                            |> round
                            |> byte

                        pixels.[x, y] <- 
                            if applyColormap then
                                RgbComposite.sampleColorMap
                                    selectedColorMap
                                    (float stretched / 255.0)
                            else
                                C4b(stretched, stretched, stretched, 255uy)

                PixTexture2d(
                    PixImageMipMap [| output :> PixImage |],
                    false
                ) :> ITexture

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    let fullscreenQuadGeometry =
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray =
                ([| 0; 1; 2; 0; 2; 3 |] :> Array),
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions,
                    ([|
                        V3f(-1.0f, -1.0f, 0.0f)
                        V3f( 1.0f, -1.0f, 0.0f)
                        V3f( 1.0f,  1.0f, 0.0f)
                        V3f(-1.0f,  1.0f, 0.0f)
                    |] :> Array)

                    DefaultSemantic.DiffuseColorCoordinates,
                    ([|
                        V2f(0.0f, 0.0f)
                        V2f(1.0f, 0.0f)
                        V2f(1.0f, 1.0f)
                        V2f(0.0f, 1.0f)
                    |] :> Array)
                ]
        )

    

    // the 2D view displays the texture directly
    let createInstrumentScene
        (rgbTexture : aval<ITexture>)
        (adjustedGpuTexture : aval<ITexture>)
        (bandTexture : aval<ITexture>)
        (colormapTexture : aval<ITexture>) 
        (splineTexture : aval<ITexture>)
        (gpuSettings : aval<float * float * bool * float * float * float>) 
        (useGpu : aval<bool>) 
        (clickedPixel : aval<Option<V2i>>)
        (imageWidth : aval<int>)
        (imageHeight : aval<int>)
        (useTransferFunctionGpu : aval<bool>)=
        
        Log.warn "createInstrumentScene called"
        
        let showPixelMarker =
            (clickedPixel, imageWidth, imageHeight)
            |||> AVal.map3 (fun clickedPixel width height ->
                clickedPixel.IsSome && width > 0 && height > 0
            )

        let pixelMarkerBounds =
            (clickedPixel, imageWidth, imageHeight)
            |||> AVal.map3 (fun clickedPixel width height ->
                match clickedPixel with
                | Some pixel when width > 0 && height > 0 ->
                    let markerRadius = 2.0

                    let minX =
                        (float pixel.X - markerRadius) / float width

                    let maxX =
                        (float pixel.X + markerRadius) / float width

                    let minY =
                        1.0 - (float pixel.Y + markerRadius) / float height

                    let maxY =
                        1.0 - (float pixel.Y - markerRadius) / float height

                    V2d(minX, minY), V2d(maxX, maxY)

                | _ ->
                    V2d.Zero, V2d.Zero
            )

        let pixelMarkerMin =
            pixelMarkerBounds |> AVal.map fst

        let pixelMarkerMax =
            pixelMarkerBounds |> AVal.map snd

        let baseSg = Sg.ofIndexedGeometry fullscreenQuadGeometry

        let rgbSg =
            baseSg
            |> Sg.texture "RgbCompositeTexture" rgbTexture
            |> Sg.uniform "ShowPixelMarker" showPixelMarker
            |> Sg.uniform "PixelMarkerMin" pixelMarkerMin
            |> Sg.uniform "PixelMarkerMax" pixelMarkerMax
            |> Sg.shader {
                do! Shaders.displayRgbComposite
            }

        let minValue = gpuSettings |> AVal.map (fun (minimum, _, _, _, _, _) -> minimum)
        let maxValue = gpuSettings |> AVal.map (fun (_, maximum, _, _, _, _) -> maximum)
        let shaderFalseColor = gpuSettings |> AVal.map (fun (_, _, flag, _, _, _) -> flag)
       
        let transferFunctionSg =
            baseSg
            |> Sg.texture "InstrumentImage" bandTexture
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.texture "SplineTexture" splineTexture
            |> Sg.uniform "MinValue" minValue
            |> Sg.uniform "MaxValue" maxValue
            |> Sg.uniform "UseFalseColor" shaderFalseColor
            |> Sg.uniform "DataType" (AVal.constant 2)
            |> Sg.shader {
                do! Shaders.hshColorsTF
            }

        // shows the already-adjusted GPU result (+ pixel marker)
        let adjustmentSg =
            baseSg
            |> Sg.texture "RgbCompositeTexture" adjustedGpuTexture
            |> Sg.uniform "ShowPixelMarker" showPixelMarker
            |> Sg.uniform "PixelMarkerMin" pixelMarkerMin
            |> Sg.uniform "PixelMarkerMax" pixelMarkerMax
            |> Sg.shader {
                do! Shaders.displayRgbComposite
            }

        let gpuSg =
            useTransferFunctionGpu
            |> AVal.map (fun enabled ->
                if enabled then transferFunctionSg
                else adjustmentSg
            )
            |> Sg.dynamic                

        let selectedSg =
            useGpu
            |> AVal.map (fun enabled -> if enabled then gpuSg else rgbSg)
            |> Sg.dynamic

        let imageSg =            
            selectedSg
            |> Sg.requirePicking
            |> Sg.withEvents [
                SceneEventKind.Click,
                (fun (hit : SceneHit) ->
                    let position = hit.globalPosition
                    let viewportSize = hit.event.evtViewport

                    Log.warn "Clicked at position: %A, viewport: %A" position viewportSize

                    false, Seq.singleton (Message.ImageClicked (position, viewportSize))
                )
            ]

        Sg.ofList [
            imageSg
        ]

    let update (m : Image) (msg : ImageMessage) =
        match msg with
            | SetDataTypeAndRange (dataType, min, max) ->
                { m with inputMinValue = { minValue with min = min}; inputMaxValue = {minValue with max = max} }
            | SetCustomMin v -> 
                { m with inputMinValue = {minValue with value = v} }
            | SetCustomMax v -> 
                { m with inputMaxValue = {maxValue with value = v} }
            | ResetCustomMinMax ->
                { m with inputMinValue = {minValue with value = m.defaultMinValues[m.selectedChannel.idx]}; inputMaxValue = {maxValue with value = m.defaultMaxValues[m.selectedChannel.idx]} }
            | SetColorMap (map : ColorMap) ->
                { m with colorMap = map }
            | SetEXRChannel channel ->
                let (min, max) = (m.defaultMinValues[channel.idx], m.defaultMaxValues[channel.idx])
                { m with 
                    inputMinValue = {minValue with value = min};
                    inputMaxValue = {maxValue with value = max};
                    selectedChannel = channel
                }
            | ToggleFalseColor ->
                { m with useFalseColor = not m.useFalseColor }
            | ImageMessage.Empty ->
                m

    let whitePix =
        let pi = PixImage<byte>(Col.Format.RGBA, V2i.II)
        pi.GetMatrix<C4b>().SetByCoord(fun (c : V2l) -> C4b.White) |> ignore
        pi

    let whiteTex =
        PixTexture2d(PixImageMipMap [| whitePix :> PixImage |], false) :> ITexture

    let colorMapDataUrl (colorMap : ColorMap) =
        let fileName = ColorMap.getColorMapFileName colorMap

        let resource =
            AppDomain.CurrentDomain.GetAssemblies()
            |> Array.tryPick (fun assembly ->
                assembly.GetManifestResourceNames()
                |> Array.tryFind (fun name ->
                    name.EndsWith(fileName, StringComparison.OrdinalIgnoreCase)
                )
                |> Option.map (fun resourceName ->
                    assembly, resourceName
                )
            )

        match resource with
        | Some (assembly, resourceName) ->
            use stream = assembly.GetManifestResourceStream(resourceName)
            use memory = new MemoryStream()

            stream.CopyTo(memory)

            sprintf
                "data:image/png;base64,%s"
                (Convert.ToBase64String(memory.ToArray()))

        | None ->
            ""

    let view (m : AdaptiveImage) =
        let content = 
            Html.table [ 
                Html.row "False Color:" [
                    
                    Html.SemUi.dropDown m.colorMap SetColorMap
                ]
            ]

        let transferFunctionPreview =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! active = m.useFalseColor
                        let! colorMap = m.colorMap

                        if active then
                            yield
                                img [
                                    attribute "src" (colorMapDataUrl colorMap)
                                    style "display: block; width: 100%; height: 28px; margin-top: 10px;"
                                ]
                    }
                )

        require Html.semui (
            div [] [
                div [style "position: relative; paddingLeft: 25px; paddingTop: 25px; width: 100%"] [
                    content
                    transferFunctionPreview
                ]
            ]
        )

    let view2DAnd3DImageAbsolute
        (opacity : aval<float>)
        (boresightAdjustment : aval<Option<Trafo3d>>)
        (orbitState : AdaptiveOrbitState)
        (sourceImagePath : aval<Option<string>>)
        (rgbTexture : aval<ITexture>)
        (adjustedDisplayTexture : aval<ITexture>)
        (bandTexture : aval<ITexture>)
        (colormapTexture : aval<ITexture>)
        (splineTexture : aval<ITexture>)
        (gpuSettings : aval<float * float * bool * float * float * float>)
        (useGpu : aval<bool>)
        clickedPixel
        imageWidth
        imageHeight 
        (useTransferFunctionGpu  : aval<bool>) =

        let instrumentVisualization =
            createInstrumentScene
                rgbTexture
                adjustedDisplayTexture
                bandTexture
                colormapTexture
                splineTexture
                gpuSettings
                useGpu
                clickedPixel
                imageWidth
                imageHeight
                useTransferFunctionGpu 

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum2D = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))
        let farPlaneMars = 30101626.50 * 1000.0
        let frustum = Frustum.perspective 80.0 10.0 farPlaneMars 1.0 |> AVal.constant

        let observer = cval "MARS" //"HERA_AFC-1" 
        let supportBody = cval "SUN"
        let referenceFrame = cval "ECLIPJ2000"
        let referenceFrame = cval "IAU_MARS"

      
        let currentProjectedImage =
            sourceImagePath
            |> AVal.map (function
                | Some path when File.Exists path ->
                    Some (
                        path,
                        InstrumentMetadata.tryParseMetadataForImagePath path
                    )

                | _ ->
                    None
            )            

        let projectionSetup = 
            // instrument projection
            let p : InstrumentProjection = {
                target = InstrumentImages.CameraFocus.FocusBody "MARS"
                cameraSource = InstrumentImages.CameraSource.InBody "HERA"
                instrumentReferenceFrame = "HERA_AFC-1"
                instrumentName = "HERA_AFC-1"
                supportBody = "SUN"
                time = DateTime.Now
                boresightAdjustment = None
            }

            (currentProjectedImage, boresightAdjustment)
            ||> AVal.map2 (fun currentProjectedImage boresight -> 
                match currentProjectedImage with
                | Some (_, (Some mbi, _)) -> 
                    // update using selected image metadata
                    let instrumentName =
                        match InstrumentProjection.instrument2SpiceName mbi.instrument with
                        | Some name ->
                            name
                        | None ->
                            failwith "no spice name for the given instrument."

                    let p = 
                        {
                            p with
                                time = mbi.obs_date
                                instrumentName = instrumentName
                                instrumentReferenceFrame = "J2000"
                                boresightAdjustment = boresight
                        }

                    p, mbi.obs_date

                | _ ->
                    Log.warn
                        "Could not access observation time from selected image metadata. Projection time was not updated. Current fallback value is: %A"
                        p.time

                    p, p.time
            )

        let projection =
            projectionSetup |> AVal.map fst

        let time =
            projectionSetup |> AVal.map snd
            
        let primaryProjectionEnabled =
            currentProjectedImage
            |> AVal.map (function
                | Some (_, (Some _, _)) -> true
                | _ -> false
            )

        require Html.semui (
                div [] [
                    div [] [
                        // the 2D control
                        let leftControl = [style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"; attribute "showLoader" "false"]
                        renderControl (AVal.constant (Camera.create cameraView frustum2D)) leftControl instrumentVisualization
                    
                    ]
                ]
        )

    // --- RGB IMAGES: clipped rgb image -> shadows/highlights mask -> box blur -> adjustments (brightness, saturation, mid-contrast) ---

    let clipPassTexture
        (runtime : IRuntime)
        (sourceTexture : aval<ITexture>)
        (clipMin : aval<V3d>)
        (clipMax : aval<V3d>)
        (size : aval<V2i>)
        : aval<IBackendTexture> =

        let clipSg =
            CoreSg.ofIndexedGeometry fullscreenQuadGeometry
            |> CoreSg.texture "SourceTexture" sourceTexture
            |> CoreSg.uniform "ClipMin" clipMin
            |> CoreSg.uniform "ClipMax" clipMax
            |> CoreSg.shader {
                do! Shaders.clipPass
            }

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8 // TODO: maybe RGBA16F for precision?
            ]

        RenderTask.renderToColor size (runtime.CompileRender(signature, clipSg))

    let createShadowsHighlightsMaskTexture 
        (runtime : IRuntime)
        (rgbTexture : aval<ITexture>)
        (highlightTone : aval<float>)
        (shadowsTone : aval<float>)
        (imageWidth : aval<int>)
        (imageHeight : aval<int>) 
        : aval<IBackendTexture> = 

        let size =
            (imageWidth, imageHeight)
            ||> AVal.map2 (fun w h -> V2i(max 1 w, max 1 h))

        let runtime = runtime  
        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
            ]

        Log.warn "createShadowsHighlightsMaskTexture called with highlightTone=%f, shadowsTone=%f" (highlightTone.GetValue()) (shadowsTone.GetValue())
        let maskSg =
            CoreSg.ofIndexedGeometry fullscreenQuadGeometry
            |> CoreSg.texture "RgbCompositeTexture" rgbTexture
            |> CoreSg.uniform "HighlightTone" highlightTone
            |> CoreSg.uniform "ShadowTone" shadowsTone
            |> CoreSg.shader {
                do! Shaders.hshShadowsHighlights
            }

        let task = runtime.CompileRender(signature, maskSg)

        RenderTask.renderToColor size task 

    let horizontalBlurPass 
        (runtime : IRuntime)
        (shadowsHighlightsMaskTexture : aval<IBackendTexture>)
        (shadowRadius : aval<float>)
        (highlightRadius : aval<float>)
        (size : aval<V2i>)
        : aval<IBackendTexture> =

        let direction = V2d(1.0, 0.0)
        let horizontalBlurSg =
            CoreSg.ofIndexedGeometry fullscreenQuadGeometry
            |> CoreSg.texture "ShadowsHighlightsMaskTexture" (shadowsHighlightsMaskTexture)
            |> CoreSg.uniform "BlurTextureSize" size
            |> CoreSg.uniform "BlurShadowRadius" shadowRadius
            |> CoreSg.uniform "BlurHighlightRadius" highlightRadius
            |> CoreSg.uniform "BlurDirection" (AVal.constant direction)
            |> CoreSg.shader {
                do! Shaders.boxBlur
            }
            
        let runtime = runtime  
        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
            ]

        let task = runtime.CompileRender(signature, horizontalBlurSg)

        RenderTask.renderToColor size task

    let verticalBlurPass 
        (runtime : IRuntime)
        (maskTextureFromFirstBlur : aval<IBackendTexture>)
        (shadowRadius : aval<float>)
        (highlightRadius : aval<float>)
        (size : aval<V2i>)
        : aval<IBackendTexture> =

        let direction = V2d(0.0, 1.0)

        let verticalBlurSg =
            CoreSg.ofIndexedGeometry fullscreenQuadGeometry
            |> CoreSg.texture "ShadowsHighlightsMaskTexture" (maskTextureFromFirstBlur)
            |> CoreSg.uniform "BlurTextureSize" size
            |> CoreSg.uniform "BlurShadowRadius" shadowRadius
            |> CoreSg.uniform "BlurHighlightRadius" highlightRadius
            |> CoreSg.uniform "BlurDirection" (AVal.constant direction)
            |> CoreSg.shader {
                do! Shaders.boxBlur
            }
            
        let runtime = runtime  
        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
            ]

        let task = runtime.CompileRender(signature, verticalBlurSg)

        RenderTask.renderToColor size task

    let applyGpuChangesToRgbImage
        (runtime : IRuntime)
        (imageWidth : aval<int>)
        (imageHeight : aval<int>)
        (rgbTexture : aval<ITexture>)
        (rgbImageGpuSettings : RgbImageGpuSettings)
        : aval<IBackendTexture> =
        
        let size = (imageWidth, imageHeight) ||> AVal.map2 (fun w h -> V2i(max 1 w, max 1 h))
        let clipped =
            clipPassTexture runtime rgbTexture rgbImageGpuSettings.clipMin rgbImageGpuSettings.clipMax size
            |> AVal.map (fun t -> t :> ITexture)

        let mask = createShadowsHighlightsMaskTexture runtime clipped rgbImageGpuSettings.highlightsTone rgbImageGpuSettings.shadowsRadius imageWidth imageHeight
        let hBlur = horizontalBlurPass runtime mask rgbImageGpuSettings.shadowsRadius rgbImageGpuSettings.highlightsRadius size
        let blurredMaskTexture = verticalBlurPass runtime hBlur rgbImageGpuSettings.shadowsRadius rgbImageGpuSettings.highlightsRadius size

        let shaderBrightness = rgbImageGpuSettings.brightnessGain
        let shaderMidContrast = rgbImageGpuSettings.midtoneContrastGain
        let shaderSaturation = rgbImageGpuSettings.saturationGain
        let highlightAmount = rgbImageGpuSettings.highlightsAmount
        let highlightTone = rgbImageGpuSettings.highlightsTone
        let shadowAmount = rgbImageGpuSettings.shadowsAmount
        let shadowTone = rgbImageGpuSettings.shadowsTone

        let adjustmentSg =
            CoreSg.ofIndexedGeometry fullscreenQuadGeometry
            |> CoreSg.texture "RgbCompositeTexture" clipped
            |> CoreSg.uniform "Brightness" shaderBrightness
            |> CoreSg.uniform "Midpoint" (AVal.constant Midtone.init.mid)
            |> CoreSg.uniform "MidtoneContrastAdjustment" shaderMidContrast
            |> CoreSg.uniform "Saturation" shaderSaturation
            |> CoreSg.uniform "HighlightAmount" highlightAmount
            |> CoreSg.uniform "HighlightTone" highlightTone
            |> CoreSg.uniform "ShadowAmount" shadowAmount
            |> CoreSg.uniform "ShadowTone" shadowTone
            |> CoreSg.texture "ShadowsHighlightsMaskTexture" blurredMaskTexture
            |> CoreSg.uniform "MidtoneLow" (AVal.constant Midtone.init.low)
            |> CoreSg.uniform "MidtoneHigh" (AVal.constant Midtone.init.high)
            |> CoreSg.uniform "MidtoneContrastAdjustment" shaderMidContrast
            |> CoreSg.shader {
                do! Shaders.hshColorsAdjustment
            }

        let runtime = runtime  
        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
            ]

        let task = runtime.CompileRender(signature, adjustmentSg)

        RenderTask.renderToColor size task

    // Downloads the GPU result so the CPU histogram can read it.
    let downloadAdjustedImage
        (runtime : IRuntime)
        (adjustedTexture : aval<IBackendTexture>)
        : aval<Result<PixImage<byte>, string>> =

        AVal.custom (fun token ->
            let tex = adjustedTexture.GetValue token
            try
                let img = runtime.Download(tex, 0, 0)
                Result.Ok (img.ToPixImage<byte>().ToPixImage<byte>(Col.Format.RGBA))
            with e ->
                Result.Error e.Message)



    // --- ---

    
    // Headless rendering (--output, synthetic tests): runs the full GPU pipeline with the
    // test parameters and returns the RGBA result. Other adjustments use their slider defaults.
    let renderAdjustedImage (runtime : IRuntime) (config : TestConfig) : PixImage<byte> =
        
        let fail msg = failwithf "Could not load %s: %s" config.imagePath msg

        let payload = 
            BandHandler.readCachedRgbImagePayload config.imagePath
            |> Result.defaultWith fail

        // raw, unclipped source texture
        let sourceRgbTexture =
            RgbComposite.createSourceRgbTexture (AVal.constant (Some config.imagePath))

        // clip range from the histogram, with the default clip percentiles 
        let histogram = 
            RgbComposite.loadRgbHistogram config.imagePath
            |> Result.defaultWith fail

        let clipMinValue, clipMaxValue =
            RgbComposite.computeClipRange
                histogram
                BlackWhiteClip.init.blackClipPercentile.value
                BlackWhiteClip.init.whiteClipPercentile.value
                
        let clipMin = AVal.constant clipMinValue
        let clipMax = AVal.constant clipMaxValue

        // <blackpoint, whitepoint, multispectral flag, brightness, midgain, saturation>
        let gpuSettings =
            AVal.constant (
                0.0, 1.0, false,
                Brightness.init.gainFactor.value,
                MidtoneContrastAdjustment.init.gainFactor.value,
                Saturation.init.gainFactor.value)

        let shSettings =
            AVal.constant (
                config.highlightAmount, config.highlightTone, config.highlightRadius,
                config.shadowAmount, config.shadowTone, config.shadowRadius)

        let rgbImageGpuSettings =
            {
                clipMin = clipMin
                clipMax = clipMax
                highlightsAmount = AVal.constant config.highlightAmount
                highlightsTone = AVal.constant config.highlightTone
                highlightsRadius = AVal.constant config.highlightRadius
                shadowsAmount = AVal.constant config.shadowAmount
                shadowsTone = AVal.constant config.shadowTone
                shadowsRadius = AVal.constant config.shadowRadius
                brightnessGain = AVal.constant Brightness.init.gainFactor.value
                midtoneContrastGain = AVal.constant MidtoneContrastAdjustment.init.gainFactor.value
                saturationGain = AVal.constant Saturation.init.gainFactor.value
            }

        let adjustedTexture =
            applyGpuChangesToRgbImage
                runtime
                (AVal.constant payload.width)
                (AVal.constant payload.height)
                sourceRgbTexture
                rgbImageGpuSettings

            //createGpuAdjustedRgbImageTexture
            //    runtime
            //    sourceRgbTexture
            //    gpuSettings
            //    shSettings
            //    (AVal.constant payload.width)
            //    (AVal.constant payload.height)
            //    clipMin
            //    clipMax

        let texture = AVal.force adjustedTexture                // runs all passes
        runtime.Download(texture, 0, 0).ToPixImage<byte>().ToPixImage<byte>(Col.Format.RGBA)

    let saveAdjustedImage (runtime : IRuntime) (config : TestConfig) (outputPath : string) =
        let image = renderAdjustedImage runtime config
        Directory.CreateDirectory(Path.GetDirectoryName outputPath) |> ignore
        image.SaveAsPng outputPath