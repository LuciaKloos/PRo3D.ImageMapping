namespace PRo3D.ImageMapping

open System
open Aardvark.Base
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
        (shadowsHighlightsAdjustmentsRenderSettings : ShadowsHighlightsAdjustmentsRenderSettings)
        : aval<ITexture> =

        let adaptiveImages =
            AList.toAVal images

        AVal.custom (fun token ->

            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            let redNumeratorValue =
                bandRatioRenderSettings.redNumeratorBand.GetValue token

            let redDenominatorValue =
                bandRatioRenderSettings.redDenominatorBand.GetValue token

            let greenNumeratorValue =
                bandRatioRenderSettings.greenNumeratorBand.GetValue token

            let greenDenominatorValue =
                bandRatioRenderSettings.greenDenominatorBand.GetValue token

            let blueNumeratorValue =
                bandRatioRenderSettings.blueNumeratorBand.GetValue token

            let blueDenominatorValue =
                bandRatioRenderSettings.blueDenominatorBand.GetValue token

            let gammaValue =
                bandRatioRenderSettings.gamma.GetValue token

            let highlightAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.highlightAdjustments.GetValue token

            let highlightAmountValue =
                highlightAdjustmentValue.amount.value

            let highlightToneValue =
                highlightAdjustmentValue.tone.value

            let highlightRadiusValue =
                highlightAdjustmentValue.radius.value

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token

            let shadowAmountValue =
                shadowAdjustmentValue.amount.value

            let shadowToneValue =
                shadowAdjustmentValue.tone.value
                
            let shadowRadiusValue =
                shadowAdjustmentValue.radius.value

            let midtoneContrastValue =
                shadowsHighlightsAdjustmentsRenderSettings.midtoneContrast.GetValue token

            let midtoneContrastGainFactorValue =
                midtoneContrastValue.gainFactor.value

            let blackWhiteClipValue =
                shadowsHighlightsAdjustmentsRenderSettings.blackWhiteClip.GetValue token

            let blackClipPercentileValue =
                blackWhiteClipValue.blackClipPercentile.value

            let whiteClipPercentileValue = 
                blackWhiteClipValue.whiteClipPercentile.value

            let saturationValue = 
                shadowsHighlightsAdjustmentsRenderSettings.saturation.GetValue token

            let saturationGainFactorValue =
                saturationValue.gainFactor.value

            let brightnessValue = 
                shadowsHighlightsAdjustmentsRenderSettings.brightness.GetValue token

            let brightnessGainFactorValue =
                brightnessValue.gainFactor.value

            match
                sources,
                redNumeratorValue,
                redDenominatorValue,
                greenNumeratorValue,
                greenDenominatorValue,
                blueNumeratorValue,
                blueDenominatorValue
            with
            | [], _, _, _, _, _, _ ->
                DefaultTextures.checkerboard.GetValue()

            | _,
              Some redNumerator,
              Some redDenominator,
              Some greenNumerator,
              Some greenDenominator,
              Some blueNumerator,
              Some blueDenominator ->

                match
                    createRgbRatioCompositePixImageFromSources
                        sources
                        redNumerator
                        redDenominatorValue
                        greenNumerator
                        greenDenominatorValue
                        blueNumerator
                        blueDenominatorValue
                        gammaValue
                        highlightAmountValue
                        highlightToneValue
                        highlightRadiusValue
                        shadowAmountValue
                        shadowToneValue                       
                        shadowRadiusValue
                        midtoneContrastGainFactorValue
                        blackClipPercentileValue
                        whiteClipPercentileValue
                        saturationGainFactorValue
                        brightnessGainFactorValue
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

    let createRgbMappingTexture
        (images : alist<AdaptiveImage>)
        (rgbMappingRenderSettings : RgbMappingRenderSettings)
        (shadowsHighlightsAdjustmentsRenderSettings : ShadowsHighlightsAdjustmentsRenderSettings)
        : aval<ITexture> =
        let adaptiveImages =
            AList.toAVal images
        AVal.custom (fun token ->
            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            let redBandValue =
                rgbMappingRenderSettings.redBand.GetValue token
            let greenBandValue =
                rgbMappingRenderSettings.greenBand.GetValue token
            let blueBandValue =
                rgbMappingRenderSettings.blueBand.GetValue token
            let gammaValue =
                rgbMappingRenderSettings.gamma.GetValue token

            let highlightAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.highlightAdjustments.GetValue token
            let highlightAmountValue =
                highlightAdjustmentValue.amount.value
            let highlightToneValue =
                highlightAdjustmentValue.tone.value
            let highlightRadiusValue =
                highlightAdjustmentValue.radius.value

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token
            let shadowAmountValue =
                shadowAdjustmentValue.amount.value
            let shadowToneValue =
                shadowAdjustmentValue.tone.value
            let shadowRadiusValue =
                shadowAdjustmentValue.radius.value

            let midtoneContrastValue =
                shadowsHighlightsAdjustmentsRenderSettings.midtoneContrast.GetValue token
            let midtoneContrastGainFactorValue =
                midtoneContrastValue.gainFactor.value

            let blackWhiteClipValue =
                shadowsHighlightsAdjustmentsRenderSettings.blackWhiteClip.GetValue token
            let blackClipPercentileValue =
                blackWhiteClipValue.blackClipPercentile.value
            let whiteClipPercentileValue = 
                blackWhiteClipValue.whiteClipPercentile.value

            let saturationValue = 
                shadowsHighlightsAdjustmentsRenderSettings.saturation.GetValue token
            let saturationGainFactorValue =
                saturationValue.gainFactor.value

            let brightnessValue = 
                shadowsHighlightsAdjustmentsRenderSettings.brightness.GetValue token
            let brightnessGainFactorValue =
                brightnessValue.gainFactor.value

            match
                sources,
                redBandValue,
                greenBandValue,
                blueBandValue
            with
            | [], _, _, _ ->
                DefaultTextures.checkerboard.GetValue()
            | _, Some redBand, Some greenBand, Some blueBand ->
                match
                    createRgbMappingPixImageFromSources
                        sources
                        redBand
                        greenBand
                        blueBand
                        gammaValue
                        highlightAmountValue
                        highlightToneValue
                        highlightRadiusValue
                        shadowAmountValue
                        shadowToneValue
                        shadowRadiusValue
                        midtoneContrastGainFactorValue
                        blackClipPercentileValue
                        whiteClipPercentileValue
                        saturationGainFactorValue
                        brightnessGainFactorValue
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
                        "Could not create RGB mapping: %s"
                        error
                    DefaultTextures.checkerboard.GetValue()
            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )
        
    // read the selected band and make a raw-value texture
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

            match transferFunctionRenderSettings.selectedBand.GetValue token with
            | None ->
                DefaultTextures.checkerboard.GetValue()

            | Some selectedBand ->
                match loadSelectedTransferFunctionBand sources selectedBand with
                | Result.Error error ->
                    Log.warn "Could not load selected band texture: %s" error
                    DefaultTextures.checkerboard.GetValue()

                | Result.Ok band ->
                    let image =
                        PixImage<float32>(
                            Col.Format.Gray,
                            V2i(band.width, band.height)
                        )

                    let mutable pixels = image.GetMatrix<float32>()

                    for y in 0 .. band.height - 1 do
                        for x in 0 .. band.width - 1 do
                            let value = band.values.[y * band.width + x]

                            pixels.[x, y] <-
                                if Double.IsFinite value then
                                    float32 value
                                else
                                    0.0f

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
            let selectedBand =
                transferFunctionRenderSettings.selectedBand.GetValue token

            let selectedImage =
                adaptiveImages.GetValue token
                |> IndexList.toList
                |> List.tryFind (fun image ->
                    Some (image.bandIndex.GetValue token) = selectedBand
                )

            match selectedImage with
            | None ->
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

    let createTransferFunctionTexture 
        (images : alist<AdaptiveImage>)
        (transferFunctionRenderSettings : TransferFunctionRenderSettings)
        (shadowsHighlightsAdjustmentsRenderSettings : ShadowsHighlightsAdjustmentsRenderSettings)
        : aval<ITexture> =

        let adaptiveImages =
            AList.toAVal images

        AVal.custom (fun token ->

            let currentImages =
                adaptiveImages.GetValue token

            let sources =
                readAdaptiveBandSources currentImages token

            let selectedBandValue =
                transferFunctionRenderSettings.selectedBand.GetValue token

            let gammaValue =
                transferFunctionRenderSettings.gamma.GetValue token

            let highlightAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.highlightAdjustments.GetValue token
            let highlightAmountValue =
                highlightAdjustmentValue.amount.value
            let highlightToneValue =
                highlightAdjustmentValue.tone.value
            let highlightRadiusValue =
                highlightAdjustmentValue.radius.value

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token
            let shadowAmountValue =
                shadowAdjustmentValue.amount.value
            let shadowToneValue =
                shadowAdjustmentValue.tone.value
            let shadowRadiusValue =
                shadowAdjustmentValue.radius.value

            let midtoneContrastValue =
                shadowsHighlightsAdjustmentsRenderSettings.midtoneContrast.GetValue token
            let midtoneContrastGainFactorValue =
                midtoneContrastValue.gainFactor.value

            let saturationValue = 
                shadowsHighlightsAdjustmentsRenderSettings.saturation.GetValue token
            let saturationGainFactorValue =
                saturationValue.gainFactor.value

            let brightnessValue = 
                shadowsHighlightsAdjustmentsRenderSettings.brightness.GetValue token
            let brightnessGainFactorValue =
                brightnessValue.gainFactor.value

            match sources, selectedBandValue with
            | [], _ ->
                DefaultTextures.checkerboard.GetValue()

            | _, None ->
                DefaultTextures.checkerboard.GetValue()

            | _, Some selectedBand ->

                let selectedImage =
                    currentImages
                    |> IndexList.toList
                    |> List.tryFind (fun image ->
                        image.bandIndex.GetValue token = selectedBand
                    )

                match selectedImage with
                | None ->
                    DefaultTextures.checkerboard.GetValue()

                | Some image ->

                    let minimumValue =
                        image.inputMinValue.value.GetValue token

                    let maximumValue =
                        image.inputMaxValue.value.GetValue token

                    let useFalseColorValue =
                        image.useFalseColor.GetValue token

                    let colorMapValue =
                        image.colorMap.GetValue token

                    match
                        createTransferFunctionPixImageFromSource
                            sources
                            selectedBand
                            minimumValue
                            maximumValue
                            gammaValue
                            useFalseColorValue
                            colorMapValue
                            highlightAmountValue
                            highlightToneValue
                            highlightRadiusValue
                            shadowAmountValue
                            shadowToneValue
                            shadowRadiusValue
                            midtoneContrastGainFactorValue
                            saturationGainFactorValue
                            brightnessGainFactorValue
                    with
                    | Result.Ok pixImage ->
                        PixTexture2d(
                            PixImageMipMap [|
                                pixImage :> PixImage
                            |],
                            false
                        ) :> ITexture

                    | Result.Error error ->
                        Log.warn
                            "Could not create transfer-function image: %s"
                            error

                        DefaultTextures.checkerboard.GetValue()
        )

    // Makes the RGB texture adaptive. It is recalculated when the loaded image rows,
    // RGB band selections, contrast/gamma controls, or highlight controls change.
    let createRgbCompositeTextureWithHighlights
        (images : alist<AdaptiveImage>)
        (rgbCompositeRenderSettings : BandRatioRenderSettings)
        (shadowsHighlightsAdjustmentsRenderSettings : ShadowsHighlightsAdjustmentsRenderSettings)
        : aval<ITexture> =

        let adaptiveImages =
            AList.toAVal images

        AVal.custom (fun token ->

            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            let redNumeratorValue =
                rgbCompositeRenderSettings.redNumeratorBand.GetValue token

            let redDenominatorValue =
                rgbCompositeRenderSettings.redDenominatorBand.GetValue token

            let greenNumeratorValue =
                rgbCompositeRenderSettings.greenNumeratorBand.GetValue token

            let greenDenominatorValue =
                rgbCompositeRenderSettings.greenDenominatorBand.GetValue token

            let blueNumeratorValue =
                rgbCompositeRenderSettings.blueNumeratorBand.GetValue token

            let blueDenominatorValue =
                rgbCompositeRenderSettings.blueDenominatorBand.GetValue token

            let gammaValue =
                rgbCompositeRenderSettings.gamma.GetValue token

            let highlightAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.highlightAdjustments.GetValue token

            let highlightAmountValue =
                highlightAdjustmentValue.amount.value

            let highlightToneValue =
                highlightAdjustmentValue.tone.value

            let highlightRadiusValue =
                highlightAdjustmentValue.radius.value

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token

            let shadowAmountValue =
                shadowAdjustmentValue.amount.value

            let shadowToneValue =
                shadowAdjustmentValue.tone.value

            let shadowRadiusValue =
                shadowAdjustmentValue.radius.value

            let midtoneContrastValue =
                shadowsHighlightsAdjustmentsRenderSettings.midtoneContrast.GetValue token

            let midtoneContrastGainFactorValue =
                midtoneContrastValue.gainFactor.value

            let blackWhiteClipValue =
                shadowsHighlightsAdjustmentsRenderSettings.blackWhiteClip.GetValue token

            let blackClipPercentileValue =
                blackWhiteClipValue.blackClipPercentile.value

            let whiteClipPercentileValue = 
                blackWhiteClipValue.whiteClipPercentile.value

            let saturationValue = 
                shadowsHighlightsAdjustmentsRenderSettings.saturation.GetValue token

            let saturationGainFactorValue =
                saturationValue.gainFactor.value

            let brightnessValue = 
                shadowsHighlightsAdjustmentsRenderSettings.brightness.GetValue token

            let brightnessGainFactorValue =
                brightnessValue.gainFactor.value

            match
                sources,
                redNumeratorValue,
                greenNumeratorValue,
                blueNumeratorValue
            with
            | [], _, _, _ ->
                DefaultTextures.checkerboard.GetValue()

            | _, Some redNumerator, Some greenNumerator, Some blueNumerator ->

                match
                    createRgbRatioCompositePixImageFromSources
                        sources
                        redNumerator
                        redDenominatorValue
                        greenNumerator
                        greenDenominatorValue
                        blueNumerator
                        blueDenominatorValue
                        gammaValue
                        highlightAmountValue
                        highlightToneValue
                        highlightRadiusValue
                        shadowAmountValue
                        shadowToneValue     
                        shadowRadiusValue
                        midtoneContrastGainFactorValue
                        blackClipPercentileValue
                        whiteClipPercentileValue
                        saturationGainFactorValue
                        brightnessGainFactorValue
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

    let createStretchedGreyscaleTexture
        (sourcePath : aval<Option<string>>)
        (blackPoint : aval<float>)
        (whitePoint : aval<float>)
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

                for y in 0 .. source.Size.Y - 1 do
                    for x in 0 .. source.Size.X - 1 do
                        let grey = float input.[x, y]
                        let stretched =
                            255.0 * ImageMath.clamp01 ((grey - black) / (white - black))
                            |> round
                            |> byte

                        pixels.[x, y] <- C4b(stretched, stretched, stretched, 255uy)

                PixTexture2d(
                    PixImageMipMap [| output :> PixImage |],
                    false
                ) :> ITexture

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    // the 2D view displays the texture directly
    let createInstrumentScene
        (rgbTexture : aval<ITexture>)
        (bandTexture : aval<ITexture>)
        (colormapTexture : aval<ITexture>) 
        (gpuSettings : aval<float * float * bool>) 
        (useGpu : aval<bool>) 
        (clickedPixel : aval<Option<V2i>>)
        (imageWidth : aval<int>)
        (imageHeight : aval<int>) =

        let geometry =
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
           
        let pixelMarkerPass =
            RenderPass.after "pixel-marker" RenderPassOrder.FrontToBack RenderPass.main

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
                    let markerRadius = 1.0

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

        let baseSg = Sg.ofIndexedGeometry geometry

        let rgbSg =
            baseSg
            |> Sg.texture "RgbCompositeTexture" rgbTexture
            |> Sg.uniform "ShowPixelMarker" showPixelMarker
            |> Sg.uniform "PixelMarkerMin" pixelMarkerMin
            |> Sg.uniform "PixelMarkerMax" pixelMarkerMax
            |> Sg.shader {
                do! Shaders.displayRgbComposite
            }

        let minValue = gpuSettings |> AVal.map (fun (minimum, _, _) -> minimum)
        let maxValue = gpuSettings |> AVal.map (fun (_, maximum, _) -> maximum)
        let shaderFalseColor = gpuSettings |> AVal.map (fun (_, _, flag) -> flag)

        let gpuSg =
            baseSg
            |> Sg.texture "InstrumentImage" bandTexture
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.uniform "MinValue" minValue
            |> Sg.uniform "MaxValue" maxValue
            |> Sg.uniform "UseFalseColor" shaderFalseColor
            |> Sg.uniform "DataType" (AVal.constant 2)
            |> Sg.shader {
                do! Shaders.hshColors
            }

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
                //Html.row "EXR Channel:" [
                //    div [style "color: white;"] [
                //        let channelRepr (c : Channel) = 
                //            match c.name with
                //            | None -> string c.idx
                //            | Some name -> name
                //        Html.SemUi.dropDown' (AList.ofAVal m.channelOptions) m.selectedChannel (fun value -> SetEXRChannel value) channelRepr
                //    ]
                //]
                Html.row "False Color:" [
                    
                    Html.SemUi.dropDown m.colorMap SetColorMap
                ]
                //Html.row "Minimum:" [
                //    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.inputMinValue.value) SetCustomMin
                //    br []
                //    Numeric.view' [Slider] m.inputMinValue
                //    |> UI.map (fun action -> 
                //        match action with
                //        | Numeric.Action.SetValue v ->
                //            SetCustomMin v
                //        | _ ->
                //            ImageMessage.Empty
                //        )
                //    ]
                //Html.row "Maximum:"  [
                //    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.inputMaxValue.value) SetCustomMax
                //    br []
                //    div [style "width: 100%"] [
                //        Numeric.numericField' m.inputMaxValue Slider
                //        |> UI.map (fun action -> 
                //            match action with
                //            | Numeric.Action.SetValue v ->
                //                SetCustomMax v
                //            | _ ->
                //                ImageMessage.Empty
                //            )
                //        ]
                //    ] 
                //Html.row "" [button [clazz "ui inverted button"; onClick (fun _ -> ResetCustomMinMax)] [
                //        text "Reset"
                //    ]
                //]
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
        (bandTexture : aval<ITexture>)
        (colormapTexture : aval<ITexture>)
        (gpuSettings : aval<float * float * bool>)
        (useGpu : aval<bool>)
        clickedPixel
        imageWidth
        imageHeight =

        let instrumentVisualization =
            createInstrumentScene
                rgbTexture
                bandTexture
                colormapTexture
                gpuSettings
                useGpu
                clickedPixel
                imageWidth
                imageHeight

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

        let imageSettings =
            {
                VisualizationProperties.empty with
                    projectionOpacity = opacity
            }

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
            
        let projectPrimaryImage =
            Visualization.creatProjectionFunction
                observer
                time
                referenceFrame
                currentProjectedImage
                projection

        let primaryProjectionEnabled =
            currentProjectedImage
            |> AVal.map (function
                | Some (_, (Some _, _)) -> true
                | _ -> false
            )

        let scene =
            Visualization.createRgbSceneGraph
                imageSettings
                referenceFrame
                supportBody
                observer
                time
                projectPrimaryImage
                rgbTexture
                primaryProjectionEnabled
            |> Sg.noEvents
            

        require Html.semui (
                div [] [
                    div [] [
                        // the 2D control
                        let leftControl = [style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"; attribute "showLoader" "false"]
                        renderControl (AVal.constant (Camera.create cameraView frustum2D)) leftControl instrumentVisualization
                    
                    ]
                ]
        )

    let view2DRelative 
        (rgbTexture : aval<ITexture>) 
        clickedPixel
        imageWidth
        imageHeight =

        let instrumentVisualization =
            createInstrumentScene
                rgbTexture
                rgbTexture
                rgbTexture
                (AVal.constant (0.0, 1.0, true))
                (AVal.constant false)
                clickedPixel
                imageWidth
                imageHeight

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        require Html.semui (
            div [style "width: 100%; height: 200px; display: flex; align-items: center; justify-content: center; margin-top: 10px; border: solid 2px black; background: rgb(0, 0, 0, 0.5);"] [
                let style = [style "position: relative; width: 200px; height: 200px; padding: 2px"; attribute "showLoader" "false"]
                renderControl (AVal.constant (Camera.create cameraView frustum')) style instrumentVisualization
            ]
        )
