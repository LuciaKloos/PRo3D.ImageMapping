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

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token

            let shadowAmountValue =
                shadowAdjustmentValue.amount.value

            let shadowToneValue =
                shadowAdjustmentValue.tone.value

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
                        shadowAmountValue
                        shadowToneValue                        
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

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token
            let shadowAmountValue =
                shadowAdjustmentValue.amount.value
            let shadowToneValue =
                shadowAdjustmentValue.tone.value

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
                        shadowAmountValue
                        shadowToneValue
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

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token
            let shadowAmountValue =
                shadowAdjustmentValue.amount.value
            let shadowToneValue =
                shadowAdjustmentValue.tone.value

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
                            shadowAmountValue
                            shadowToneValue
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

            let shadowAdjustmentValue =
                shadowsHighlightsAdjustmentsRenderSettings.shadowAdjustments.GetValue token

            let shadowAmountValue =
                shadowAdjustmentValue.amount.value

            let shadowToneValue =
                shadowAdjustmentValue.tone.value

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
                        shadowAmountValue
                        shadowToneValue                        
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

    // the 2D view displays the texture directly
    let createInstrumentScene
        (rgbTexture : aval<ITexture>) =

        Sg.fullScreenQuad
        |> Sg.noEvents
        |> Sg.texture
            "RgbCompositeTexture"
            rgbTexture
        |> Sg.shader {
            do! Shaders.displayRgbComposite
        }

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

    let view (m : AdaptiveImage) =
        let content = 
            Html.table [ 
                Html.row "EXR Channel:" [
                    div [style "color: white;"] [
                        let channelRepr (c : Channel) = 
                            match c.name with
                            | None -> string c.idx
                            | Some name -> name
                        Html.SemUi.dropDown' (AList.ofAVal m.channelOptions) m.selectedChannel (fun value -> SetEXRChannel value) channelRepr
                    ]
                ]
                Html.row "False Color:" [
                    
                    Html.SemUi.dropDown m.colorMap SetColorMap
                ]
                Html.row "Minimum:" [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.inputMinValue.value) SetCustomMin
                    br []
                    Numeric.view' [Slider] m.inputMinValue
                    |> UI.map (fun action -> 
                        match action with
                        | Numeric.Action.SetValue v ->
                            SetCustomMin v
                        | _ ->
                            ImageMessage.Empty
                        )
                    ]
                Html.row "Maximum:"  [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.inputMaxValue.value) SetCustomMax
                    br []
                    div [style "width: 100%"] [
                        Numeric.numericField' m.inputMaxValue Slider
                        |> UI.map (fun action -> 
                            match action with
                            | Numeric.Action.SetValue v ->
                                SetCustomMax v
                            | _ ->
                                ImageMessage.Empty
                            )
                        ]
                    ] 
                Html.row "" [button [clazz "ui inverted button"; onClick (fun _ -> ResetCustomMinMax)] [
                        text "Reset"
                    ]
                ]
            ]

        require Html.semui (
            div [] [
                div [style "position: relative; paddingLeft: 25px; paddingTop: 25px; width: 100%"] [
                    content
                ]
            ]
        )

    let view2DAnd3DImageAbsolute
        (opacity : aval<float>)
        (boresightAdjustment : aval<Option<Trafo3d>>)
        (orbitState : AdaptiveOrbitState)
        (sourceImagePath : aval<Option<string>>)
        (rgbTexture : aval<ITexture>) =

        let instrumentVisualization =
            createInstrumentScene rgbTexture

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

    let view2DRelative (rgbTexture : aval<ITexture>) =
        let instrumentVisualization = createInstrumentScene rgbTexture

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        require Html.semui (
            div [style "width: 100%; height: 200px; display: flex; align-items: center; justify-content: center; margin-top: 10px; border: solid 2px black; background: rgb(0, 0, 0, 0.5);"] [
                let style = [style "position: relative; width: 200px; height: 200px; padding: 2px"; attribute "showLoader" "false"]
                renderControl (AVal.constant (Camera.create cameraView frustum')) style instrumentVisualization
            ]
        )
