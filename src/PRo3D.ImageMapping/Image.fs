namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO
open System.Runtime.InteropServices

open HDF.PInvoke

open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE

open System.Text.Json
open System.Collections.Concurrent

open ImageMath
open ImageMetadata
open NetCdfLoader
open NetCdfLoader

open PRo3D.ImageMapping.MbiLoader
open PRo3D.ImageMapping.ImageDefaults
open PRo3D.ImageMapping.TiffLoader
open PRo3D.ImageMapping.RgbComposite

module Image =

    let private trySaveDebugBmp
        (width : int)
        (height : int)
        (rgbBytes : byte[])
        (path : string) =

        try
            let directory =
                Path.GetDirectoryName path

            if not (String.IsNullOrWhiteSpace directory) then
                Directory.CreateDirectory directory |> ignore

            use stream =
                File.Create path

            use writer =
                new BinaryWriter(stream)

            let rowStride =
                ((width * 3 + 3) / 4) * 4

            let padding =
                Array.zeroCreate<byte> (rowStride - width * 3)

            let pixelDataSize =
                rowStride * height

            let fileSize =
                54 + pixelDataSize

            // BMP header
            writer.Write(byte 0x42) // B
            writer.Write(byte 0x4D) // M
            writer.Write(int32 fileSize)
            writer.Write(int16 0)
            writer.Write(int16 0)
            writer.Write(int32 54)

            // DIB header
            writer.Write(int32 40)
            writer.Write(int32 width)
            writer.Write(int32 height)
            writer.Write(int16 1)
            writer.Write(int16 24)
            writer.Write(int32 0)
            writer.Write(int32 pixelDataSize)
            writer.Write(int32 2835)
            writer.Write(int32 2835)
            writer.Write(int32 0)
            writer.Write(int32 0)

            // BMP stores rows bottom-up and pixels as BGR
            for y = height - 1 downto 0 do
                for x = 0 to width - 1 do
                    let index =
                        (y * width + x) * 3

                    let r =
                        rgbBytes.[index + 0]

                    let g =
                        rgbBytes.[index + 1]

                    let b =
                        rgbBytes.[index + 2]

                    writer.Write(b)
                    writer.Write(g)
                    writer.Write(r)

                if padding.Length > 0 then
                    writer.Write(padding)

            Log.warn "Saved debug RGB composite to %s" path

        with error ->
            Log.warn "Could not save debug RGB composite: %s" error.Message

    let private loadRgbCompositeTextureFromSources
        (sources : list<RgbBandSource>)
        (redNumeratorIndex : int)
        (redDenominatorIndex : int)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : int)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : int)
        (gamma : float)
        (highlightAmount : float)
        (highlightTone : float)
        (highlightRadius : float)
        (shadowAmount : float)
        (shadowTone : float)
        (shadowRadius : float)
        (midtoneContrastGainFactor : float)
        (blackClipPercentileValue : float)
        (whiteClipPercentileValue : float)
        : ITexture =

        match
            createRgbCompositePixImageFromSources
                sources
                redNumeratorIndex
                redDenominatorIndex
                greenNumeratorIndex
                greenDenominatorIndex
                blueNumeratorIndex
                blueDenominatorIndex
                gamma
                highlightAmount
                highlightTone
                highlightRadius
                shadowAmount
                shadowTone
                shadowRadius
                midtoneContrastGainFactor
                blackClipPercentileValue
                whiteClipPercentileValue
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

    // Makes the RGB texture adaptive. It is recalculated when the loaded image rows,
    // RGB band selections, contrast/gamma controls, or highlight controls change.
    let createRgbCompositeTextureWithHighlights
        (images : alist<AdaptiveImage>)
        (redNumeratorBand : aval<Option<int>>)
        (redDenominatorBand : aval<Option<int>>)
        (greenNumeratorBand : aval<Option<int>>)
        (greenDenominatorBand : aval<Option<int>>)
        (blueNumeratorBand : aval<Option<int>>)
        (blueDenominatorBand : aval<Option<int>>)
        (gamma : aval<float>)
        (highlightAmount : aval<float>)
        (highlightTone : aval<float>)
        (highlightRadius : aval<float>)
        (shadowAmount : aval<float>)
        (shadowTone : aval<float>)
        (shadowRadius : aval<float>)
        (midToneContrastGainFactor : aval<float>)
        (blackClipPercentile : aval<float>)
        (whiteClipPercentile : aval<float>)
        : aval<ITexture> =

        let adaptiveImages =
            AList.toAVal images

        AVal.custom (fun token ->

            let sources =
                adaptiveImages.GetValue token
                |> fun images -> readAdaptiveBandSources images token

            let redNumeratorValue =
                redNumeratorBand.GetValue token

            let redDenominatorValue =
                redDenominatorBand.GetValue token

            let greenNumeratorValue =
                greenNumeratorBand.GetValue token

            let greenDenominatorValue =
                greenDenominatorBand.GetValue token

            let blueNumeratorValue =
                blueNumeratorBand.GetValue token

            let blueDenominatorValue =
                blueDenominatorBand.GetValue token

            let gammaValue =
                gamma.GetValue token

            let highlightAmountValue =
                highlightAmount.GetValue token

            let highlightToneValue =
                highlightTone.GetValue token

            let highlightRadiusValue =
                highlightRadius.GetValue token

            let shadowAmountValue =
                shadowAmount.GetValue token

            let shadowToneValue =
                shadowTone.GetValue token

            let shadowRadiusValue =
                shadowRadius.GetValue token

            let midtoneContrastGainFactorValue =
                midToneContrastGainFactor.GetValue token

            let blackClipPercentileValue =
                blackClipPercentile.GetValue token

            let whiteClipPercentileValue = 
                whiteClipPercentile.GetValue token

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

                loadRgbCompositeTextureFromSources
                    sources
                    redNumerator
                    redDenominator
                    greenNumerator
                    greenDenominator
                    blueNumerator
                    blueDenominator
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

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    // Backwards-compatible wrapper for the existing App.fs call.
    // With these defaults the highlight adjustment is disabled until App.fs wires sliders
    // to createRgbCompositeTextureWithHighlights.
    let createRgbCompositeTexture
        (images : alist<AdaptiveImage>)
        (redNumeratorBand : aval<Option<int>>)
        (redDenominatorBand : aval<Option<int>>)
        (greenNumeratorBand : aval<Option<int>>)
        (greenDenominatorBand : aval<Option<int>>)
        (blueNumeratorBand : aval<Option<int>>)
        (blueDenominatorBand : aval<Option<int>>)
        (gamma : aval<float>)
        (amountHighlight : aval<float>)
        (toneHighlight : aval<float>)
        (radiusHighlight : aval<float>)
        (amountShadow : aval<float>)
        (toneShadow : aval<float>)
        (radiusShadow : aval<float>)
        (midtoneContrastGainFactor : aval<float>)
        (blackClipPercentile : aval<float>)
        (whiteClipPercentile : aval<float>)
        : aval<ITexture> =

        createRgbCompositeTextureWithHighlights
            images
            redNumeratorBand
            redDenominatorBand
            greenNumeratorBand
            greenDenominatorBand
            blueNumeratorBand
            blueDenominatorBand
            gamma
            amountHighlight
            toneHighlight
            radiusHighlight
            amountShadow
            toneShadow
            radiusShadow
            midtoneContrastGainFactor
            blackClipPercentile
            whiteClipPercentile

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
                        // Html.SemUi.dropDown m.channel SetEXRChannel
                    ]
                ]
                Html.row "False Color:" [
                    text "Activate: " 
                    Html.SemUi.toggleBox m.useFalseColor ToggleFalseColor
                    br []
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
