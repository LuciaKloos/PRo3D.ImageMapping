namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model
open PRo3D.ImageMapping.SpectralAnalysis

open System.IO

type Self = Self

module App =

    let borderColor = "rgba(255,255,255,.1)"

    let initial : Model = {
        images = IndexList.Empty;
        selectedImage = None;
        sourceImagePath = None;
        sourceImageKind = SourceImageKind.Multispectral
        editImages = List.Empty;
        projectionOpacity = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 1.0 }
        boresightAdjustment = BoresightAdjustment.identity
        cameraState = OrbitState.create V3d.Zero 0.0 0.0 (2.0 * (3389.5 * 1000.0))  
        rgbRatioComposite = RgbRatioComposite.empty
        bandMapping = BandMapping.empty
        transferFunctionMapping = TransferFunctionMapping.empty
        highlightAdjustment = HighlightAdjustment.init
        shadowAdjustment = ShadowAdjustment.init
        midtoneContrastAdjustment = MidtoneContrastAdjustment.init
        blackWhiteClip = BlackWhiteClip.init
        saturation = Saturation.init
        brightness = Brightness.init
        visualizationMode = VisualizationMode.SingleBandTransferFunction
        loadCompleteSpectralProfile = false
    }

    let private loadedLogicalBandIndices (images : IndexList<Image>) =
        images
        |> IndexList.toList
        |> List.map (fun img -> img.bandIndex)
        |> List.sort

    let private pickBand preferredIndex fallbackIndex bands =
        match List.tryItem preferredIndex bands with
        | Some band -> Some band
        | None ->
            match List.tryItem fallbackIndex bands with
            | Some band -> Some band
            | None -> List.tryHead bands

    let private defaultRgbRatioCompositeForImages images =
        let bands =
            loadedLogicalBandIndices images

        {
            RgbRatioComposite.empty with
                redNumeratorBand      = pickBand 16 1 bands
                redDenominatorBand    = pickBand 15 0 bands

                greenNumeratorBand    = pickBand 11 1 bands
                greenDenominatorBand  = pickBand 10 0 bands

                blueNumeratorBand     = pickBand 5 1 bands
                blueDenominatorBand   = pickBand 4 0 bands
        }

    let private defaultBandMappingForImages images =
        let bands =
            loadedLogicalBandIndices images

        {
            BandMapping.empty with
                redBand   = pickBand 2 0 bands
                greenBand = pickBand 1 0 bands
                blueBand  = pickBand 0 0 bands
        }

    let private defaultTransferFunctionMappingForImages images =
        let bands =
            loadedLogicalBandIndices images

        {
            TransferFunctionMapping.empty with
                selectedBand = pickBand 0 0 bands
        }

    let private isPlainRgbImagePath (path : string) =
        match Path.GetExtension(path).ToLowerInvariant() with
        | ".png"
        | ".jpg"
        | ".jpeg"
        | ".webp" -> true
        | _ -> false

    let private noBlackWhiteClip =
        {
            blackClipPercentile =
                { BlackWhiteClip.init.blackClipPercentile with value = 0.0 }

            whiteClipPercentile =
                { BlackWhiteClip.init.whiteClipPercentile with value = 0.0 }
        }

    let update (m : Model) (msg : Message) = 
        match msg with
        | Nop -> m
        | OrbitCameraMessage msg ->
            { m with cameraState = OrbitController.update m.cameraState msg }
        | SetProjectionOpacity opacity -> 
            { m with projectionOpacity = Numeric.update m.projectionOpacity opacity }
        | SetRoll r -> { m with boresightAdjustment = { m.boresightAdjustment with roll = Numeric.update m.boresightAdjustment.roll r } }
        | SetPitch r -> { m with boresightAdjustment = { m.boresightAdjustment with pitch = Numeric.update m.boresightAdjustment.pitch r } }
        | SetYaw r -> { m with boresightAdjustment = { m.boresightAdjustment with yaw = Numeric.update m.boresightAdjustment.yaw r } }
        | LoadMultispectralImage path ->
        
            BandHandler.clearDecodedBandCache ()
            
            let fullPath =
                Path.GetFullPath path

            if isPlainRgbImagePath fullPath then
                {
                    m with
                        images = IndexList.Empty
                        selectedImage = None
                        sourceImagePath = Some fullPath
                        sourceImageKind = SourceImageKind.PlainRgbImage
                        editImages = []
                        blackWhiteClip = noBlackWhiteClip
                }
            else

                let loadedBands =
                    Image.loadDataset fullPath

                if List.isEmpty loadedBands then
                    Log.warn "No image bands were loaded from selected path: %s" fullPath
                    m
                else
                    let bands =
                        loadedBands
                        |> IndexList.ofList

                    let indices =
                        bands
                        |> IndexList.mapi (fun index _ -> index)
                        |> IndexList.toList

                    let firstBand =
                        indices
                        |> List.tryHead

                    let bandCount = 
                        loadedBands.Length

                    let isNetCdf =
                        String.Equals(Path.GetExtension fullPath, ".nc", StringComparison.OrdinalIgnoreCase)

                    let privateLowBand preferred fallback =
                        if bandCount > preferred then
                            Some preferred
                        elif bandCount > fallback then
                            Some fallback
                        elif bandCount > 0 then
                            Some 0
                        else
                            None

                    let defaultRgbComposite =
                        let genericDefaults =
                            RgbRatioComposite.fromBandCount bandCount

                        if isNetCdf then
                            {
                                genericDefaults with
                                    redNumeratorBand = privateLowBand 5 1
                                    redDenominatorBand = privateLowBand 4 0

                                    greenNumeratorBand = privateLowBand 3 1
                                    greenDenominatorBand = privateLowBand 2 0

                                    blueNumeratorBand = privateLowBand 1 1
                                    blueDenominatorBand = privateLowBand 0 0
                            }
                        else
                            genericDefaults

                    if isNetCdf then
                        Log.warn
                            "Default NetCDF RGB ratio bands: R=%A/%A, G=%A/%A, B=%A/%A"
                            defaultRgbComposite.redNumeratorBand
                            defaultRgbComposite.redDenominatorBand
                            defaultRgbComposite.greenNumeratorBand
                            defaultRgbComposite.greenDenominatorBand
                            defaultRgbComposite.blueNumeratorBand
                            defaultRgbComposite.blueDenominatorBand


                    Log.warn "Loaded %d image bands from %s" bandCount fullPath
                    {
                        m with
                            images = bands
                            selectedImage = firstBand
                            sourceImagePath = Some fullPath
                            sourceImageKind = SourceImageKind.Multispectral
                            editImages = []
                            rgbRatioComposite = defaultRgbComposite
                            bandMapping = defaultBandMappingForImages bands
                            transferFunctionMapping = defaultTransferFunctionMappingForImages bands
                            visualizationMode = VisualizationMode.SingleBandTransferFunction
                            loadCompleteSpectralProfile = false
                    }                    
        | SelectImage idx ->
            { m with selectedImage = Some idx }
        | EditImage idx ->
            let editImages' =
                if List.contains idx m.editImages then
                    m.editImages |> List.filter ((<>) idx)
                else
                    idx :: m.editImages
            { m with editImages = editImages'} 
        | ImageMessage (idx, imageMessage) ->
            let images' = m.images |> IndexList.mapi (fun index img ->
                    if index = idx then
                        Image.update img imageMessage
                    else
                        img
                )
            { m with images = images' }
        | SortEntriesByDistance ->
            let images' = 
                m.images
                |> IndexList.mapi (fun idx e -> (idx, e))
                |> IndexList.toList
                |> List.sortBy (fun (idx, p) -> p.distance)
                |> IndexList.ofList
            let newSelectedIdx = 
                match m.selectedImage with
                | Some selectedImage ->
                    let (newIdx, (oldIdx, img) )= 
                        images'
                        |> IndexList.mapi (fun newIdx (idx, p) -> (newIdx, (idx, p)))
                        |> IndexList.filter (fun (newIdx, (idx, p)) -> idx = selectedImage)
                        |> IndexList.toSeq
                        |> Seq.head
                    Some newIdx
                | None -> None
            let editImages' = 
                images'
                |> IndexList.mapi (fun newIdx (idx, p) -> (newIdx, (idx, p)))
                |> IndexList.filter (fun (newIdx, (idx, p)) -> List.contains idx m.editImages)
                |> IndexList.map (fun (newIdx, (idx, p)) -> newIdx)
                |> IndexList.toList
            { m with
                images = images' |> IndexList.map (fun (idx, img) -> img);
                selectedImage = newSelectedIdx;
                editImages = editImages'
            }
        | SortEntriesByDate ->
            let images' = 
                m.images
                |> IndexList.mapi (fun idx e -> (idx, e))
                |> IndexList.toList
                |> List.sortBy (fun (idx, p) -> p.time)
                |> IndexList.ofList
            let newSelectedIdx = 
                match m.selectedImage with
                | Some selectedImage ->
                    let (newIdx, (oldIdx, img) )= 
                        images'
                        |> IndexList.mapi (fun newIdx (idx, p) -> (newIdx, (idx, p)))
                        |> IndexList.filter (fun (newIdx, (idx, p)) -> idx = selectedImage)
                        |> IndexList.toSeq
                        |> Seq.head
                    Some newIdx
                | None -> None
            let editImages' = 
                images'
                |> IndexList.mapi (fun newIdx (idx, p) -> (newIdx, (idx, p)))
                |> IndexList.filter (fun (newIdx, (idx, p)) -> List.contains idx m.editImages)
                |> IndexList.map (fun (newIdx, (idx, p)) -> newIdx)
                |> IndexList.toList
            { m with 
                images = images' |> IndexList.map (fun (idx, img) -> img);
                selectedImage = newSelectedIdx;
                editImages = editImages'}
        | SetBandRatioBand   (rgbChannel, rgbBandRole, rowIndex) ->

            let selectedBandIndex =
                m.images
                |> IndexList.mapi (fun index img -> index, img)
                |> IndexList.toList
                |> List.tryFind (fun (index, _) -> index = rowIndex)
                |> Option.map (fun (_, img) -> img.bandIndex)

            match selectedBandIndex with
            | Some bandIndex ->
                {
                    m with
                        rgbRatioComposite =
                            m.rgbRatioComposite
                            |> RgbRatioComposite.set rgbChannel rgbBandRole bandIndex
                }

            | None ->        
                m
        | SetRgbMappingBand (rgbChannel, rowIndex) ->
            let selectedBandIndex =
                m.images
                |> IndexList.mapi (fun index img -> index, img)
                |> IndexList.toList
                |> List.tryFind (fun (index, _) -> index = rowIndex)
                |> Option.map (fun (_, img) -> img.bandIndex)
            match selectedBandIndex with
            | Some bandIndex ->
                {
                    m with
                        bandMapping =
                            m.bandMapping
                            |> BandMapping.set rgbChannel bandIndex
                }
            | None ->        
                m
        | SetTransferFunctionBand (rowIndex) ->
            let selectedBandIndex =
                m.images
                |> IndexList.mapi (fun index img -> index, img)
                |> IndexList.toList
                |> List.tryFind (fun (index, _) -> index = rowIndex)
                |> Option.map (fun (_, img) -> img.bandIndex)
            match selectedBandIndex with
            | Some bandIndex ->
                {
                    m with
                        transferFunctionMapping =
                            m.transferFunctionMapping
                            |> TransferFunctionMapping.set bandIndex 
                }
            | None ->        
                m

        | SetRgbGamma action ->
            {
                m with
                    rgbRatioComposite =
                        {
                            m.rgbRatioComposite with
                                gamma =
                                    Numeric.update m.rgbRatioComposite.gamma action
                        }
            }
        | SetAmountHighlight action ->
            {
                m with 
                    highlightAdjustment =
                        {
                            m.highlightAdjustment with
                                amount = 
                                    Numeric.update m.highlightAdjustment.amount action
                        }
            }
        | SetToneHighlight action ->
            {
                m with 
                    highlightAdjustment =
                        {
                            m.highlightAdjustment with
                                tone = 
                                    Numeric.update m.highlightAdjustment.tone action
                        }
            }
        | SetRadiusHighlight action ->
            {
                m with 
                    highlightAdjustment =
                        {
                            m.highlightAdjustment with
                                radius = 
                                    Numeric.update m.highlightAdjustment.radius action
                        }
            }
        | SetAmountShadow action ->
            {
                m with 
                    shadowAdjustment =
                        {
                            m.shadowAdjustment with
                                amount = 
                                    Numeric.update m.shadowAdjustment.amount action
                        }
            }
        | SetToneShadow action ->
            {
                m with 
                    shadowAdjustment =
                        {
                            m.shadowAdjustment with
                                tone = 
                                    Numeric.update m.shadowAdjustment.tone action
                        }
            }
        | SetRadiusShadow action ->
            {
                m with 
                    shadowAdjustment =
                        {
                            m.shadowAdjustment with
                                radius = 
                                    Numeric.update m.shadowAdjustment.radius action
                        }
            }
        | SetMidtoneContrastGainFactor action ->
            {
                m with 
                    midtoneContrastAdjustment =
                        {
                            m.midtoneContrastAdjustment with
                                gainFactor =
                                    Numeric.update m.midtoneContrastAdjustment.gainFactor action
                        }
            }
        | SetBlackClipPercentile action ->
            {
                m with
                    blackWhiteClip =
                        {
                            m.blackWhiteClip with
                                blackClipPercentile =
                                    Numeric.update m.blackWhiteClip.blackClipPercentile action
                        }
            }
        | SetWhiteClipPercentile action ->
            {
                m with
                    blackWhiteClip =
                        {
                            m.blackWhiteClip with
                                whiteClipPercentile = 
                                    Numeric.update m.blackWhiteClip.whiteClipPercentile action
                        }
            }
        | SetSaturationGainFactor action ->
            {
                m with
                    saturation =
                        {
                            m.saturation with
                                gainFactor =
                                    Numeric.update m.saturation.gainFactor action
                        }
            }
        | SetBrightnessGainFactor action ->
            {
                m with
                    brightness =
                        {
                            m.brightness with
                                gainFactor =
                                    Numeric.update m.brightness.gainFactor action
                        }
            }
        | ResetHighlights ->
            {
                m with
                    highlightAdjustment = HighlightAdjustment.init
            }
        | ResetShadows ->
            {
                m with
                    shadowAdjustment = ShadowAdjustment.init
            }
        | ResetAdjustments ->
            {
                m with
                    midtoneContrastAdjustment = MidtoneContrastAdjustment.init
                    blackWhiteClip = BlackWhiteClip.init
                    saturation = Saturation.init
                    brightness = Brightness.init
            }

        | SetVisualizationMode mode ->
            match mode with
            | VisualizationMode.RgbRatioComposite ->
                {
                    m with
                        visualizationMode = mode
                        rgbRatioComposite = defaultRgbRatioCompositeForImages m.images
                        editImages = []
                }

            | VisualizationMode.RgbComposite ->
                {
                    m with
                        visualizationMode = mode
                        bandMapping = defaultBandMappingForImages m.images
                        editImages = []
                }

            | VisualizationMode.SingleBandTransferFunction ->
                {
                    m with
                        visualizationMode = mode
                        transferFunctionMapping = defaultTransferFunctionMappingForImages m.images
                        editImages = []
                }

        | ToggleCompleteSpectralProfile ->
             { m with loadCompleteSpectralProfile = not m.loadCompleteSpectralProfile }

    let numericInputFromAdaptive
        (token : AdaptiveToken)
        (input : AdaptiveNumericInput)
        : NumericInput =
        {
            min = input.min.GetValue token
            max = input.max.GetValue token
            step = input.step.GetValue token
            format = input.format.GetValue token
            value = input.value.GetValue token
        }

    let headerForMode mode =
        match mode with
        | VisualizationMode.RgbRatioComposite ->
            [
                "R Num"; "R Den"
                "G Num"; "G Den"
                "B Num"; "B Den"
                "Wavelengths"
                "Dist. to Planet"
                "OBS Date"
            ]

        | VisualizationMode.RgbComposite ->
            [
                "R"
                "G"
                "B"
                "Wavelengths"
                "Dist. to Planet"
                "OBS Date"
            ]

        | VisualizationMode.SingleBandTransferFunction ->
            [
                "Use"
                "Edit"
                "Wavelengths"
                "Dist. to Planet"
                "OBS Date"
            ]

    let view (m : AdaptiveModel) (showDOM : AdaptiveImage -> DomNode<ImageMessage>) (showRelative2DImage : aval<ITexture> -> DomNode<Message>) (showAbsolute2DAnd3DImage : aval<Option<string>> -> aval<ITexture> -> DomNode<Message>) =
    
        let onlyForMultispectral (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! sourceKind = m.sourceImageKind

                        if sourceKind = SourceImageKind.Multispectral then
                            yield node
                    }
                )
    
        let onlyForPlainRGBImage (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! sourceKind = m.sourceImageKind

                        if sourceKind = SourceImageKind.PlainRgbImage then
                            yield node
                    }
                )


        let onlyIfCompleteSpectralProfile (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! loaded = m.loadCompleteSpectralProfile

                        if loaded = true then
                            yield node
                    }
                )

        let bandRatioRenderSettings : BandRatioRenderSettings =
            {
                redNumeratorBand =
                    m.rgbRatioComposite.redNumeratorBand

                redDenominatorBand =
                    m.rgbRatioComposite.redDenominatorBand

                greenNumeratorBand =
                    m.rgbRatioComposite.greenNumeratorBand

                greenDenominatorBand =
                    m.rgbRatioComposite.greenDenominatorBand

                blueNumeratorBand =
                    m.rgbRatioComposite.blueNumeratorBand

                blueDenominatorBand =
                    m.rgbRatioComposite.blueDenominatorBand

                gamma =
                    m.rgbRatioComposite.gamma.value

            }

        let rgbMappingRenderSettings : RgbMappingRenderSettings =
            {
                redBand =
                    m.bandMapping.redBand
                greenBand =
                    m.bandMapping.greenBand
                blueBand =
                    m.bandMapping.blueBand
                gamma =
                    m.bandMapping.gamma.value
            }

        let transferFunctionRenderSettings : TransferFunctionRenderSettings =
            {
                selectedBand =
                    m.transferFunctionMapping.selectedBand
                gamma =
                    m.transferFunctionMapping.gamma.value
            }

        let shadowsHighlightsAdjustmentsRenderSettings : ShadowsHighlightsAdjustmentsRenderSettings =
            {
                highlightAdjustments =
                    AVal.custom (fun token ->
                        {
                            amount =
                                numericInputFromAdaptive token m.highlightAdjustment.amount
                            tone =
                                numericInputFromAdaptive token m.highlightAdjustment.tone
                            radius =
                                numericInputFromAdaptive token m.highlightAdjustment.radius
                        }
                    )
                shadowAdjustments =
                    AVal.custom (fun token ->
                        {
                            amount =
                                numericInputFromAdaptive token m.shadowAdjustment.amount
                            tone =
                                numericInputFromAdaptive token m.shadowAdjustment.tone
                            radius =
                                numericInputFromAdaptive token m.shadowAdjustment.radius
                        }
                    )
                midtoneContrast =
                    AVal.custom (fun token ->
                        {
                            gainFactor =
                                numericInputFromAdaptive token m.midtoneContrastAdjustment.gainFactor
                        }
                    )
                blackWhiteClip =
                    AVal.custom (fun token ->
                        {
                            blackClipPercentile =
                                numericInputFromAdaptive token m.blackWhiteClip.blackClipPercentile
                            whiteClipPercentile =
                                numericInputFromAdaptive token m.blackWhiteClip.whiteClipPercentile
                        }
                    )
                saturation =
                    AVal.custom (fun token ->
                        {
                            gainFactor =
                                numericInputFromAdaptive token m.saturation.gainFactor
                        }
                    )
                brightness =
                    AVal.custom (fun token ->
                        {
                            gainFactor =
                                numericInputFromAdaptive token m.brightness.gainFactor
                        }
                    )
            }

        let outputTexture : aval<ITexture> =
            m.sourceImageKind
            |> AVal.bind (fun sourceKind ->
                match sourceKind with
                | SourceImageKind.PlainRgbImage ->
                    RgbComposite.createPlainRgbTexture
                        m.sourceImagePath
                        shadowsHighlightsAdjustmentsRenderSettings

                | SourceImageKind.Multispectral ->
                    m.visualizationMode
                    |> AVal.bind (fun mode ->
                        match mode with
                        | VisualizationMode.RgbRatioComposite ->
                            Image.createBandRatioTexture
                                m.images
                                bandRatioRenderSettings
                                shadowsHighlightsAdjustmentsRenderSettings

                        | VisualizationMode.RgbComposite ->
                            Image.createRgbMappingTexture
                                m.images
                                rgbMappingRenderSettings
                                shadowsHighlightsAdjustmentsRenderSettings

                        | VisualizationMode.SingleBandTransferFunction ->
                            Image.createTransferFunctionTexture
                                m.images
                                transferFunctionRenderSettings
                                shadowsHighlightsAdjustmentsRenderSettings
                    )
            )

        let rgbRatioSelectedBandHistograms =
            computeRgbRatioSelectedBandHistograms m 32

        let rgbMappingSelectedBandHistogram =
            computeRgbMappingSelectedBandHistograms m 32

        let transferFunctionSelectedBandHistogram =
            computeTransferFunctionSelectedBandHistogram m 32

        let transferFunctionNonMultispectralRgbHistograms =
            computeNonMultispectralRgbHistograms m 32

        let rgbSelectedBandSpectralProfiles =
            computeRgbSpectralProfiles m 

        let allSpectralProfiles = 
            computeCompleteSpectralProfile m

        let spectralProfileOfAllBands =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! sourceKind =
                            m.sourceImageKind

                        if sourceKind = SourceImageKind.Multispectral then
                            yield
                                div [
                                    clazz "ui inverted segment"
                                    style "margin-top: 10px;"
                                ] [
                                    div [
                                        style "font-weight: bold; margin-bottom: 8px;"
                                    ] [                                           
                                    ]

                                    div [
                                        style "font-size: 11px; color: #aaa;"
                                    ] [
                                        text "Spectral Profile of all bands"
                                    ]

                                    spectralProfileView
                                        allSpectralProfiles
                                ]
                    }
                )
        
        let histogramsAndProfilesForCurrentMode =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! mode =
                            m.visualizationMode

                        let! sourceKind =
                            m.sourceImageKind                        
                            
                        if sourceKind = SourceImageKind.Multispectral then
                            match mode with
                            | VisualizationMode.RgbRatioComposite ->
                                yield
                                        div [
                                            clazz "ui inverted segment"
                                            style "margin-top: 10px;"
                                        ] [
                                            div [
                                                style "font-weight: bold; margin-bottom: 8px;"
                                            ] [                                           
                                            ]

                                            div [
                                                style "font-size: 11px; color: #aaa;"
                                            ] [
                                                text "Calculated numerator/denominator ratio per RGB channel"
                                            ]

                                            selectedSpectralProfilesView
                                                rgbSelectedBandSpectralProfiles
                                        ]

                                yield
                                    selectedHistogramsView
                                        "Currently selected RGB band histograms and spectral profiles"
                                        rgbRatioSelectedBandHistograms

                            
                            | VisualizationMode.RgbComposite ->
                                yield
                                        div [
                                            clazz "ui inverted segment"
                                            style "margin-top: 10px;"
                                        ] [
                                            div [
                                                style "font-size: 11px; color: #aaa;"
                                            ] [
                                                text "Spectral Profile of the selected bands"
                                            ]


                                            selectedSpectralProfilesView
                                                rgbSelectedBandSpectralProfiles
                                        ]

                                yield
                                    selectedHistogramsView
                                        "Currently selected RGB band histograms and spectral profiles"
                                        rgbMappingSelectedBandHistogram
                                
                            | VisualizationMode.SingleBandTransferFunction ->
                                yield
                                        div [
                                            clazz "ui inverted segment"
                                            style "margin-top: 10px;"
                                        ] [
                                            div [
                                                style "font-size: 11px; color: #aaa;"
                                            ] [
                                                text "Spectral Profile of the selected band"
                                            ]

                                            selectedSpectralProfilesView
                                                rgbSelectedBandSpectralProfiles
                                        ]

                                yield
                                    selectedHistogramsView
                                            "Selected transfer-function band histogram and spectral profile"
                                            transferFunctionSelectedBandHistogram
                        else 
                            yield
                                    div [
                                        clazz "ui inverted segment"
                                        style "margin-top: 10px;"
                                    ] [
                                        div [
                                            style "font-size: 11px; color: #aaa;"
                                        ] [
                                            text "Spectral Profile of the R G B channels"
                                        ]

                                        selectedSpectralProfilesView
                                            rgbSelectedBandSpectralProfiles
                                    ]

                            yield
                                selectedHistogramsView                                    
                                        "Original image RGB channel histograms"
                                        transferFunctionNonMultispectralRgbHistograms
                    }
                )

        let jsImportDialog =
            "top.aardvark.dialog.showOpenDialog({title: 'Select image', filters: [{name: 'Images', extensions: ['mbi', 'json', 'tif', 'tiff', 'nc', 'png', 'jpg', 'jpeg', 'webp']}], properties: ['openFile']}).then(result => {if (!result.canceled && result.filePaths && result.filePaths.length > 0) {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);}}).catch(error => {console.error('Could not open image dialog:', error);});"

        let accordion text' icon active styling content' =
                let title = if active then "title active inverted" else "title inverted"
                let content = if active then "content active" else "content"
                                    
                onBoot "$('#__ID__').accordion();" (
                    div [styling] [
                        div [clazz "ui inverted accordion fluid"] [
                            div [clazz title; style "background-color: #282828"] [
                                    i [clazz ("dropdown icon")] []
                                    text text'                                
                                    div [style "float:right"] [i [clazz (icon + " icon")] []]
                                
                            ]
                            div [clazz content;  style "overflow-y : auto; "] content' 
                        ]
                    ]
                )

        let accordionLists text' icon active styling resetMessage content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"

            onBoot "$('#__ID__ > .ui.accordion').accordion({ exclusive: false, selector: { title: '> .title', content: '> .content' } });" (
                div styling [
                    div [clazz "ui inverted accordion fluid"] [
                        div [clazz title; style "background-color: #282828"] [
                            i [clazz "dropdown icon"] []
                            text text'
                            div [style "float:right"] [
                                i [clazz (icon + " icon")] []
                            ]
                        ]

                        div [clazz content; style "overflow-y : auto; "] (
                            [
                                div [style "display: flex; justify-content: flex-end; padding: 5px;"] [
                                    button [
                                        clazz "ui tiny inverted button"
                                        onClick (fun _ -> resetMessage)
                                    ] [
                                        text "Reset"
                                    ]
                                ]
                            ] @ content'
                        )
                    ]
                ]
            )

        let accordionHist text' icon active styling content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"

            onBoot "$('#__ID__ > .ui.accordion').accordion({ exclusive: false, selector: { title: '> .title', content: '> .content' } });" (
                div styling [
                        div [clazz "ui inverted accordion fluid"] [
                            div [clazz title; style "background-color: #282828"] [
                                    i [clazz ("dropdown icon")] []
                                    text text'                                
                                    div [style "float:right"] [i [clazz (icon + " icon")] []]
                                
                            ]
                            div [clazz content;  style "overflow-y : auto; "] (
                                [
                                ] @ content'
                            )
                        ]
                ]
            )

        let visualizationModeOption label mode =
            div [
                style "display: flex; align-items: center; gap: 6px; margin-right: 12px; cursor: pointer;"
            ] [
                Html.SemUi.iconCheckBox
                    (
                        m.visualizationMode
                        |> AVal.map (fun currentMode -> currentMode = mode)
                    )
                    (SetVisualizationMode mode)

                span [
                    onClick (fun _ -> SetVisualizationMode mode)
                ] [
                    text label
                ]
            ]

        let visualizationModeSelector =
            
            Incremental.div AttributeMap.empty (
                alist {
                    let! sourceKind = m.sourceImageKind


                    if sourceKind = SourceImageKind.Multispectral then
                        div [
                            clazz "item"
                            style "border-bottom: solid 1px black; padding: 5px;"
                        ] [
                            div [
                                style "margin-bottom: 6px;"
                            ] [
                                text "Mode:"
                            ]

                            Incremental.div
                                (AttributeMap.ofList [
                                    attribute "style"
                                        "display: flex; align-items: center; flex-wrap: wrap;"
                                ])
                                (
                            
                                    alist {
                                        yield
                                            visualizationModeOption
                                                "Band ratio"
                                                VisualizationMode.RgbRatioComposite

                                        yield
                                            visualizationModeOption
                                                "RGB mapping"
                                                VisualizationMode.RgbComposite

                                        yield
                                            visualizationModeOption
                                                "Transfer function"
                                                VisualizationMode.SingleBandTransferFunction
                                    }
                                
                                )
                        ]
                    }
                )

        let contentImages = 
            let attributesSelect = attribute "style" $"cursor: pointer; width: 50px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesEdit = attribute "style" $"cursor: pointer; width: 50px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesWavelength = attribute "style" $"cursor: pointer; width: 120px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesAttr1 = attribute "style" $"cursor: pointer; width: 120px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesAttr2 = attribute "style" $"cursor: pointer; width: 120px; height: 40px; display: flex; justify-content: center; align-items: center;"

            let header =
                Incremental.div
                    (AttributeMap.ofList [
                        attribute "style" $"display: flex; font-weight: bold; border-bottom: 2px solid black; background: black"
                    ])
                    (
                        alist {
                            let! mode = m.visualizationMode

                            let selectHeader label =
                                div [attributesSelect] [text label]

                            match mode with
                            | VisualizationMode.RgbRatioComposite ->
                                yield selectHeader "R Num"
                                yield selectHeader "R Den"
                                yield selectHeader "G Num"
                                yield selectHeader "G Den"
                                yield selectHeader "B Num"
                                yield selectHeader "B Den"

                            | VisualizationMode.RgbComposite ->
                                yield selectHeader "R"
                                yield selectHeader "G"
                                yield selectHeader "B"

                            | VisualizationMode.SingleBandTransferFunction ->
                                yield selectHeader "Use"
                                yield div [attributesEdit] [text "Edit"]

                            yield div [attributesWavelength] [text "Wavelengths"]

                            yield div [attributesAttr1] [
                                i [clazz "sort icon"; onClick (fun _ -> SortEntriesByDistance)] []
                                text "Dist. to Planet"
                            ]

                            yield div [attributesAttr2] [
                                i [clazz "sort icon"; onClick (fun _ -> SortEntriesByDate)] []
                                text "OBS Date"
                            ]
                        }
                    )
            Incremental.div (AttributeMap.ofList [ attribute "class" "table-container" ]) (
                alist {
                    yield header
                    yield div [attribute "style" "max-height: calc(100vh - 300px); overflow: auto;" ] [
                        yield Incremental.div (AttributeMap.ofList [ attribute "style" "overflow-y: visible; " ]) (
                            alist {
                                let domNodes = 
                                    m.images 
                                    |> AList.mapi (fun index img ->
                                        // let distanceToPlanet = CooTransformation.getRelState "HERA" "SUN" "MARS"  

                                        div [attribute "style" $"border: 1px solid rgba(255,255,255,0.5);"] [
                                            div [attribute "style" $"border-bottom: 1px solid {borderColor}; background: #333"]
                                                [
                                                    Incremental.text (
                                                        (img.texture, img.bandIndex)
                                                        ||> AVal.map2 (fun path bandIndex ->
                                                            let bandNumber = bandIndex + 1

                                                            sprintf "%s — Band %d"
                                                                    (Path.GetFileName path)
                                                                    bandNumber

                                                        )
                                                    )
                                                ]
                                            Incremental.div
                                                (AttributeMap.ofList [
                                                    attribute "style" "display: flex; font-weight: bold"
                                                ])
                                                (
                                                    alist {
                                                        let! mode = m.visualizationMode

                                                        let selectedBandIndex =
                                                            img.bandIndex

                                                        let bandRatioSelector
                                                            (selected : aval<Option<int>>)
                                                            (channel : RgbChannel)
                                                            (role : RgbBandRole) =

                                                            div [attributesSelect] [
                                                                Html.SemUi.iconCheckBox
                                                                    (
                                                                        (selected, selectedBandIndex)
                                                                        ||> AVal.map2 (fun selected bandIndex ->
                                                                            selected = Some bandIndex
                                                                        )
                                                                    )
                                                                    (SetBandRatioBand (channel, role, index))
                                                            ]

                                                        let rgbMappingSelector
                                                            (selected : aval<Option<int>>)
                                                            (channel : RgbChannel) =

                                                            div [attributesSelect] [
                                                                Html.SemUi.iconCheckBox
                                                                    (
                                                                        (selected, selectedBandIndex)
                                                                        ||> AVal.map2 (fun selected bandIndex ->
                                                                            selected = Some bandIndex
                                                                        )
                                                                    )
                                                                    (SetRgbMappingBand (channel, index))
                                                            ]

                                                        let transferFunctionSelector
                                                            (selected : aval<Option<int>>) =

                                                            div [attributesSelect] [
                                                                Html.SemUi.iconCheckBox
                                                                    (
                                                                        (selected, selectedBandIndex)
                                                                        ||> AVal.map2 (fun selected bandIndex ->
                                                                            selected = Some bandIndex
                                                                        )
                                                                    )
                                                                    (SetTransferFunctionBand index)
                                                            ]

                                                        let editSelector =
                                                            div [attributesEdit] [
                                                                Html.SemUi.iconCheckBox
                                                                    (
                                                                        m.editImages
                                                                        |> AVal.map (fun editImages ->
                                                                            List.contains index editImages
                                                                        )
                                                                    )
                                                                    (EditImage index)
                                                            ]

                                                        match mode with
                                                        | VisualizationMode.RgbRatioComposite ->
                                                            yield bandRatioSelector m.rgbRatioComposite.redNumeratorBand RgbChannel.Red RgbBandRole.Numerator
                                                            yield bandRatioSelector m.rgbRatioComposite.redDenominatorBand RgbChannel.Red RgbBandRole.Denominator
                                                            yield bandRatioSelector m.rgbRatioComposite.greenNumeratorBand RgbChannel.Green RgbBandRole.Numerator
                                                            yield bandRatioSelector m.rgbRatioComposite.greenDenominatorBand RgbChannel.Green RgbBandRole.Denominator
                                                            yield bandRatioSelector m.rgbRatioComposite.blueNumeratorBand RgbChannel.Blue RgbBandRole.Numerator
                                                            yield bandRatioSelector m.rgbRatioComposite.blueDenominatorBand RgbChannel.Blue RgbBandRole.Denominator

                                                        | VisualizationMode.RgbComposite ->
                                                            yield rgbMappingSelector m.bandMapping.redBand RgbChannel.Red
                                                            yield rgbMappingSelector m.bandMapping.greenBand RgbChannel.Green
                                                            yield rgbMappingSelector m.bandMapping.blueBand RgbChannel.Blue

                                                        | VisualizationMode.SingleBandTransferFunction ->
                                                            yield transferFunctionSelector m.transferFunctionMapping.selectedBand
                                                            yield editSelector

                                                        yield div [attributesWavelength] [
                                                            Incremental.text (
                                                                img.selectedChannel
                                                                |> AVal.map (fun channel ->
                                                                    match channel.name with
                                                                    | Some wavelength -> wavelength
                                                                    | None -> "-"
                                                                )
                                                            )
                                                        ]

                                                        yield div [attributesAttr1] [
                                                            Incremental.text (
                                                                img.distance
                                                                |> AVal.map (fun f -> sprintf "%.2f" f)
                                                            )
                                                        ]

                                                        yield div [attributesAttr2] [
                                                            Incremental.text (
                                                                img.time
                                                                |> AVal.map string
                                                            )
                                                        ]
                                                    }
                                                )
                                        
                                            Incremental.div AttributeMap.empty (
                                                alist {
                                                    let! mode = m.visualizationMode

                                                    let! isInEditMode =
                                                        m.editImages
                                                        |> AVal.map (fun editEntries ->
                                                            List.contains index editEntries
                                                        )

                                                    if mode = VisualizationMode.SingleBandTransferFunction && isInEditMode then
                                                        yield div [attribute "style" $"border-top: 1px dotted rgba(255,255,255,0.5)"] [
                                                            showDOM img
                                                            |> UI.map (fun msg -> Message.ImageMessage (index, msg))
                                                        ]
                                                }
                                            )
                                        ]
                                    )
                                for domNode in domNodes do
                                    yield domNode
                        })
                    ]
                })

        let content = 
            div [style "overlow-y: auto; max-height: calc(100vh - 95px);"] [

                div [clazz "ui inverted list"] [
                    div [clazz "item"; style "border-bottom: solid 1px black; padding: 5px; display: flex; justify-content: space-between; align-items: center;"] [
                        div [] [text "Data:"]
                        button [clazz "ui button tiny";
                                style "margin-left: auto;"
                                Dialogs.onChooseDirectory (Guid.NewGuid()) (fun (guid, chosen) -> LoadMultispectralImage (chosen) );
                                clientEvent "onclick" (jsImportDialog) ] [
                                text "Import Image"
                        ]
                    ]

                    div [clazz "item"; style "border-bottom: solid 1px black; height: 30px; padding: 5px; display: flex; justify-content: space-between; align-items: center;"] [
                        div [] [text "Visualization:"]
                        div [style "margin-left: auto;"] [
                            Numeric.view' [NumericInputType.Slider] m.projectionOpacity
                            |> UI.map SetProjectionOpacity
                        ]
                    ]

                    visualizationModeSelector
                     
                    onlyForPlainRGBImage (
                        accordionLists "Highlights and Shadows" "sliders horizontal" false
                            [clazz "item"; style "margin-top: 10px;"]
                            ResetHighlights
                            [
                                div [
                                    style "font-weight: bold; margin-bottom: 6px;"
                                ] [
                                    text "Highlights"
                                ]

                                Html.table [
                                    Html.row "Amount:" [
                                        Numeric.view' [NumericInputType.Slider] m.highlightAdjustment.amount
                                        |> UI.map SetAmountHighlight

                                        Numeric.view' [NumericInputType.InputBox] m.highlightAdjustment.amount
                                        |> UI.map SetAmountHighlight
                                    ]

                                    Html.row "Tone:" [
                                        Numeric.view' [NumericInputType.Slider] m.highlightAdjustment.tone
                                        |> UI.map SetToneHighlight

                                        Numeric.view' [NumericInputType.InputBox] m.highlightAdjustment.tone
                                        |> UI.map SetToneHighlight
                                    ]

                                    Html.row "Radius:" [
                                        Numeric.view' [NumericInputType.Slider] m.highlightAdjustment.radius
                                        |> UI.map SetRadiusHighlight

                                        Numeric.view' [NumericInputType.InputBox] m.highlightAdjustment.radius
                                        |> UI.map SetRadiusHighlight
                                    ]
                                ]

                                div [
                                    style "font-weight: bold; margin-bottom: 6px;"
                                ] [
                                    text "Shadows"
                                ]

                                Html.table [
                                    Html.row "Amount:" [
                                        Numeric.view' [NumericInputType.Slider] m.shadowAdjustment.amount
                                        |> UI.map SetAmountShadow

                                        Numeric.view' [NumericInputType.InputBox] m.shadowAdjustment.amount
                                        |> UI.map SetAmountShadow
                                    ]

                                    Html.row "Tone:" [
                                        Numeric.view' [NumericInputType.Slider] m.shadowAdjustment.tone
                                        |> UI.map SetToneShadow

                                        Numeric.view' [NumericInputType.InputBox] m.shadowAdjustment.tone
                                        |> UI.map SetToneShadow
                                    ]

                                    Html.row "Radius:" [
                                        Numeric.view' [NumericInputType.Slider] m.shadowAdjustment.radius
                                        |> UI.map SetRadiusShadow

                                        Numeric.view' [NumericInputType.InputBox] m.shadowAdjustment.radius
                                        |> UI.map SetRadiusShadow
                                    ]
                                ]
                            ]
                        )

                    onlyForPlainRGBImage (
                        accordionLists "Adjustments" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] ResetAdjustments [
                            Html.table [
                                Html.row "Color (Saturation):" [
                                    Numeric.view' [NumericInputType.Slider] m.saturation.gainFactor  
                                    |> UI.map SetSaturationGainFactor

                                    Numeric.view' [NumericInputType.InputBox] m.saturation.gainFactor
                                    |> UI.map SetSaturationGainFactor
                                ]

                                Html.row "Brightness:" [
                                    Numeric.view' [NumericInputType.Slider] m.brightness.gainFactor  
                                    |> UI.map SetBrightnessGainFactor

                                    Numeric.view' [NumericInputType.InputBox] m.brightness.gainFactor
                                    |> UI.map SetBrightnessGainFactor
                                ]

                                Html.row "Midtone Contrast:" [
                                    Numeric.view' [NumericInputType.Slider] m.midtoneContrastAdjustment.gainFactor  
                                    |> UI.map SetMidtoneContrastGainFactor

                                    Numeric.view' [NumericInputType.InputBox] m.midtoneContrastAdjustment.gainFactor
                                    |> UI.map SetMidtoneContrastGainFactor
                                ]

                                Html.row "Black Clip:" [
                                    Numeric.view' [NumericInputType.InputBox] m.blackWhiteClip.blackClipPercentile
                                    |> UI.map SetBlackClipPercentile
                                ]

                                Html.row "White Clip:" [
                                    Numeric.view' [NumericInputType.InputBox] m.blackWhiteClip.whiteClipPercentile
                                    |> UI.map SetWhiteClipPercentile
                                ]
                            ]
                        ]
                    )

                    accordionHist "Selected Bands Spectral Analysis" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [                        
                        histogramsAndProfilesForCurrentMode
                    ]
                    
                    onlyForMultispectral (
                        div [style "display: flex; justify-content: flex-end; padding: 5px;"] [
                            button [
                                clazz "ui tiny inverted button"
                                onClick (fun _ -> ToggleCompleteSpectralProfile)
                            ] [
                                text "Compute Complete Spectral Profile"
                            ]
                        ]
                    )

                    onlyIfCompleteSpectralProfile (
                        accordionHist "Complete Spectral Profile" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [
                            spectralProfileOfAllBands                            
                        ]
                    )

                    onlyForMultispectral (
                        div [style $"border: 2px solid black; margin-top: 10px"] [
                            contentImages
                        ]
                    )
                    
                ]

            ]
            

        require Html.semui (
            body [] [

                div [] [
                    showAbsolute2DAnd3DImage m.sourceImagePath outputTexture   
                ]
                div [style "position: fixed; left: 20px; top: 20px; width: 400px"] [
                    accordion "Texture Mapping" "file image outline" false (clazz "ui inverted segment") [ content ]
                ]
            ])


    let viewFull (m : AdaptiveModel) = 
        let computeBoresight (b : AdaptiveBoresightAdjustment) : aval<Trafo3d> = 
            b.Current |> AVal.map (fun b -> 
                Trafo3d.RotationXInDegrees(b.yaw.value) * Trafo3d.RotationYInDegrees(b.pitch.value) * Trafo3d.RotationZInDegrees(b.roll.value)
            )
        let boresight = computeBoresight m.boresightAdjustment |> AVal.map Some
        let backgroundImageAnd3D = Image.view2DAnd3DImageAbsolute m.projectionOpacity.value boresight m.cameraState
        view m Image.view Image.view2DRelative backgroundImageAnd3D

    let app () =
        {
            initial = initial
            update = update
            view = viewFull
            threads = constF ThreadPool.empty
            unpersist = Unpersist.instance
        }