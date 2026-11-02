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
        transferFunctionMapping = TransferFunctionMapping.empty
        highlightAdjustment = HighlightAdjustment.init
        shadowAdjustment = ShadowAdjustment.init
        midtoneContrastAdjustment = MidtoneContrastAdjustment.init
        blackWhiteClip = BlackWhiteClip.init
        saturation = Saturation.init
        brightness = Brightness.init
        visualizationMode = VisualizationMode.RgbRatioComposite
        activeCategory = ActiveCategory.RgbImage
        loadCompleteSpectralProfile = false
        pixelDetectionEnabled = false
        pixelDetected = false
        imageWidth = 0
        imageHeight = 0
        clickedPixel = None
        viewportSize = V2i.Zero
        greyscaleColorMap = ColorMap.Viridis
        greyscaleBlackPoint = { Numeric.init with min = 0.0; max = 254.0; step = 1.0; value = 0.0 }
        greyscaleWhitePoint = { Numeric.init with min = 0.0; max = 255.0; step = 1.0; value = 255.0 }
        applyGreyscaleTransferFunction = false
        savedBandExpressions = []
        bandExpressionDraft = ""
        bandNames = []
        renamingBand = None
        analysisOpen = false
        transferSpline = TransferSpline.init
    }

    let private isSingleChannelImage (path : string) =
        let image = PixImage<byte>(path)
        image.ChannelCount = 1

    let initialFor (test : Option<TestConfig>) =
        match test with
        | None -> initial
        | Some t ->
            let m = Update.update initial (LoadMultispectralImage t.imagePath)
            let setValue (n : NumericInput) v = { n with value = v }
            { m with
                activeCategory = ActiveCategory.RgbImage   // required for the GPU path
                highlightAdjustment =
                    { m.highlightAdjustment with
                        amount = setValue m.highlightAdjustment.amount t.highlightAmount
                        tone   = setValue m.highlightAdjustment.tone   t.highlightTone
                        radius = setValue m.highlightAdjustment.radius t.highlightRadius }
                shadowAdjustment =
                    { m.shadowAdjustment with
                        amount = setValue m.shadowAdjustment.amount t.shadowAmount
                        tone   = setValue m.shadowAdjustment.tone   t.shadowTone
                        radius = setValue m.shadowAdjustment.radius t.shadowRadius } }
       
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

    let view
        (runtime : IRuntime)
        (m : AdaptiveModel)
        (showDOM : AdaptiveImage -> DomNode<ImageMessage>)
        (
            showRelative2DImage :
                aval<ITexture> ->
                aval<ITexture> ->
                aval<ITexture> ->
                aval<Option<V2i>> ->
                aval<int> ->
                aval<int> ->
                DomNode<Message>
        )
        (
            showAbsolute2DAnd3DImage :
                aval<Option<string>> ->
                aval<ITexture> ->                 // outputTexture
                aval<ITexture> ->                 // bandTexture
                aval<ITexture> ->                 // colormapTexture
                aval<ITexture> ->                 // splineTexture
                aval<ITexture> ->                 // midtoneTexture
                aval<float * float * bool * float * float * float> ->     // gpuSettings
                aval<float * float * float * float * float * float> ->     // shadowsHighlightsGpuSettings
                aval<bool> ->                     // useGpu
                aval<Option<V2i>> ->              // clickedPixel
                aval<int> ->                      // imageWidth
                aval<int> ->                      // imageHeight
                aval<bool> ->                     // useTransferFunctionGpu
                DomNode<Message>
        ) =
                
        let onlyIfAnalysisOpen (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! isOpen = m.analysisOpen
                        if isOpen then
                            yield node
                    }
                )

        let onlyForMultispectral (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! activeCategory = m.activeCategory

                        if activeCategory = ActiveCategory.MultispectralImage then
                            yield node
                    }
                )
        
        let onlyForTransferFunction (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! activeCategory = m.activeCategory
                        let! mode = m.visualizationMode

                        if activeCategory = ActiveCategory.MultispectralImage && mode = VisualizationMode.SingleBandTransferFunction then
                            yield node
                    }
                )

        let onlyForBandRatios (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! activeCategory = m.activeCategory
                        let! visMode = m.visualizationMode

                        if activeCategory = ActiveCategory.MultispectralImage && visMode = VisualizationMode.RgbRatioComposite then
                            yield node
                    }
                )

        let onlyForGreyscaleImage (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! activeCategory = m.activeCategory

                        if activeCategory = ActiveCategory.GreyscaleImage then
                            yield node
                    }
                )

    
        let onlyForPlainRGBImage (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! activeCategory = m.activeCategory

                        if activeCategory = ActiveCategory.RgbImage then
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

        let onlyIfPixelDetectionEnabled (node : DomNode<Message>) =
            Incremental.div
                AttributeMap.empty
                (
                    alist {
                        let! enabled = m.pixelDetectionEnabled
                        if enabled = true then
                            yield node
                    }
                )

        let bandRatioRenderSettings : BandRatioRenderSettings =
            {
                redNumeratorBand     = m.rgbRatioComposite.redNumeratorBand
                redDenominatorBand   = m.rgbRatioComposite.redDenominatorBand
                greenNumeratorBand   = m.rgbRatioComposite.greenNumeratorBand
                greenDenominatorBand = m.rgbRatioComposite.greenDenominatorBand
                blueNumeratorBand    = m.rgbRatioComposite.blueNumeratorBand
                blueDenominatorBand  = m.rgbRatioComposite.blueDenominatorBand
                gamma                = m.rgbRatioComposite.gamma.value

                redExpression        = m.rgbRatioComposite.redExpression.parsed
                greenExpression      = m.rgbRatioComposite.greenExpression.parsed
                blueExpression       = m.rgbRatioComposite.blueExpression.parsed
            }

        let transferFunctionRenderSettings : TransferFunctionRenderSettings =
            {
                selectedBands =
                    m.transferFunctionMapping.selectedBands
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
                blackWhiteClip =
                    AVal.custom (fun token ->
                        {
                            blackClipPercentile =
                                numericInputFromAdaptive token m.blackWhiteClip.blackClipPercentile
                            whiteClipPercentile =
                                numericInputFromAdaptive token m.blackWhiteClip.whiteClipPercentile
                        }
                    )
            }

        let adjustedPlainRgbImageAndMidMaskAndSaturation =
            RgbComposite.createPlainRgbPixImage
                m.sourceImagePath
                shadowsHighlightsAdjustmentsRenderSettings
        
        let adjustedPlainRgbImage = adjustedPlainRgbImageAndMidMaskAndSaturation |> AVal.map (Result.map (fun (image, _) -> image))
        let midMaskImage = adjustedPlainRgbImageAndMidMaskAndSaturation |> AVal.map (Result.map (fun (_, mask) -> mask))
        let midtoneTexture = RgbComposite.createPlainRgbTexture midMaskImage

        let bandTexture = 
            m.sourceImageKind
            |> AVal.bind (function
                | SourceImageKind.PlainRgbImage ->
                    Image.createRawGreyscaleTexture m.sourceImagePath 
                | SourceImageKind.Multispectral ->
                    Image.createSelectedBandTexture m.images transferFunctionRenderSettings)
        
        let colormapTexture = 
            m.sourceImageKind
            |> AVal.bind (function
                | SourceImageKind.PlainRgbImage ->
                    Image.createGreyscaleColormapTexture m.greyscaleColorMap
                | SourceImageKind.Multispectral ->
                    Image.createColormapTexture m.images transferFunctionRenderSettings)

        let splineTexture = Image.createSplineTexture m.transferSpline 256

        // <blackpoint, whitepoint, multispectral flag, brightness, midgain, saturation>
        let gpuSettings =
            AVal.custom (fun token ->
                match m.sourceImageKind.GetValue token with
                | SourceImageKind.PlainRgbImage ->
                    match m.activeCategory.GetValue token with
                    | ActiveCategory.GreyscaleImage ->
                        let blackPoint = m.greyscaleBlackPoint.value.GetValue token
                        let whitePoint = m.greyscaleWhitePoint.value.GetValue token

                        blackPoint / 255.0, whitePoint / 255.0, false, 0.0, 0.0, 0.0

                    | ActiveCategory.RgbImage ->
                        let brightness = m.brightness.gainFactor.value.GetValue token
                        let midpContrast = m.midtoneContrastAdjustment.gainFactor.value.GetValue token
                        let saturation = m.saturation.gainFactor.value.GetValue token

                        0.0, 1.0, false, brightness, midpContrast, saturation

                    | ActiveCategory.MultispectralImage ->
                        0.0, 1.0, true, 0.0, 0.0, 0.0

                | SourceImageKind.Multispectral ->
                    let selectedBands =
                        transferFunctionRenderSettings.selectedBands.GetValue token
                    let images = (AList.toAVal m.images).GetValue token
                    let selected =
                        images |> IndexList.toList
                        |> List.filter (fun i -> List.contains (i.bandIndex.GetValue token) selectedBands)

                    match selected with
                    | first :: _ ->
                        (selected |> List.map (fun i -> i.inputMinValue.value.GetValue token) |> List.min),
                        (selected |> List.map (fun i -> i.inputMaxValue.value.GetValue token) |> List.max),
                        not (first.useFalseColor.GetValue token), 0.0, 0.0, 0.0
                    | [] -> 0.0, 1.0, true, 0.0, 0.0, 0.0

            )
            
        // <highlightsAmount, highlightsTone, highlightRadius, shadowsAmount, shadowsTone, shadowRadius>
        let shadowsHighlightsGpuSettings =
            AVal.custom (fun token ->
                let highlightsAmount = m.highlightAdjustment.amount.value.GetValue token
                let highlightsTone = m.highlightAdjustment.tone.value.GetValue token    
                let highlightRadius = m.highlightAdjustment.radius.value.GetValue token
                let shadowsAmount = m.shadowAdjustment.amount.value.GetValue token
                let shadowsTone = m.shadowAdjustment.tone.value.GetValue token
                let shadowRadius = m.shadowAdjustment.radius.value.GetValue token

                highlightsAmount,
                highlightsTone,
                highlightRadius,
                shadowsAmount,
                shadowsTone,
                shadowRadius
            )

        let useGpu =
            AVal.custom (fun token ->
                match m.sourceImageKind.GetValue token with
                | SourceImageKind.PlainRgbImage ->
                    (m.activeCategory.GetValue token = ActiveCategory.GreyscaleImage 
                    && m.applyGreyscaleTransferFunction.GetValue token
                    &&
                        (match m.sourceImagePath.GetValue token with
                        | Some path -> File.Exists path && isSingleChannelImage path
                        | None -> false))
                    ||
                    (m.activeCategory.GetValue token = ActiveCategory.RgbImage
                    &&
                        (match m.sourceImagePath.GetValue token with
                        | Some path -> File.Exists path
                        | None -> false))

                | SourceImageKind.Multispectral ->
                    m.visualizationMode.GetValue token =
                        VisualizationMode.SingleBandTransferFunction
            )

        let useTransferFunctionGpu =
            AVal.custom (fun token ->
                match m.sourceImageKind.GetValue token with
                | SourceImageKind.Multispectral ->
                    m.visualizationMode.GetValue token =
                        VisualizationMode.SingleBandTransferFunction

                | SourceImageKind.PlainRgbImage ->
                    m.activeCategory.GetValue token = ActiveCategory.GreyscaleImage
                    && m.applyGreyscaleTransferFunction.GetValue token
                    &&
                        match m.sourceImagePath.GetValue token with
                        | Some path ->
                            File.Exists path && isSingleChannelImage path
                        | None ->
                            false
            )

        let cpuOutputTexture : aval<ITexture> =
            m.sourceImageKind
            |> AVal.bind (fun sourceKind ->
                match sourceKind with
                | SourceImageKind.PlainRgbImage ->
                    m.activeCategory
                    |> AVal.bind (fun category ->
                        if category = ActiveCategory.GreyscaleImage then
                            m.sourceImagePath
                            |> AVal.bind (function
                                | Some path when File.Exists path && isSingleChannelImage path ->
                                    Image.createStretchedGreyscaleTexture
                                        m.sourceImagePath
                                        m.greyscaleBlackPoint.value
                                        m.greyscaleWhitePoint.value
                                        m.applyGreyscaleTransferFunction
                                        m.greyscaleColorMap
                                        
                                | _ ->
                                    RgbComposite.createPlainRgbTexture adjustedPlainRgbImage)
                        else
                            RgbComposite.createPlainRgbTexture adjustedPlainRgbImage
                    )
                | SourceImageKind.Multispectral ->
                    m.visualizationMode
                    |> AVal.bind (fun mode ->
                        match mode with
                        | VisualizationMode.RgbRatioComposite ->
                            Image.createBandRatioTexture
                                m.images
                                bandRatioRenderSettings

                        | VisualizationMode.SingleBandTransferFunction ->
                            AVal.constant (DefaultTextures.checkerboard.GetValue())
                    )
            )

        let outputTexture : aval<ITexture> =
            useTransferFunctionGpu
            |> AVal.bind (fun gpuTransferFunction ->
                if gpuTransferFunction then
                    Log.warn "Using GPU transfer function for rendering"
                    AVal.constant (DefaultTextures.checkerboard.GetValue())
                else
                    Log.warn "Using CPU transfer function for rendering"
                    cpuOutputTexture
            )

        let rgbRatioSelectedBandHistograms =
            computeRgbRatioSelectedBandHistograms m 32

        let transferFunctionSelectedBandHistogram =
            computeTransferFunctionSelectedBandHistogram m 32

        let gpuAdjustedImage =
            Image.createGpuAdjustedTexture
                runtime
                (RgbComposite.createPlainRgbTexture adjustedPlainRgbImage)
                midtoneTexture
                gpuSettings
                shadowsHighlightsGpuSettings
                m.imageWidth
                m.imageHeight
            |> Image.downloadAdjustedImage runtime

        let transferFunctionNonMultispectralRgbHistograms =
            computeNonMultispectralRgbHistograms gpuAdjustedImage 32

        let greyscaleHistogram =
            computeGreyscaleHistogram m 256

        let rgbSelectedBandSpectralProfiles =
            computeRgbSpectralProfiles m 

        let allSpectralProfiles = 
            computeCompleteSpectralProfile m

        let pixelSpectralProfile =
            computePixelSpectralProfile m "clicked pixel"

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

                                    completeSpectralProfileView
                                        allSpectralProfiles
                                ]
                    }
                )
        
        let spectralProfileOfPixel =
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
                                        text "Spectral Profile of clicked pixel"
                                    ]

                                    localSpectralProfileView
                                        pixelSpectralProfile
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
                                                rgbSelectedBandSpectralProfiles true
                                        ]

                                yield
                                    selectedHistogramsView
                                        "Currently selected RGB band histograms and spectral profiles"
                                        rgbRatioSelectedBandHistograms
                                
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
                                                rgbSelectedBandSpectralProfiles false
                                        ]

                                yield
                                    selectedHistogramsView
                                            "Selected transfer-function band histogram and spectral profile"
                                            transferFunctionSelectedBandHistogram
                        else 
                            yield
                                combinedRgbHistogramsView
                                    "Original image RGB channel histograms"
                                    transferFunctionNonMultispectralRgbHistograms
                    }
                )

        let savedBandExpressionsList =
            let removeButtonStyle =
                "cursor: pointer; margin-left: 8px; padding: 0 6px; color: #f88; font-weight: bold; user-select: none;"
 
            div [style "padding: 5px;"] [
                // input + Add button
                div [style "display: flex; align-items: center; gap: 6px; margin-bottom: 8px;"] [
                    Incremental.input (
                        AttributeMap.ofAMap (amap {
                            yield attribute "type" "text"
                            yield attribute "placeholder" "e.g. B6/B4"
                            yield style "flex: 1; font-family: monospace;"
                            let! t = m.bandExpressionDraft
                            yield attribute "value" t
                            yield onChange (fun s -> SetBandExpressionDraft s)
                        })
                    )
 
                    button [
                        clazz "ui tiny inverted button"
                        attribute "type" "button"
                        onClick (fun _ -> AddSavedBandExpression)
                    ] [
                        text "Add"
                    ]
                ]
 
                // saved expressions
                Incremental.div
                    (AttributeMap.ofList [
                        style "max-height: calc(100vh - 300px); overflow: auto;"
                    ])
                    (
                        alist {
                            let! expressions = m.savedBandExpressions
 
                            for index, expression in List.indexed expressions do
                                yield
                                    div [
                                        style "display: flex; justify-content: space-between; align-items: center; padding: 4px 6px; border: 1px solid rgba(255,255,255,0.3); margin-bottom: 4px;"
                                    ] [
                                        span [style "font-family: monospace;"] [
                                            text expression
                                        ]
 
                                        span [
                                            style removeButtonStyle
                                            attribute "title" "Remove expression"
                                            onClick (fun _ -> RemoveSavedBandExpression index)
                                        ] [
                                            text "x"
                                        ]
                                    ]
                        }
                    )
            ]
 
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

        let accordionAnalysis text' icon active styling content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"

            onBoot "$('#__ID__ > .ui.accordion').accordion({ exclusive: false, selector: { title: '> .title', content: '> .content' } });" (
                div styling [
                    div [clazz "ui inverted accordion fluid"] [
                        div [
                            clazz title
                            style "background-color: #282828"
                            onClick (fun _ -> ToggleAnalysisOpen)
                        ] [
                            i [clazz "dropdown icon"] []
                            text text'
                            div [style "float:right"] [i [clazz (icon + " icon")] []]
                        ]
                        div [clazz content; style "overflow-y : auto; "] content'
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
            let attributesName = attribute "style" $"cursor: pointer; width: 120px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center; overflow: hidden; white-space: nowrap; text-overflow: ellipsis;"
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
                                yield div [attributesName] [text "Band"]

                            | VisualizationMode.SingleBandTransferFunction ->
                                yield div [attributesName] [text "Band"]

                            yield div [attributesWavelength] [text "Wavelengths"]

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
                                            Incremental.div
                                                (AttributeMap.ofList [
                                                    attribute "style" "display: flex; font-weight: bold"
                                                ])
                                                (
                                                    alist {
                                                        let! mode = m.visualizationMode
                                                        let! bandIdx  = img.bandIndex
                                                        let! names    = m.bandNames
                                                        let! renaming = m.renamingBand

                                                        let bandNameCell (cellStyle : Attribute<Message>) =
                                                            if renaming = Some bandIdx then
                                                                let current = BandNames.tryFind names bandIdx
                                                                div [cellStyle] [
                                                                    input [
                                                                        attribute "type" "text"
                                                                        attribute "value" (defaultArg current "")
                                                                        attribute "placeholder" (sprintf "Band %d" (bandIdx + 1))
                                                                        attribute "autofocus" "autofocus"
                                                                        style "width: 70px; font-family: monospace;"
                                                                        onChange (fun s -> RenameBand (bandIdx, s))
                                                                    ]
                                                                    if current.IsSome then
                                                                        span [
                                                                            style "margin-left: 4px; color: #f88; cursor: pointer;"
                                                                            attribute "title" "Reset to default name"
                                                                            onClick (fun _ -> ResetBandName bandIdx)
                                                                        ] [ text "reset" ]
                                                                    span [
                                                                        style "margin-left: 4px; color: #aaa; cursor: pointer;"
                                                                        attribute "title" "Cancel"
                                                                        onClick (fun _ -> CancelRenameBand)
                                                                    ] [ text "x" ]
                                                                ]
                                                            else
                                                                div [
                                                                    cellStyle
                                                                    attribute "title" "Right-click to rename"
                                                                    clientEvent "oncontextmenu" "aardvark.processEvent('__ID__', 'onrightclick'); return false;"
                                                                    onEvent "onrightclick" [] (fun _ -> StartRenameBand bandIdx)
                                                                ] [ text (BandNames.label names bandIdx) ]

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

                                                        let bandNameDisplay 
                                                            (selected : aval<Option<int>>) =
                                                            div [attributesSelect]
                                                                [
                                                                    Incremental.text (
                                                                        (img.texture, img.bandIndex)
                                                                        ||> AVal.map2 (fun path bandIndex ->
                                                                            let bandNumber = bandIndex + 1

                                                                            sprintf "Band %d"
                                                                                    bandNumber

                                                                        )
                                                                    )

                                                                ]

                                                        match mode with
                                                        | VisualizationMode.RgbRatioComposite ->
                                                            yield bandNameCell attributesName

                                                        | VisualizationMode.SingleBandTransferFunction ->
                                                            yield bandNameCell attributesName
                                                            
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
                                                    }
                                                )
                                        
                                        ]
                                    )
                                for domNode in domNodes do
                                    yield domNode
                        })
                    ]
                })

        let greyscaleCounts : aval<int[]> =
            AVal.custom (fun token ->
                let counts = Array.zeroCreate<int> 256
                let sourceKind = m.sourceImageKind.GetValue token

                let black =
                    m.greyscaleBlackPoint.value.GetValue token

                let white =
                    max
                        (black + 1.0)
                        (m.greyscaleWhitePoint.value.GetValue token)

                // This implementation is only valid for ordinary image files.
                // Multispectral datasets use BandHandler instead.
                match sourceKind, m.sourceImagePath.GetValue token with
                | SourceImageKind.PlainRgbImage, Some filePath
                    when File.Exists filePath && isSingleChannelImage filePath ->

                    let image = PixImage<byte>(filePath)
                    let pixels = image.GetMatrix<byte>()

                    for y in 0 .. image.Size.Y - 1 do
                        for x in 0 .. image.Size.X - 1 do
                            let grey = float pixels.[x, y]

                            let stretched =
                                255.0
                                * ImageMath.clamp01
                                    ((grey - black) / (white - black))
                                |> round
                                |> int

                            counts.[stretched] <-
                                counts.[stretched] + 1

                | _ ->
                    // Unsupported source type: the optional feature does nothing.
                    ()

                counts
            )

        let greyscaleHistogramView =
            Incremental.div AttributeMap.empty (
                alist {
                    let! sourceKind = m.sourceImageKind

                    match sourceKind with
                    | SourceImageKind.Multispectral ->
                        // Uses BandHandler, so NetCDF, MBI and TIFF datasets work.
                        yield
                            selectedHistogramsView
                                "Selected band grey-value histogram"
                                greyscaleHistogram

                    | SourceImageKind.PlainRgbImage ->
                        let! counts = greyscaleCounts
                        let maxCount = counts |> Array.max |> max 1

                        yield
                            div [] [
                                text "Pixels"

                                div [
                                    style "height: 120px; display: flex; align-items: flex-end; border-left: 1px solid #aaa; border-bottom: 1px solid #aaa;"
                                ] [
                                    for grey in 0 .. 255 do
                                        div [
                                            attribute
                                                "title"
                                                (sprintf
                                                    "Grey value %d: %d pixels"
                                                    grey
                                                    counts.[grey])

                                            style (
                                                sprintf
                                                    "flex: 1; min-width: 1px; height: %.2f%%; background: #aaa;"
                                                    (100.0
                                                     * float counts.[grey]
                                                     / float maxCount)
                                            )
                                        ] []
                                ]

                                div [
                                    style "display: flex; justify-content: space-between;"
                                ] [
                                    text "0 (black)"
                                    text "Grey value"
                                    text "255 (white)"
                                ]
                            ]
                }
            )

        let expressionRow (label : string) (channel : RgbChannel) (input : AdaptiveBandExpressionInput) =
            div [style "margin-bottom: 6px;"] [
                div [style "display: flex; align-items: center; gap: 6px;"] [
                    span [style "width: 20px;"] [text label]
                    Incremental.input (
                        AttributeMap.ofAMap (amap {
                            yield attribute "type" "text"
                            yield attribute "placeholder" "e.g. B6/B4"
                            yield style "flex: 1; font-family: monospace;"
                            let! t = input.text
                            yield attribute "value" t
                            yield onChange (fun s -> SetBandRatioExpression (channel, s))
                        })
                    )
                    onBoot
                        "$('#__ID__').on('change', function () { var s = this; setTimeout(function () { s.selectedIndex = 0; }, 0); });"
                        (
                            Incremental.select
                                (AttributeMap.ofList [
                                    style "width: 25px; font-family: monospace;"
                                    attribute "title" "Saved band expressions"
                                    onChange (fun s ->
                                        if String.IsNullOrEmpty s then Nop
                                        else SetBandRatioExpression (channel, s))
                                ])
                                (alist {
                                    yield option [attribute "value" ""] [text ""]

                                    let! saved = m.savedBandExpressions

                                    for expression in saved do
                                        yield option [attribute "value" expression] [text expression]
                                })
                        )
                ]
                Incremental.div AttributeMap.empty (alist {
                    match! input.error with
                    | Some e -> yield div [style "color: #f88; font-size: 10px; white-space: pre-wrap;"] [text e]
                    | None -> ()
                    match! input.info with
                    | Some i -> yield div [style "color: #aaa; font-size: 10px;"] [text i]
                    | None -> ()
                })
            ]

        let bandRatioExpressionEditor =
            Incremental.div AttributeMap.empty (alist {
                let! mode = m.visualizationMode
                if mode = VisualizationMode.RgbRatioComposite then
                    yield div [clazz "item"; style "padding: 5px;"] [
                        expressionRow "R" RgbChannel.Red   m.rgbRatioComposite.redExpression
                        expressionRow "G" RgbChannel.Green m.rgbRatioComposite.greenExpression
                        expressionRow "B" RgbChannel.Blue  m.rgbRatioComposite.blueExpression
                    ]
            })

        
        let tf = m.transferFunctionMapping

        // colormap + false-color state of the reference band (first band in the interval)
        let transferFunctionReference : aval<ColorMap * bool> =
            AVal.custom (fun token ->
                let selected = tf.selectedBands.GetValue token
                let images = (AList.toAVal m.images).GetValue token
                match BandHandler.tryFindReferenceImage images token selected with
                | Some image -> image.colorMap.GetValue token, image.useFalseColor.GetValue token
                | None -> ColorMap.Viridis, false)

        let transferFunctionEditor =
            div [] [
                Html.table [
                    Html.row "False Color:" [
                        Html.SemUi.dropDown
                            (transferFunctionReference |> AVal.map fst)
                            SetTransferFunctionColorMap
                    ]
                ]
                Incremental.div AttributeMap.empty (
                    alist {
                        let! (colorMap, useFalseColor) = transferFunctionReference
                        if useFalseColor then
                            yield
                                img [
                                    attribute "src" (Image.colorMapDataUrl colorMap)
                                    style "display: block; width: 100%; height: 28px; margin-top: 10px;"
                                ]
                    })
            ]

        let wavelengthIntervalSelector =
            Incremental.div (AttributeMap.ofList [ clazz "item"; style "padding: 5px;"]) (
                alist {
                    match! tf.availableSpan with
                    | None -> 
                        yield div [ style "color: #f88;" ] [ text "No wavelength metadata available for these bands." ]
                    | Some (lo, hi) ->
                        let! bands = tf.selectedBands
                        let! fallback = tf.usedNearestFallback
                        yield div [] [ text (sprintf "Available wavelength span: %.1f - %.1f nm" lo hi) ]
                        yield transferFunctionEditor
                        yield 
                            Html.table [
                                Html.row "Min (nm):" [
                                    Numeric.view' [NumericInputType.InputBox] tf.wavelengthMin |> UI.map SetWavelengthMin ]
                                Html.row "Max (nm):" [
                                    Numeric.view' [NumericInputType.InputBox] tf.wavelengthMax |> UI.map SetWavelengthMax ]
                            ]
                        let! bands = tf.selectedBands
                        let! fallback = tf.usedNearestFallback
                        yield
                            div [ style "font-size: 11px; color: #aaa;" ] [
                                text (
                                    if fallback then sprintf "No band in interval, using nearest (%d band)." bands.Length
                                    else sprintf "%d band(s) in interval, averaged." bands.Length)
                            ]
                })

        let content = 
            div [style "overlow-y: auto; max-height: calc(100vh - 20vh);"] [

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

                    onlyForMultispectral (
                        div [] [
                            visualizationModeSelector
                            bandRatioExpressionEditor
                        ]
                    )

                    onlyForTransferFunction (
                        wavelengthIntervalSelector
                    )
                     
                    onlyForTransferFunction (
                        accordionLists "Transfer Function Curve" "chart line" false
                            [clazz "item"; style "margin-top: 10px;"]
                            ResetSpline
                            [
                                SplineEditor.view m.transferSpline
                                div [style "font-size: 11px; color: #aaa; padding: 6px 5px;"] [
                                    text "Drag points and the orange tangent handles. Double-click empty space to add a point, right-click a point to remove it, double-click a handle to flatten its tangent."
                                ]
                            ]
                    )

                    onlyForGreyscaleImage (
                        accordionHist
                            "Grey-value histogram"
                            "chart bar"
                            false
                            [clazz "item"; style "margin-top: 10px;"]
                            [greyscaleHistogramView]
                    )

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
                    
                    onlyForMultispectral (
                        accordionAnalysis "Selected Bands Spectral Analysis" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [
                            onlyIfAnalysisOpen histogramsAndProfilesForCurrentMode
                        ]
                    )

                    onlyForBandRatios (
                        accordionHist "Saved Band Ratio Expressions" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [
                            savedBandExpressionsList
                        ]
                    )

                    onlyForPlainRGBImage (
                        accordionHist "RGB Histogram" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [                        
                            histogramsAndProfilesForCurrentMode
                        ]
                    )
                    
                    onlyForGreyscaleImage (
                        accordionHist "Edit" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [
                            div [] [
                                Html.table [
                                
                                    Html.row "Black point:" [
                                        Numeric.view'
                                            [NumericInputType.Slider]
                                            m.greyscaleBlackPoint
                                        |> UI.map SetGreyscaleBlackPoint
                                    ]

                                    Html.row "White point:" [
                                        Numeric.view'
                                            [NumericInputType.Slider]
                                            m.greyscaleWhitePoint
                                        |> UI.map SetGreyscaleWhitePoint
                                    ]

                                    Html.row "Apply transfer function:" [
                                        Incremental.div AttributeMap.empty (
                                            alist {
                                                let! enabled = m.applyGreyscaleTransferFunction

                                                yield
                                                    input [
                                                        attribute "type" "checkbox"
                                                        onClick (fun _ -> ToggleGreyscaleTransferFunction)

                                                        if enabled then
                                                            attribute "checked" "checked"
                                                    ]
                                            }
                                        )
                                    ]

                                    Html.row "Transfer function:" [
                                        Html.SemUi.dropDown
                                            m.greyscaleColorMap
                                            SetGreyscaleColorMap
                                   
                                    ]
                                ]

                                Incremental.div
                                    AttributeMap.empty
                                    (
                                        alist {
                                            let! colorMap = m.greyscaleColorMap

                                            yield
                                                img [
                                                    attribute
                                                        "src"
                                                        (Image.colorMapDataUrl colorMap)

                                                    style
                                                        "display: block; width: 100%; height: 28px; margin-top: 10px;"
                                                ]
                                        }
                                    )
                            ]
                        ]
                    )

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
                        div [style "display: flex; justify-content: flex-end; padding: 5px;"] [
                            button [
                                clazz "ui tiny inverted button"
                                onClick (fun _ -> TogglePixelDetection)
                            ] [
                                text "Toggle Pixel Detection"
                            ]
                        ]
                    )

                    onlyIfPixelDetectionEnabled (
                        accordionHist "Spectral Profile at Pixel" "sliders horizontal" false [clazz "item"; style "margin-top: 10px;"] [
                            spectralProfileOfPixel
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
                    showAbsolute2DAnd3DImage
                        m.sourceImagePath
                        outputTexture
                        bandTexture
                        colormapTexture
                        splineTexture
                        midtoneTexture
                        gpuSettings
                        shadowsHighlightsGpuSettings
                        useGpu
                        m.clickedPixel
                        m.imageWidth
                        m.imageHeight
                        useTransferFunctionGpu
                ]
                div [style "position: fixed; left: 20px; top: 20px; width: 400px"] [
                    Incremental.div
                        (AttributeMap.ofList [
                            style "display: flex; gap: 6px; margin-bottom: 10px;"
                        ])
                        (
                            alist {
                                let! selected = m.activeCategory

                                for label, category in 
                                    [
                                        "Greyscale", ActiveCategory.GreyscaleImage
                                        "RGB", ActiveCategory.RgbImage
                                        "Multispectral", ActiveCategory.MultispectralImage
                                    ] do

                                    let background = if selected = category then "#000" else "#777"

                                    yield button [
                                        attribute "type" "button"
                                        onClick (fun _ -> SetActiveCategory category)
                                        style (sprintf
                                            "flex: 1; padding: 8px; background: %s; color: white; border: 1px solid #999; border-radius: 4px; cursor: pointer;"
                                            background)
                                    ] [ text label ]

                            }
                        )
                    accordion "Texture Mapping" "file image outline" false (clazz "ui inverted segment") [ content ]
                ]
            ])

    let viewFull (runtime : IRuntime) (m : AdaptiveModel) = 
        Log.warn "viewFull called"
        let computeBoresight (b : AdaptiveBoresightAdjustment) : aval<Trafo3d> = 
            b.Current |> AVal.map (fun b -> 
                Trafo3d.RotationXInDegrees(b.yaw.value) * Trafo3d.RotationYInDegrees(b.pitch.value) * Trafo3d.RotationZInDegrees(b.roll.value)
            )
        let boresight = computeBoresight m.boresightAdjustment |> AVal.map Some
        let backgroundImageAnd3D = Image.view2DAnd3DImageAbsolute runtime m.projectionOpacity.value boresight m.cameraState
        view runtime m Image.view (Image.view2DRelative runtime) backgroundImageAnd3D

    let app (runtime : IRuntime) (testConfig : Option<TestConfig>) =
        {
            initial = initialFor testConfig
            update = Update.update
            view = viewFull runtime
            threads = constF ThreadPool.empty
            unpersist = Unpersist.instance
        }