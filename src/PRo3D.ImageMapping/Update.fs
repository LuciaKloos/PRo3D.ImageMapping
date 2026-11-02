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

module Update =


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
      
    let private loadedLogicalBandIndices (images : IndexList<Image>) =
        images
        |> IndexList.toList
        |> List.map (fun img -> img.bandIndex)
        |> List.sort
        
    let private defaultTransferFunctionMappingForImages images =
        let bands =
            loadedLogicalBandIndices images

        {
            TransferFunctionMapping.empty with
                selectedBands = bands
        }
    
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

    
    let private applyBandNames (m : Model) names =
        { m with
            bandNames = names
            renamingBand = None
            rgbRatioComposite =
                m.rgbRatioComposite
                |> RgbRatioComposite.relabelExpressions (BandNames.exprLabel names) }

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
            BandHandler.clearDecodedRgbImageCache ()
            
            let fullPath =
                Path.GetFullPath path

            if isPlainRgbImagePath fullPath then
                match BandHandler.readCachedRgbImagePayload fullPath with
                | Result.Error error ->
                    Log.warn "Could not decode plain RGB image: %s" error
                    m

                | Result.Ok payload ->
                    {
                        m with
                            images = IndexList.Empty
                            selectedImage = None
                            sourceImagePath = Some fullPath
                            sourceImageKind = SourceImageKind.PlainRgbImage
                            imageWidth = payload.width
                            imageHeight = payload.height
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
                    let firstLoadedBand =
                        loadedBands |> List.head

                    let firstBandSource : RgbBandSource =
                        {
                            logicalIndex = firstLoadedBand.bandIndex
                            filePath = firstLoadedBand.texture
                            channelIndex = firstLoadedBand.selectedChannel.idx
                            wavelength = firstLoadedBand.wavelength
                        }

                    let width, height =
                        match BandHandler.readBandSourceAsFloat firstBandSource with
                        | Result.Ok bandData ->
                            bandData.width, bandData.height

                        | Result.Error error ->
                            Log.warn
                                "Could not determine image dimensions: %s"
                                error

                            0, 0

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
                            imageWidth = width
                            imageHeight = height
                            editImages = []
                            rgbRatioComposite = defaultRgbComposite |> RgbRatioComposite.syncAllExpressions (BandNames.exprLabel [])
                            transferFunctionMapping = TransferFunctionMapping.initFor (IndexList.toList bands) TransferFunctionMapping.empty
                            visualizationMode = VisualizationMode.RgbRatioComposite
                            loadCompleteSpectralProfile = false
                            bandExpressionDraft = ""
                            bandNames = []
                            renamingBand = None
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
                            |> RgbRatioComposite.syncExpression(BandNames.exprLabel m.bandNames) rgbChannel
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
                        rgbRatioComposite = defaultRgbRatioCompositeForImages m.images |> RgbRatioComposite.syncAllExpressions(BandNames.exprLabel m.bandNames)
                        editImages = []
                }

            | VisualizationMode.SingleBandTransferFunction ->
                {
                    m with
                        visualizationMode = mode
                        transferFunctionMapping = 
                            if List.isEmpty m.transferFunctionMapping.selectedBands then
                                TransferFunctionMapping.initFor (IndexList.toList m.images) m.transferFunctionMapping
                            else m.transferFunctionMapping
                        editImages = []
                }

        | ToggleCompleteSpectralProfile ->
             { m with loadCompleteSpectralProfile = not m.loadCompleteSpectralProfile }

        | TogglePixelDetection ->
            let enabled = not m.pixelDetectionEnabled

            {
                m with
                    pixelDetectionEnabled = enabled
                    clickedPixel =
                        if enabled then m.clickedPixel
                        else None
            }

        | SetPixelDetected detected ->
            { m with pixelDetected = detected }

        | ImageClicked (position, viewportSize) ->
            if m.pixelDetectionEnabled then
                let viewportWidth  = float viewportSize.X
                let viewportHeight = float viewportSize.Y

                if
                    viewportWidth > 0.0 &&
                    viewportHeight > 0.0 &&
                    m.imageWidth > 0 &&
                    m.imageHeight > 0
                then
                    let viewportAspect =
                        viewportWidth / viewportHeight

                    let left   = -1.0
                    let right  =  0.0
                    let top    =  0.0
                    let bottom = -1.0 / viewportAspect

                    let u =
                        (position.X - left) / (right - left)

                    let v =
                        (top - position.Y) / (top - bottom)

                    let x =
                        int (u * float m.imageWidth)
                        |> max 0
                        |> min (m.imageWidth - 1)

                    let y =
                        int (v * float m.imageHeight)
                        |> max 0
                        |> min (m.imageHeight - 1)

                    Log.warn
                        "Clicked pixel: x=%d, y=%d, viewport=%A, aspect=%f"
                        x y viewportSize viewportAspect

                    {
                        m with
                            pixelDetected = true
                            clickedPixel = Some(V2i(x, y))
                            viewportSize = viewportSize
                    }
                else
                    m
            else
                m

        | SetGreyscaleColorMap colorMap ->
            { m with greyscaleColorMap = colorMap }

        | SetGreyscaleBlackPoint action ->
            let input = Numeric.update m.greyscaleBlackPoint action
            let blackPoint = min input.value (m.greyscaleWhitePoint.value - 1.0)
            { m with
                greyscaleBlackPoint = { input with value = blackPoint }
            }

        | SetGreyscaleWhitePoint action ->
            let input = Numeric.update m.greyscaleWhitePoint action
            let whitePoint = max input.value (m.greyscaleBlackPoint.value + 1.0)
            { m with
                greyscaleWhitePoint = { input with value = whitePoint }
            }        

        | ToggleGreyscaleTransferFunction ->
            { m with 
                applyGreyscaleTransferFunction = not m.applyGreyscaleTransferFunction
            }

        | SetBandRatioExpression (channel, text) ->
            // parse → resolveWavelengths → validateBands → "uses ≥1 band" check → toRatio

            let available = loadedLogicalBandIndices m.images |> Set.ofList

            let candidates =
                m.images |> IndexList.toList
                |> List.choose (fun img ->
                    img.wavelength |> Option.filter Double.IsFinite
                    |> Option.map (fun w -> img.bandIndex, w))

            // parse + check that every referenced band is loaded + at least one band is used
            let parsed =
                BandExpression.parse text
                |> Result.bind (BandExpression.rejectRenamedBands m.bandNames)
                |> Result.bind (BandExpression.resolveNames m.bandNames)
                |> Result.bind (BandExpression.resolveWavelengths candidates)
                |> Result.bind (fun (expr, resolved) ->
                    BandExpression.validateBands available expr
                    |> Result.bind ( fun expr ->
                        if Set.isEmpty (BandExpression.referenceBands expr) then
                            Result.Error "The expression must use at least one band, e.g. B6."
                        else
                            Result.Ok (expr, resolved)))
                    

            // plain "Bx" or "Bx/By" also moves the band checkboxes; anything else leaves them alone
            let simpleBands =
                match parsed with
                | Result.Ok (expr, _) ->
                    match BandExpression.toRatio available expr with
                    | Result.Ok (num, den) -> Some (num, den)
                    | Result.Error _ -> None
                | Result.Error _ -> None

            let withInput (input : BandExpressionInput) =
                match parsed with
                | Result.Ok (expr, resolved) ->
                    let info =
                        match resolved with
                        | [] -> None
                        | rs ->
                            rs
                            |> List.map (fun r ->
                                sprintf "w%g → %s (%g nm)" r.requested
                                    (BandNames.exprLabel m.bandNames (r.bandNumber - 1)) r.actual)
                            |> String.concat "; "
                            |> Some
                    { text = text; error = None; info = info; parsed = Some expr }

                | Result.Error e ->
                    { input with text = text; error = Some e; info = None }   // clear stale info on error

            let c = m.rgbRatioComposite

            let c' =
                match channel, simpleBands with
                | RgbChannel.Red, Some (num, den) ->
                    { c with redNumeratorBand = Some num; redDenominatorBand = den }
                | RgbChannel.Green, Some (num, den) ->
                    { c with greenNumeratorBand = Some num; greenDenominatorBand = den }
                | RgbChannel.Blue, Some (num, den) ->
                    { c with blueNumeratorBand = Some num; blueDenominatorBand = den }
                | _ -> c

            let c'' =
                match channel with
                | RgbChannel.Red   -> { c' with redExpression = withInput c'.redExpression }
                | RgbChannel.Green -> { c' with greenExpression = withInput c'.greenExpression }
                | RgbChannel.Blue  -> { c' with blueExpression = withInput c'.blueExpression }

            { m with rgbRatioComposite = c'' }

        | SetBandExpressionDraft text ->
            { m with bandExpressionDraft = text }
 
        | AddSavedBandExpression ->
            let text = m.bandExpressionDraft.Trim()
 
            if String.IsNullOrWhiteSpace text || List.contains text m.savedBandExpressions then
                m
            else
                { m with
                    savedBandExpressions = m.savedBandExpressions @ [ text ]
                    bandExpressionDraft = ""
                }
 
        | RemoveSavedBandExpression index ->
            { m with
                savedBandExpressions =
                    m.savedBandExpressions
                    |> List.indexed
                    |> List.filter (fun (i, _) -> i <> index)
                    |> List.map snd
            }
        | StartRenameBand idx -> { m with renamingBand = Some idx }
        | CancelRenameBand    -> { m with renamingBand = None }

        | RenameBand (idx, text) ->
            match BandNames.tryRename m.bandNames idx text with
            | Result.Ok names -> applyBandNames m names
            | Result.Error e ->
                Log.warn "Could not rename band: %s" e
                { m with renamingBand = None }

        | ResetBandName idx ->
            applyBandNames m (BandNames.reset idx m.bandNames)

        | SetWavelengthMin action ->
            Log.warn "SetWavelengthMin %A" action
            let tf = m.transferFunctionMapping
            let lo = (Numeric.update tf.wavelengthMin action).value
            let hi = max tf.wavelengthMax.value lo
            { m with
                transferFunctionMapping =
                    { tf with
                        wavelengthMin = { tf.wavelengthMin with value = lo }
                        wavelengthMax = { tf.wavelengthMax with value = hi } }
                    |> TransferFunctionMapping.recompute (IndexList.toList m.images) }

        | SetWavelengthMax action ->
            Log.warn "SetWavelengthMax %A" action
            let tf = m.transferFunctionMapping
            let hi = (Numeric.update tf.wavelengthMax action).value
            let lo = min tf.wavelengthMin.value hi
            { m with
                transferFunctionMapping =
                    { tf with
                        wavelengthMin = { tf.wavelengthMin with value = lo }
                        wavelengthMax = { tf.wavelengthMax with value = hi } }
                    |> TransferFunctionMapping.recompute (IndexList.toList m.images) }

        | SetTransferFunctionColorMap colorMap ->
            { m with
                images =
                    m.images
                    |> IndexList.map (fun img -> Image.update img (SetColorMap colorMap)) }
        | ToggleAnalysisOpen ->
            { m with analysisOpen = not m.analysisOpen }

        | SetActiveCategory category ->
            { m with activeCategory = category; analysisOpen = false }

        | SplineMovePoint (index, x, y) ->
            { m with transferSpline = TransferSpline.movePoint index x y m.transferSpline }

        | SplineSetTangent (index, slope) ->
            { m with transferSpline = TransferSpline.setTangent index slope m.transferSpline }

        | SplineResetTangent index ->
            { m with transferSpline = TransferSpline.resetTangent index m.transferSpline }

        | SplineAddPoint (x, y) ->
            { m with transferSpline = TransferSpline.addPoint x y m.transferSpline }

        | SplineRemovePoint index ->
            { m with transferSpline = TransferSpline.removePoint index m.transferSpline }

        | ResetSpline ->
            { m with transferSpline = TransferSpline.init }
