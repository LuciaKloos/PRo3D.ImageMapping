namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model
open PRo3D.ImageMapping.BandHandler

module SpectralAnalysis =
    
    // HISTOGRAMS

     let emptyRGBHistograms () =
        [
            {
                label = "R"
                bandIndices = None
                histogram = [||]
            }
            {
                label = "G"
                bandIndices = None    
                histogram = [||]
            }
            {
                label = "B"
                bandIndices = None
                histogram = [||]
            }
        ]

     let computeOne
        (sources : RgbBandSource list)
        (binCount : int)
        (label : string)
        (bandIndex : int option)
        : RgbSelectedBandHistogram =

        match bandIndex with
        | None ->
            {
                label = label
                bandIndices = None
                histogram = [||]
            }

        | Some selectedBandIndex ->
            match
                sources
                |> List.tryFind (fun source ->
                    source.logicalIndex = selectedBandIndex)
            with
            | None ->
                Log.warn
                    "Could not find selected histogram band %d for %s"
                    selectedBandIndex
                    label

                {
                    label = label
                    bandIndices = Some [| selectedBandIndex |]
                    histogram = [||]
                }

            | Some source ->
                match
                    readBandSourceHistogram
                        binCount
                        1.0e-4
                        source
                with
                | Result.Ok histogram ->
                    {
                        label = label
                        bandIndices = Some [| selectedBandIndex |]
                        histogram = histogram
                    }

                | Result.Error error ->
                    Log.warn
                        "Could not compute histogram for %s band %d: %s"
                        label
                        selectedBandIndex
                        error

                    {
                        label = label
                        bandIndices = Some [| selectedBandIndex |]
                        histogram = [||]
                    }

     let computeRgbMappingSelectedBandHistograms
        (m : AdaptiveModel)
        (binCount : int)
        : aval<RgbSelectedBandHistogram list> =

        AVal.custom (fun token ->

            let sources =
                m.images
                |> AList.force
                |> fun images -> readAdaptiveBandSources images token
            
            [
                computeOne sources binCount "R" (m.bandMapping.redBand.GetValue token)
                computeOne sources binCount "G" (m.bandMapping.greenBand.GetValue token)
                computeOne sources binCount "B" (m.bandMapping.blueBand.GetValue token)
            ]
        )

     let computeTransferFunctionSelectedBandHistogram
        (m : AdaptiveModel)
        (binCount : int)
        : aval<RgbSelectedBandHistogram list> =

        AVal.custom (fun token ->
            let selectedBandIndex =
                m.transferFunctionMapping.selectedBand.GetValue token

            let label = 
                "Transfer function"
                
            let images =
                m.images
                |> AList.force

            let sources =
                readAdaptiveBandSources images token
            [
                computeOne sources binCount label selectedBandIndex      
            ]            
        )

     let computeRgbRatioSelectedBandHistograms
            (m : AdaptiveModel)
            (binCount : int)
            : aval<RgbSelectedBandHistogram list> =

            AVal.custom (fun token ->

                let images =
                    m.images
                    |> AList.force

                let sources =
                    readAdaptiveBandSources images token

                [
                    computeOne sources binCount "R numerator"   (m.rgbRatioComposite.redNumeratorBand.GetValue token)
                    computeOne sources binCount "R denominator" (m.rgbRatioComposite.redDenominatorBand.GetValue token)

                    computeOne sources binCount "G numerator"   (m.rgbRatioComposite.greenNumeratorBand.GetValue token)
                    computeOne sources binCount "G denominator" (m.rgbRatioComposite.greenDenominatorBand.GetValue token)

                    computeOne sources binCount "B numerator"   (m.rgbRatioComposite.blueNumeratorBand.GetValue token)
                    computeOne sources binCount "B denominator" (m.rgbRatioComposite.blueDenominatorBand.GetValue token)
                ]
            )              

     let computeNonMultispectralRgbHistograms
        (m : AdaptiveModel)
        (binCount : int)
        : aval<RgbSelectedBandHistogram list> =

        AVal.custom (fun token ->

            match m.sourceImagePath.GetValue token with
            | None ->
                emptyRGBHistograms ()

            | Some imagePath ->            
                match readSourceImageRGBChannels imagePath with
                | Result.Error error ->
                    Log.warn
                        "Could not compute RGB histograms for band %s: %s"
                        imagePath
                        error
                    emptyRGBHistograms ()

                | Result.Ok (red, green, blue) -> 
                    let computeChannel label values =
                        {
                            label = label
                            bandIndices = None
                            histogram =
                                ImageMath.computeHistogram
                                    binCount
                                    1.0e-4
                                    values
                        }
                    [
                        computeChannel "R" red
                        computeChannel "G" green
                        computeChannel "B" blue
                    ]
        )
  
     let computeMultispectralRgbHistograms
        (m : AdaptiveModel)
        (binCount : int)
        : aval<RgbSelectedBandHistogram list> =

        AVal.custom (fun token ->
            
            let images =
                m.images
                |> AList.force

            let sources =
                readAdaptiveBandSources images token

            match ImageMath.tryFindVisibleRgbBands sources with
            | None ->
                emptyRGBHistograms ()

            | Some (redSource, greenSource, blueSource) ->

                let computeChannel label source =
                    match
                        readBandSourceHistogram
                            binCount
                            1.0e-4
                            source
                    with
                    | Result.Ok histogram ->
                        {
                            label = label
                            bandIndices = Some [| source.logicalIndex |]
                            histogram = histogram
                        }

                    | Result.Error error ->
                        Log.warn
                            "Could not compute %s histogram for band %d: %s"
                            label
                            source.logicalIndex
                            error

                        {
                            label = label
                            bandIndices = Some [| source.logicalIndex |] 
                            histogram = [||]
                        }

                [
                    computeChannel "R" redSource
                    computeChannel "G" greenSource
                    computeChannel "B" blueSource
                ]
        )

     let histogramItemView
        (item : RgbSelectedBandHistogram)
        : DomNode<Message> =

        let titleText =
            match item.bandIndices with
            | Some bandIndices when bandIndices.Length > 0 ->
                sprintf "%s, band %d — %d bins"
                    item.label
                    bandIndices.[0]
                    item.histogram.Length

            | _ ->
                sprintf "%s — %d bins"
                    item.label
                    item.histogram.Length

        div [
            clazz "ui inverted segment"
            style "margin-top: 8px;"
        ] [
            div [
                style "font-weight: bold; margin-bottom: 6px;"
            ] [
                text titleText
            ]

            // checks if the computation returned an empty array
            if item.histogram.Length = 0 then
                div [
                    style "opacity: 0.7;"
                ] [
                    text "No histogram available."
                ]
            else
                let maxCount =
                    item.histogram
                    |> Array.map (fun b -> b.count)
                    |> Array.max
                    |> max 1

                div [
                    // the histogram container
                    style "height: 90px; display: flex; align-items: flex-end; gap: 1px; border-left: 1px solid #666; border-bottom: 1px solid #666; padding-left: 2px;"
                ] [
                    for bin in item.histogram do
                        // normalize bin height. The talles bin always fills the available height
                        let heightPercent =
                            100.0 * float bin.count / float maxCount

                        // browser tooltip for each bin, showing the bin range and count
                        let titleHist =
                            sprintf "%.6g – %.6g: %d" bin.lower bin.upper bin.count

                        div [
                            attribute "title" titleHist
                            style (
                                // creates a vertical bar for each bin, with a height proportional to the bin count
                                sprintf
                                    "flex: 1 1 0; min-width: 4px; flex-shrink: 0; height: %.2f%%; background: #aaa;"
                                    heightPercent
                            )
                        ] []
                ]

                let first =
                    item.histogram.[0]

                let last =
                    item.histogram.[item.histogram.Length - 1]

                div [
                    style "display: flex; justify-content: space-between; font-size: 11px; opacity: 0.8; margin-top: 4px;"
                ] [
                    div [] [text (sprintf "%.6g" first.lower)]
                    div [] [text (sprintf "%.6g" last.upper)]
                ]
        ]

     let selectedHistogramsView
        (title : string)
        (histograms : aval<RgbSelectedBandHistogram list>)
        : DomNode<Message> =

        Incremental.div
            (AttributeMap.ofList [
                clazz "ui inverted segment"
                style "margin-top: 10px;"
            ])
            (
                alist {
                    yield div [
                        style "font-size: 11px; color: #aaa;"
                    ] [
                        text title
                    ]

                    let! items = histograms

                    for item in items do
                        yield histogramItemView item
                }
            )

    // SPECTRAL PROFILES
     let emptyProfile label =
        {
            label = label
            wavelengthSpan = None
            spectralProfile = [||]
        }

     let averageSignal (values: float[]) =
        let mutable sum = 0.0
        let mutable count = 0L

        for value in values do
            if Double.IsFinite value && value > 0.0 then
                sum <- sum + value
                count <- count + 1L

        if count = 0L then None
        else Some (sum / float count)

     let computeSpectralPoints 
        sources
        label
        color
        (bandIndices : int[])
        : SpectralProfilePoint[] =
        
        bandIndices
            |> Array.choose (fun selectedBandIndex ->
                if selectedBandIndex < 0 then
                    Log.warn
                        "Selected band index %d for %s is invalid"
                        selectedBandIndex
                        label

                    None
                else
                    match readLogicalBandStatistics sources selectedBandIndex with
                    | Result.Error error ->
                        Log.warn
                            "Could not compute spectral point for %s band %d: %s"
                            label
                            selectedBandIndex
                            error

                        None

                    | Result.Ok statistics ->
                        match statistics.positiveFiniteMean with
                        | Some average ->
                            let xValue = float (selectedBandIndex + 1)

                            Some {
                                wavelength = xValue
                                value = average
                                color = color
                            }

                        | None ->
                            Log.warn
                                "Band %d contains no positive finite values"
                                selectedBandIndex

                            None            
            )
            |> Array.sortBy (fun point -> point.wavelength)

     let computeMappedBand
        sources
        label
        color
        (bandIndices : int[]) =

        let spectralPoints = computeSpectralPoints sources label color bandIndices            

        if Array.isEmpty spectralPoints then
            Log.warn
                "The spectralPoints are empty"
            emptyProfile label

        else           

            let minimumWavelength =
                spectralPoints.[0].wavelength

            let maximumWavelength =
                spectralPoints.[spectralPoints.Length - 1].wavelength

            {
                label = label

                wavelengthSpan =
                    Some (
                        minimumWavelength,
                        maximumWavelength
                    )

                spectralProfile = spectralPoints
            }

     let averageRatio (numerator: float[]) (denominator: float[]) =
        let count = min numerator.Length denominator.Length
        let mutable sum = 0.0
        let mutable validCount = 0L

        for i = 0 to count - 1 do
            let n = numerator.[i]
            let d = denominator.[i]

            if Double.IsFinite n &&
               Double.IsFinite d &&
               abs d > 1.0e-12 then
                let ratio = n / d

                if Double.IsFinite ratio then
                    sum <- sum + ratio
                    validCount <- validCount + 1L

        if validCount = 0L then None
        else Some (sum / float validCount)

     let computeRatioBand
        sources
        label
        color
        (numeratorBandIndices : int[])
        (denominatorBandIndices : int[]) =

        if numeratorBandIndices.Length <> denominatorBandIndices.Length then    
            Log.warn "The number of numerators and denominators must be identical."
            
            emptyProfile label

        else
            let spectralPoints =
                Array.zip
                    numeratorBandIndices
                    denominatorBandIndices
                |> Array.choose (fun (numeratorIndex, denominatorIndex) ->

                    match
                        readLogicalBand sources numeratorIndex,
                        readLogicalBand sources denominatorIndex
                    with
                    | Result.Ok numerator,
                      Result.Ok denominator ->

                        let averageRatio = averageRatio numerator.values denominator.values

                        match averageRatio with
                        | Some ratio ->
                            let xValue = float (numeratorIndex + 1)

                            Some {
                                wavelength = xValue
                                value = ratio
                                color = color
                            }

                        | None ->
                            Log.warn
                                "Ratio %d/%d has no valid values"
                                numeratorIndex
                                denominatorIndex

                            None

                    | Result.Error error, _ ->
                        Log.warn
                            "Could not read numerator band %d: %s"
                            numeratorIndex
                            error

                        None

                    | _, Result.Error error ->
                        Log.warn
                            "Could not read denominator band %d: %s"
                            denominatorIndex
                            error

                        None
                )
                |> Array.sortBy (fun point -> point.wavelength)

            if Array.isEmpty spectralPoints then
                emptyProfile label
            else
                {
                    label = label

                    wavelengthSpan =
                        Some (
                            spectralPoints.[0].wavelength,
                            spectralPoints.[spectralPoints.Length - 1].wavelength
                        )

                    spectralProfile = spectralPoints
                }

     let computeCompleteSpectralProfile
        (m : AdaptiveModel)
        : aval<RGBSelectedBandSpectralProfile> =

        let adaptiveImages =
            AList.toAVal m.images

        AVal.custom (fun token ->
            let images =
                adaptiveImages.GetValue token

            let sources =
                readAdaptiveBandSources images token

            let bandIndices =
                sources
                |> List.map (fun source -> source.logicalIndex)
                |> List.distinct
                |> List.sort
                |> List.toArray

            computeMappedBand
                sources
                "Complete spectral profile"
                "#ffffff"
                bandIndices
        )
                
     let computeRgbSpectralProfiles
        (m : AdaptiveModel)
        (sampleCount : int)
        : aval<RGBSelectedBandSpectralProfile list> =

        let adaptiveImages =
            AList.toAVal m.images

        AVal.custom (fun token ->

            match m.sourceImageKind.GetValue token with
            | SourceImageKind.Multispectral ->

                let images =
                    adaptiveImages.GetValue token

                let sources =
                    readAdaptiveBandSources images token

                match m.visualizationMode.GetValue token with
                | VisualizationMode.RgbComposite ->

                    let bandIndices =
                        [|
                            m.bandMapping.redBand.GetValue token
                            m.bandMapping.greenBand.GetValue token
                            m.bandMapping.blueBand.GetValue token
                        |]
                        |> Array.choose id

                    [
                        computeMappedBand
                            sources
                            "RGB composite"
                            "#ffffff"
                            bandIndices
                    ]

                | VisualizationMode.RgbRatioComposite ->

                    let ratioBandPairs =
                        [
                            m.rgbRatioComposite.redNumeratorBand.GetValue token,
                            m.rgbRatioComposite.redDenominatorBand.GetValue token

                            m.rgbRatioComposite.greenNumeratorBand.GetValue token,
                            m.rgbRatioComposite.greenDenominatorBand.GetValue token

                            m.rgbRatioComposite.blueNumeratorBand.GetValue token,
                            m.rgbRatioComposite.blueDenominatorBand.GetValue token
                        ]
                        |> List.choose (fun (numerator, denominator) ->
                            match numerator, denominator with
                            | Some numeratorIndex,
                              Some denominatorIndex ->
                                Some (
                                    numeratorIndex,
                                    denominatorIndex
                                )

                            | _ ->
                                None
                        )

                    let numeratorBandIndices =
                        ratioBandPairs
                        |> List.map fst
                        |> List.toArray

                    let denominatorBandIndices =
                        ratioBandPairs
                        |> List.map snd
                        |> List.toArray

                    [
                        computeRatioBand
                            sources
                            "RGB ratio composite"
                            "#ffffff"
                            numeratorBandIndices
                            denominatorBandIndices
                    ]

                | VisualizationMode.SingleBandTransferFunction ->

                    let bandIndices =
                        [|
                            m.transferFunctionMapping.selectedBand.GetValue token
                        |]
                        |> Array.choose id
                    
                    [
                        computeMappedBand
                            sources
                            "Transfer function"
                            "#ffffff"
                            bandIndices
                    ]

            | SourceImageKind.PlainRgbImage ->

                match m.sourceImagePath.GetValue token with
                | None ->
                    [
                        emptyProfile "RGB"
                    ]

                | Some imagePath ->
                    match readSourceImageRGBChannels imagePath with
                    | Result.Error error ->
                        Log.warn
                            "Could not compute RGB spectral profile for %s: %s"
                            imagePath
                            error

                        [
                            emptyProfile "RGB"
                        ]

                    | Result.Ok (red, green, blue) ->

                        let channels =
                            [|
                                1.0, red
                                2.0, green
                                3.0, blue
                            |]

                        let spectralPoints =
                            channels
                            |> Array.choose (fun (wavelength, values) ->
                                match averageSignal values with
                                | Some average ->
                                    Some {
                                        wavelength = wavelength
                                        value = average
                                        color = "#ffffff"
                                    }

                                | None ->
                                    None
                            )
                            |> Array.sortBy (fun point ->
                                point.wavelength
                            )

                        if Array.isEmpty spectralPoints then
                            [
                                emptyProfile "RGB"
                            ]
                        else
                            [
                                {
                                    label = "RGB"

                                    wavelengthSpan =
                                        Some (
                                            spectralPoints.[0].wavelength,
                                            spectralPoints.[
                                                spectralPoints.Length - 1
                                            ].wavelength
                                        )

                                    spectralProfile =
                                        spectralPoints
                                }
                            ]
        )

     let spectralProfileMaximum
        (profile : RGBSelectedBandSpectralProfile)
        =
        profile.spectralProfile
        |> Array.map (fun point -> point.value)
        |> Array.filter Double.IsFinite
        |> function
            | [||] ->
                1.0

            | values ->
                let maximum = Array.max values
                if maximum > 0.0 then maximum else 1.0

     let rgbSpectralProfilesView
        (profiles : RGBSelectedBandSpectralProfile list)
        (isCompleteProfile : bool)
        (sharedMaximumValue : float)
        =

        let validProfiles =
            profiles
            |> List.choose (fun profile ->
                match profile.wavelengthSpan with
                | Some (minWavelength, maxWavelength)
                    when profile.spectralProfile.Length > 0 ->

                    Some (
                        profile,
                        minWavelength,
                        maxWavelength
                    )

                | _ ->
                    None
            )

        if validProfiles.IsEmpty then
            div [] [
                text "No spectral profile available."
            ]
        else

            let width = 320.0
            let height = 160.0

            let leftPadding = 42.0
            let rightPadding = 12.0
            let topPadding = 12.0
            let bottomPadding = 30.0

            let plotWidth =
                width - leftPadding - rightPadding

            let plotHeight =
                height - topPadding - bottomPadding

            // Shared wavelength range for all RGB profiles.
            let minimumWavelength =
                validProfiles
                |> List.map (fun (_, minimum, _) -> minimum)
                |> List.min

            let maximumWavelength =
                validProfiles
                |> List.map (fun (_, _, maximum) -> maximum)
                |> List.max

            let wavelengthRange =
                maximumWavelength - minimumWavelength

            let allSpectralPoints =
                validProfiles
                |> List.collect (fun (profile, _, _) ->
                    profile.spectralProfile
                    |> Array.toList
                )
            
            let maximumValue =
                if Double.IsFinite sharedMaximumValue &&
                   sharedMaximumValue > 0.0 then
                    sharedMaximumValue
                else
                    1.0

            let toX wavelength =
                if wavelengthRange <= 0.0 then
                    leftPadding + plotWidth / 2.0
                else
                    leftPadding +
                    (wavelength - minimumWavelength) /
                    wavelengthRange *
                    plotWidth

            let toY value =
                topPadding +
                (1.0 - value / maximumValue) *
                plotHeight


            let createPolylinePoints
                (profile : RGBSelectedBandSpectralProfile)
                =
                profile.spectralProfile
                |> Array.map (fun point ->
                    sprintf
                        "%.2f,%.2f"
                        (toX point.wavelength)
                        (toY point.value)
                )
                |> String.concat " "

            let profileLines =
                validProfiles
                |> List.map (fun (profile, _, _) ->
                    let color =
                        profile.spectralProfile.[0].color

                    let commonAttributes =
                        [
                            attribute "fill" "none"
                            attribute "stroke" color
                            attribute "stroke-width" "2"
                        ]

                    let commonAttributes =
                        if profile.label.Contains("denominator") then
                            attribute "stroke-dasharray" "5,4"
                            :: commonAttributes
                        else
                            commonAttributes

                    if profile.spectralProfile.Length = 1 then
                        // A one-point profile is shown as a horizontal line.
                        let y =
                            profile.spectralProfile.[0].value
                            |> toY

                        Svg.line (
                            [
                                attribute "x1" (string leftPadding)
                                attribute "y1" (string y)
                                attribute "x2" (string (leftPadding + plotWidth))
                                attribute "y2" (string y)
                            ]
                            @ commonAttributes
                        )
                    else
                        Svg.polyline (
                            attribute
                                "points"
                                (createPolylinePoints profile)
                            :: commonAttributes
                        )
                )

            let profileDots =
                validProfiles
                |> List.collect (fun (profile, _, _) ->
                    profile.spectralProfile
                    |> Array.map (fun point ->
                        Svg.circle [
                            attribute "cx" (string (toX point.wavelength))
                            attribute "cy" (string (toY point.value))
                            attribute "r" "3"
                            attribute "fill" point.color
                            attribute "stroke" "#222"
                            attribute "stroke-width" "1"
                        ]
                    )
                    |> Array.toList
                )

            let bandNumbers =
                allSpectralPoints
                |> List.map (fun point -> point.wavelength)
                |> List.distinct
                |> List.sort

            let visibleBandNumbers =
                if isCompleteProfile then
                    match bandNumbers with
                    | [] ->
                        []

                    | [ singleBand ] ->
                        [ singleBand ]

                    | bands ->
                        [
                            List.head bands
                            List.last bands
                        ]
                else
                    bandNumbers

            let isPlainRgbProfile =
                not isCompleteProfile &&
                validProfiles
                |> List.exists (fun (profile, _, _) ->
                    profile.label = "RGB"
                )

            let bandLabels =
                visibleBandNumbers
                |> List.map (fun bandNumber ->
                    let label =
                        if isPlainRgbProfile then
                            match int bandNumber with
                            | 1 -> "R"
                            | 2 -> "G"
                            | 3 -> "B"
                            | _ -> sprintf "%.0f" bandNumber
                        else
                            sprintf "%.0f" bandNumber

                    Svg.text [
                        attribute "x" (string (toX bandNumber))
                        attribute "y" (string (height - 10.0))
                        attribute "text-anchor" "middle"
                        attribute "font-size" "10"
                        attribute "fill" "#aaa"
                    ] label
                )

            let axisAndLabels =
                    [
                        // Y axis
                        Svg.line [
                            attribute "x1" (string leftPadding)
                            attribute "y1" (string topPadding)
                            attribute "x2" (string leftPadding)
                            attribute "y2" (string (topPadding + plotHeight))
                            attribute "stroke" "#777"
                        ]

                        // X axis
                        Svg.line [
                            attribute "x1" (string leftPadding)
                            attribute "y1" (string (topPadding + plotHeight))
                            attribute "x2" (string (leftPadding + plotWidth))
                            attribute "y2" (string (topPadding + plotHeight))
                            attribute "stroke" "#777"
                        ]

                        // X-axis title
                        Svg.text [
                            attribute "x" 
                                (string (topPadding + plotWidth / 2.0))
                            attribute "y" 
                                (string (bottomPadding * 1.5 + plotHeight))

                            attribute "text-anchor" "middle"
                            attribute "font-size" "10"
                            attribute "fill" "#aaa"
                        ] "Band Number"

                        // Y = 1
                        Svg.text [
                            attribute "x" (string (leftPadding - 6.0))
                            attribute "y" (string (topPadding + 4.0))
                            attribute "text-anchor" "end"
                            attribute "font-size" "10"
                            attribute "fill" "#aaa"
                        ] (sprintf "%.2f" maximumValue)

                        // Y = 0
                        Svg.text [
                            attribute "x" (string (leftPadding - 6.0))
                            attribute "y" (string (topPadding + plotHeight))
                            attribute "text-anchor" "end"
                            attribute "font-size" "10"
                            attribute "fill" "#aaa"
                        ] "0.0"

                        // Y-axis title
                        Svg.text [
                            attribute "x" "10"
                            attribute "y"
                                (string (topPadding + plotHeight / 2.0))

                            attribute "transform"
                                (sprintf
                                    "rotate(-90 10 %.2f)"
                                    (topPadding + plotHeight / 2.0))

                            attribute "text-anchor" "middle"
                            attribute "font-size" "10"
                            attribute "fill" "#aaa"
                        ] "Average signal"
                    ] @ bandLabels

            div [
                clazz "ui inverted segment"
                style "margin-top: 8px;"
            ] [
                div [
                    style "font-weight: bold; margin-bottom: 4px;"
                ] [
                ]
                if (isCompleteProfile) then
                    Svg.svg [
                        attribute "viewBox"
                            (sprintf "0 0 %.0f %.0f" width height)

                        style "width: 100%; height: 160px;"
                    ] (
                        axisAndLabels @ profileLines
                    )
                else 
                    Svg.svg [
                        attribute "viewBox"
                            (sprintf "0 0 %.0f %.0f" width height)

                        style "width: 100%; height: 160px;"
                    ] (
                        axisAndLabels @ profileLines @ profileDots
                    )
            ]

     let spectralProfileView
        (profile : aval<RGBSelectedBandSpectralProfile>)
        : DomNode<Message> =
        Incremental.div
            AttributeMap.empty
            (
                alist {
                    let! item = profile 

                    let maximumValue = spectralProfileMaximum item

                    yield rgbSpectralProfilesView [ item ] true maximumValue
                }
            )

     let selectedSpectralProfilesView
        (profiles : aval<RGBSelectedBandSpectralProfile list>)
        (completeProfile : aval<RGBSelectedBandSpectralProfile>)
        : DomNode<Message> =

        Incremental.div
            AttributeMap.empty
            (
                alist {
                    let! items = profiles
                    let! completeItem = completeProfile

                    let maximumValue =
                        spectralProfileMaximum completeItem

                    yield
                        rgbSpectralProfilesView
                            items
                            false
                            maximumValue
                }
            )