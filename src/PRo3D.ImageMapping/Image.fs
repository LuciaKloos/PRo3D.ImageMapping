namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO

open Aardvark.GeoSpatial.Opc
open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentData
open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE

open System.Text.Json

module Shaders = 
    open FShade
    open Aardvark.Rendering.Effects

    let instrumentSampler = 
        sampler2d {
            texture uniform?InstrumentImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let colormapTextureSampler =
        sampler2d {
            texture uniform?ColormapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let rgbCompositeSampler =
        sampler2d {
            texture uniform?RgbCompositeTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue
        member x.UseFalseColor : bool = uniform?UseFalseColor
        member x.DataType : int = uniform?DataType
        member x.OverlayMax : V2d = uniform?OverlayMax
        member x.OverlayMin : V2d = uniform?OverlayMin

    let hshColors (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X 
            let remappedClampedNormalizedXInt16 =
                ((min uniform.MaxValue (max uniform.MinValue (hshValueX * 65000.0))) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remappedClampedNormalizedXFloat =
                (hshValueX - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remapClampNormalize =
                if uniform.UseFalseColor then
                    V4d(
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        1.0
                    )
                else 
                    colormapTextureSampler.Sample(V2d ((if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16), 0.0))
            return remapClampNormalize
        }

    let displayRgbComposite (v : Vertex) =
        fragment {
            return rgbCompositeSampler.Sample(v.tc)
        }

module Image =

    let initialPath = ""

    let minValue = {
        value   = 0.0
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }

    let maxValue = {
        value   = 0.0
        min     = 0.0
        max     = 65000.0
        step    = 1
        format  = "{0:0.00}"
    }

    let initial = { 
        colorMap = ColorMap.Magma;
        useFalseColor = true;
        selectedChannel = { idx = 0; name = None }
        channelOptions = [];
        dataType = DataType.UInt16;
        defaultMinValues = [minValue.value];
        defaultMaxValues = [maxValue.value];
        inputMinValue = minValue;
        inputMaxValue = maxValue;
        texture = initialPath;
        distance = 0;
        time = new DateTime();
    }

    let private getBandAsFloat
        (bandIndex : int)
        (image : TiffReadResult)
        : float[] =

        if bandIndex < 0 || bandIndex >= image.bands then
            invalidArg
                "bandIndex"
                (sprintf
                    "Band %d is outside the available range 0..%d"
                    bandIndex
                    (image.bands - 1)
                )

        match image.buffers with
        | PixelBuffers.Float32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.UInt16Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.Int16Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.UInt32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.Int32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

    let private percentile
        (fraction : float)
        (sortedValues : float[]) =

        if sortedValues.Length = 0 then
            0.0
        else
            let index =
                fraction * float (sortedValues.Length - 1)
                |> Math.Round
                |> int
                |> max 0
                |> min (sortedValues.Length - 1)

            sortedValues.[index]


    /// Calculates one common display range from all RGB bands.
    let private getSharedDisplayRange (bands : float[][]) =

        let finiteValues =
            bands
            |> Array.collect (fun values ->
                values
                |> Array.filter (fun value ->
                    Double.IsFinite value &&
                    value > 0.0
                )
            )

        Array.sortInPlace finiteValues

        if finiteValues.Length = 0 then
            0.0, 1.0
        else
            let minimum =
                percentile 0.05 finiteValues

            let maximum =
                percentile 0.999 finiteValues

            if maximum <= minimum then
                minimum, minimum + 1.0
            else
                minimum, maximum

    let private getSymmetricDisplayLimit
        (validForeground : bool[])
        (values : float[]) =

        if validForeground.Length <> values.Length then
            invalidArg
                "validForeground"
                "Foreground mask and value array must have the same length."

        let absoluteValues =
            values
            |> Array.mapi (fun index value ->
                if
                    validForeground.[index] &&
                    Double.IsFinite value
                then
                    Some(abs value)
                else
                    None
            )
            |> Array.choose id

        Array.sortInPlace absoluteValues

        if absoluteValues.Length = 0 then
            1.0
        else
            percentile 0.98 absoluteValues
            |> max 1e-12

    // important, otherwise black
    let private valueToByte
        (minimum : float)
        (maximum : float)
        (value : float) =

        if not (Double.IsFinite value) || maximum <= minimum then
            0uy
        else
            let normalized = 
                (value - minimum) / (maximum - minimum)
                |> max 0.0
                |> min 1.0

            // Brightens darker values. Makes dark scientific values more visible.
            let gammaCorrected = 
                Math.Pow(normalized, 0.1 / 2.0) // 1/2.2 is less then one, -> raises dark and mid-range values

            // produces one byte of each of RGB
            gammaCorrected * 255.0
            |> Math.Round
            |> byte

    let private safeBandRatio
        (minimumSignal : float)
        (numerator : float[])
        (denominator : float[]) =

        Array.map2
            (fun n d ->
                if
                    Double.IsFinite n &&
                    Double.IsFinite d &&
                    n > minimumSignal &&
                    d > minimumSignal
                then
                    n / d
                else
                    Double.NaN
            )
            numerator
            denominator

    let private safeLogRatio
        (minimumSignal : float)
        (numerator : float[])
        (denominator : float[]) =

        if numerator.Length <> denominator.Length then
            invalidArg
                "denominator"
                "Ratio bands must contain the same number of pixels."

        Array.map2
            (fun numeratorValue denominatorValue ->
                if
                    Double.IsFinite numeratorValue &&
                    Double.IsFinite denominatorValue &&
                    numeratorValue > minimumSignal &&
                    denominatorValue > minimumSignal
                then
                    Math.Log(numeratorValue / denominatorValue)
                else
                    Double.NaN
            )
            numerator
            denominator

    let private signedValueToByte
        (limit : float)
        (value : float) =

        if not (Double.IsFinite value) || limit <= 0.0 then
            0uy
        else
            let scaled =
                value / limit
                |> max -1.0
                |> min 1.0

            let normalized =
                0.5 + 0.5 * scaled

            normalized * 255.0
            |> Math.Round
            |> byte

    let private getBackgroundThreshold
        (width : int)
        (height : int)
        (borderWidth : int)
        (band : float[]) =

        let backgroundValues =
            band
            |> Array.mapi (fun index value ->
                let x = index % width
                let y = index / width

                let isBorder =
                    x < borderWidth ||
                    x >= width - borderWidth ||
                    y < borderWidth ||
                    y >= height - borderWidth

                if isBorder && Double.IsFinite value then
                    Some value
                else
                    None
            )
            |> Array.choose id

        Array.sortInPlace backgroundValues

        if backgroundValues.Length = 0 then
            0.0
        else
            let median =
                percentile 0.5 backgroundValues

            let deviations =
                backgroundValues
                |> Array.map (fun value ->
                    abs (value - median)
                )

            Array.sortInPlace deviations

            let mad =
                percentile 0.5 deviations

            // 1.4826 converts MAD to a robust standard-deviation estimate.
            median + 6.0 * 1.4826 * mad

    // constructs four-channel byte image. The original TIFF values may be UInt16, UInt32, Float32, and so forth. 
    // The final RGB image is always an 8-bit-per-channel RGBA image.
    let private createRgbCompositePixImage
        (path : string)
        (redNumeratorIndex : int)
        (redDenominatorIndex : int)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : int)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : int)
        : Result<PixImage<byte>, string> =

        try
            match MultiBandReader.tryReadMultiBandTiff path false with
            | Result.Error error ->
                Result.Error error

            | Result.Ok image ->

                let selectedIndices =
                    [
                        redNumeratorIndex
                        redDenominatorIndex
                        greenNumeratorIndex
                        greenDenominatorIndex
                        blueNumeratorIndex
                        blueDenominatorIndex
                    ]

                let invalidIndex =
                    selectedIndices
                    |> List.tryFind (fun index ->
                        index < 0 || index >= image.bands
                    )

                match invalidIndex with
                | Some badIndex ->
                    Result.Error (
                        sprintf
                            "Selected RGB band %d is outside the available range 0..%d."
                            badIndex
                            (image.bands - 1)
                    )

                | None ->
                    let redNumerator =
                        getBandAsFloat redNumeratorIndex image

                    let redDenominator =
                        getBandAsFloat redDenominatorIndex image

                    let greenNumerator =
                        getBandAsFloat greenNumeratorIndex image

                    let greenDenominator =
                        getBandAsFloat greenDenominatorIndex image

                    let blueNumerator =
                        getBandAsFloat blueNumeratorIndex image

                    let blueDenominator =
                        getBandAsFloat blueDenominatorIndex image


                    let minimumSignal =
                        1e-3

                   
                    let redBand =
                        safeLogRatio minimumSignal redNumerator redDenominator

                    let greenBand =
                        safeLogRatio minimumSignal greenNumerator greenDenominator

                    let blueBand =
                        safeLogRatio minimumSignal blueNumerator blueDenominator


                    let isValidSignal value =
                        Double.IsFinite value &&
                        value > minimumSignal

                    let validForeground =
                        Array.init redBand.Length (fun index ->
                            isValidSignal redBand.[index] ||
                            isValidSignal greenBand.[index] ||
                            isValidSignal blueBand.[index]
                        )

                    let displayRangeForValidPixels values =
                        let validValues =
                            values
                            |> Array.mapi (fun index value ->
                                if
                                    validForeground.[index] &&
                                    Double.IsFinite value &&
                                    value > 0.0
                                then
                                    Some value
                                else
                                    None
                            )
                            |> Array.choose id

                        Array.sortInPlace validValues

                        if validValues.Length = 0 then
                            0.0, 1.0
                        else
                            let minimum =
                                percentile 0.02 validValues

                            let maximum =
                                percentile 0.98 validValues

                            if maximum <= minimum then
                                minimum, minimum + 1.0
                            else
                                minimum, maximum

                        

                    let redMin, redMax =
                        displayRangeForValidPixels redBand

                    let greenMin, greenMax =
                        displayRangeForValidPixels greenBand

                    let blueMin, blueMax =
                        displayRangeForValidPixels blueBand

                    let rgbImage =
                        PixImage<byte>(
                            Col.Format.RGBA,
                            V2i(image.width, image.height)
                        )


                    //let redLimit =
                    //    getSymmetricDisplayLimit
                    //        validForeground
                    //        redRatio

                    //let greenLimit =
                    //    getSymmetricDisplayLimit
                    //        validForeground
                    //        greenRatio

                    //let blueLimit =
                    //    getSymmetricDisplayLimit
                    //        validForeground
                    //        blueRatio


                    //let redMin, redMax =
                    //    getSharedDisplayRange [| redRatio |]

                    //let greenMin, greenMax =
                    //    getSharedDisplayRange [| greenRatio |]

                    //let blueMin, blueMax =
                    //    getSharedDisplayRange [| blueRatio |]


                    //rgbImage
                    //    .GetMatrix<C4b>()
                    //    .SetByCoord(fun (position : V2l) ->

                    //        let x =
                    //            int position.X

                    //        let y =
                    //            int position.Y

                    //        let index =
                    //            y * image.width + x

                            //let r =
                            //    valueToByte sharedMin sharedMax redRatio.[index]

                            //let g =
                            //    valueToByte sharedMin sharedMax greenRatio.[index]

                            //let b =
                            //    valueToByte sharedMin sharedMax blueRatio.[index]        
                            //let r =
                            //    signedValueToByte redLimit redRatio.[index]

                            //let g =
                            //    signedValueToByte greenLimit greenRatio.[index]

                            //let b =
                            //    signedValueToByte blueLimit blueRatio.[index]
                                
                            //let rawR1 = redBand1.[index]
                            //let rawR2 = redBand2.[index]
                            //let rawG1 = greenBand1.[index]
                            //let rawG2 = greenBand2.[index]
                            //let rawB1 = blueBand1.[index]
                            //let rawB2 = blueBand2.[index]

                            //let signal1 =
                            //    max rawR1 (max rawG1 rawB1)
                            //let signal2 =
                            //    max rawR2 (max rawG2 rawB2)
                            //let signal =
                            //    max signal1 signal2

                            //let foregroundThreshold = 0.001

                            //let alpha =
                            //    if signal < foregroundThreshold then
                            //        0uy
                            //    else
                            //        255uy

                    rgbImage
                        .GetMatrix<C4b>()
                        .SetByCoord(fun (position : V2l) ->

                            let x =
                                int position.X

                            let y =
                                int position.Y

                            let index =
                                y * image.width + x

                            if validForeground.[index] then

                                let r =
                                    valueToByte redMin redMax redBand.[index]

                                let g =
                                    valueToByte greenMin greenMax greenBand.[index]

                                let b =
                                    valueToByte blueMin blueMax blueBand.[index]

                                C4b(r, g, b, 255uy)

                            else
                                C4b(0uy, 0uy, 0uy, 0uy)
                        )
                    |> ignore

                    Result.Ok rgbImage

            with error ->
                Result.Error error.Message

    let private loadRgbCompositeTexture
        (path : string)
        (redNumeratorIndex : int)
        (redDenominatorIndex : int)
        (greenNumeratorIndex : int)
        (greenDenominatorIndex : int)
        (blueNumeratorIndex : int)
        (blueDenominatorIndex : int)
        : ITexture =

        match 
             createRgbCompositePixImage
                path
                redNumeratorIndex
                redDenominatorIndex
                greenNumeratorIndex
                greenDenominatorIndex
                blueNumeratorIndex
                blueDenominatorIndex
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
                "Could not create RGB composite for %s: %s"
                path
                error

            DefaultTextures.checkerboard.GetValue()

    let createRgbCompositeTexture
        (path : aval<Option<string>>)
        (redNumeratorBand : aval<Option<int>>)
        (redDenominatorBand : aval<Option<int>>)
        (greenNumeratorBand : aval<Option<int>>)
        (greenDenominatorBand : aval<Option<int>>)
        (blueNumeratorBand : aval<Option<int>>)
        (blueDenominatorBand : aval<Option<int>>)
        : aval<ITexture> =

        AVal.custom (fun token ->

            let pathValue =
                path.GetValue token

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

            match
                pathValue,
                redNumeratorValue,
                redDenominatorValue,
                greenNumeratorValue,
                greenDenominatorValue,
                blueNumeratorValue,
                blueDenominatorValue
            with
            | Some filePath,
              Some redNumerator,
              Some redDenominator,
              Some greenNumerator,
              Some greenDenominator,
              Some blueNumerator,
              Some blueDenominator when File.Exists filePath ->

                loadRgbCompositeTexture
                    filePath
                    redNumerator
                    redDenominator
                    greenNumerator
                    greenDenominator
                    blueNumerator
                    blueDenominator

            | Some filePath, _, _, _, _, _, _ when not (File.Exists filePath) ->
                Log.warn "RGB source file does not exist: %s" filePath
                DefaultTextures.checkerboard.GetValue()

            | _ ->
                DefaultTextures.checkerboard.GetValue()
        )

    let private tryReadWavelengthsFromJson (jsonPath : string) =
        try
            use document =
                JsonDocument.Parse(File.ReadAllText jsonPath)

            let mutable wavelengthsElement =
                Unchecked.defaultof<JsonElement>

            if
                document.RootElement.TryGetProperty(
                    "wavelengths",
                    &wavelengthsElement
                )
                && wavelengthsElement.ValueKind = JsonValueKind.Array
            then
                wavelengthsElement.EnumerateArray()
                |> Seq.map (fun value -> value.GetDouble())
                |> Seq.toList
                |> Some
            else
                None
        with error ->
            Log.warn "Could not read wavelengths from %s: %s"
                jsonPath
                error.Message

            None

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

    let loadBands (texturePath : string) : list<Image> =

        let fullPath = Path.GetFullPath texturePath

        let tiffMbiJson, tiffJson =
            InstrumentMetadata.tryParseMetadataForImagePath fullPath

        let channelCount =
            match tiffJson with
            | Some metadata -> max 1 metadata.channels
            | None -> 1

        let wavelengths =
            let jsonPath = Path.ChangeExtension(texturePath, ".json")

            if File.Exists jsonPath then
                tryReadWavelengthsFromJson jsonPath
                |> Option.defaultValue []
            else
                []

        let dataType =
            match tiffJson with
            | Some metadata ->
                match metadata.data_type.ToLowerInvariant() with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float"  -> DataType.Float
                | _        -> DataType.UInt16
            | None ->
                DataType.UInt16

        let rawMinValues =
            match tiffJson with
            | Some metadata ->
                metadata.image_statistics
                |> Array.map (fun statistics -> statistics.minimum)
                |> Array.toList
            | None ->
                []

        let rawMaxValues =
            match tiffJson with
            | Some metadata ->
                metadata.image_statistics
                |> Array.map (fun statistics -> statistics.maximum)
                |> Array.toList
            | None ->
                []

        // Ensure that these lists always contain one value per channel.
        // ResetCustomMinMax indexes them using selectedChannel.idx.
        let defaultMinValues =
            [
                for channelIndex in 0 .. channelCount - 1 do
                    yield
                        rawMinValues
                        |> List.tryItem channelIndex
                        |> Option.defaultValue 0.0
            ]

        let defaultMaxValues =
            [
                for channelIndex in 0 .. channelCount - 1 do
                    yield
                        rawMaxValues
                        |> List.tryItem channelIndex
                        |> Option.defaultValue 1.0
            ]

        let distance =
            match tiffMbiJson with
            | Some metadata -> metadata.targetPos.Length
            | None -> 0.0

        let time =
            match tiffMbiJson with
            | Some metadata -> metadata.obs_date
            | None -> DateTime.MinValue

        [
            for channelIndex in 0 .. channelCount - 1 do

                let minimum = defaultMinValues[channelIndex]
                let maximum = defaultMaxValues[channelIndex]

                let wavelengthName =
                    wavelengths
                    |> List.tryItem channelIndex
                    |> Option.map (fun wavelength ->
                        sprintf "%.0f nm" wavelength
                    )

                let channel =
                    {
                        idx = channelIndex
                        name = wavelengthName
                    }

                let sliderMinimum, sliderMaximum =
                    match dataType with
                    | DataType.Float ->
                        minimum, maximum

                    | DataType.UInt16 ->
                        0.0, 65535.0

                    | DataType.UInt32 ->
                        0.0, float UInt32.MaxValue

                let inputMinimum =
                    {
                        minValue with
                            value = minimum
                            min = sliderMinimum
                            max = sliderMaximum
                    }

                let inputMaximum =
                    {
                        maxValue with
                            value = maximum
                            min = sliderMinimum
                            max = sliderMaximum
                    }

                yield
                    {
                        initial with
                            texture = fullPath
                            selectedChannel = channel

                            // This band entry represents exactly one channel.
                            channelOptions = [channel]

                            defaultMinValues = defaultMinValues
                            defaultMaxValues = defaultMaxValues

                            inputMinValue = inputMinimum
                            inputMaxValue = inputMaximum

                            dataType = dataType
                            distance = distance
                            time = time
                    }
        ]

    let loadFile (texturePath : string) =
        // this could be a fallback
        let ifUsefulThisIsHowToExtractInfos = MultiBandReader.tryGetChannels texturePath

        let (tiffMbiJson, tiffJson) = InstrumentMetadata.tryParseMetadataForImagePath texturePath

        let channels =
            match tiffJson with
            | Some tf -> tf.channels
            | None -> 1

        let channelOptions = [ 0 .. channels - 1 ] |> List.map (fun channel -> {idx = channel; name = None})

        let selectedChannelIdx = 0

        let defaultMinValues = 
            match tiffJson with
            | Some tf -> tf.image_statistics |> Array.toList |> List.map (fun x -> x.minimum)
            | None -> [0.0]

        let defaultMaxValues = 
            match tiffJson with
            | Some tf -> tf.image_statistics |> Array.toList|> List.map (fun x -> x.maximum)
            | None -> [0.0]

        let dataType = 
            match tiffJson with
            | Some tf -> 
                match tf.data_type with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float" -> DataType.Float
                | _ -> DataType.UInt16
            | None -> DataType.UInt16

        let (rangeMin, rangeMax) =
            match dataType with
            | DataType.Float -> (defaultMinValues[selectedChannelIdx], defaultMaxValues[selectedChannelIdx])
            | DataType.UInt16 
            | _ -> (0, 65536)

        let inputMinValue = { minValue with value = defaultMinValues[selectedChannelIdx]; min = rangeMin; max = rangeMax}

        let inputMaxValue = { minValue with value = defaultMaxValues[selectedChannelIdx]; min = rangeMin; max = rangeMax }

        let distance =
            match tiffMbiJson with
            | Some mbi -> mbi.targetPos.Length
            | None -> 0.0

        let time =
            match tiffMbiJson with
            | Some mbi -> mbi.obs_date
            | None -> System.DateTime.MinValue // which default time?

        { initial with
            texture = Path.GetFullPath(texturePath);
            defaultMinValues = defaultMinValues;
            defaultMaxValues = defaultMaxValues;
            inputMinValue = inputMinValue;
            inputMaxValue = inputMaxValue;
            selectedChannel = channelOptions[selectedChannelIdx];
            channelOptions = channelOptions;
            dataType = dataType;
            distance = distance;
            time = time;
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

        let currentProjectedImageFromImage (m : AdaptiveImage) =
            m.texture
            |> AVal.map (fun path ->
                if File.Exists path then
                    Some (
                        path,
                        InstrumentMetadata.tryParseMetadataForImagePath path
                    )
                else
                    None
            )

        let currentProjectedImageFromOptionalImage
            (img : aval<Option<AdaptiveImage>>) =

            img
            |> AVal.bind (function
                | Some img ->
                    currentProjectedImageFromImage img
                | None ->
                    AVal.constant None
            )

        let selectedChannelFromOptionalImage
            (img : aval<Option<AdaptiveImage>>) =

            img
            |> AVal.bind (function
                | Some img ->
                    img.selectedChannel
                | None ->
                    AVal.constant { idx = 0; name = None }
            )

        let extractBandValue
            (defaultValue : float)
            (getter : AdaptiveImage -> aval<float>)
            (image : aval<Option<AdaptiveImage>>) =

            image
            |> AVal.bind (function
                | Some selected ->
                    getter selected
                | None ->
                    AVal.constant defaultValue
            )

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
                    let defaultTime =
                        DateTime.Parse("2025-03-12 11:50:30.000Z")

                    p, defaultTime
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
                        let leftControl = [style "position: fixed; left: 0; top: 0; width: 50%; height: 100%"; attribute "showLoader" "false"]
                        renderControl (AVal.constant (Camera.create cameraView frustum2D)) leftControl instrumentVisualization
                    
                        // the 3D projection view
                        let rightControl =
                            [
                                style "position: fixed; right: 0; top: 0; width: 50%; height: 100%"
                                attribute "showLoader" "false"
                            ]
                            |> AttributeMap.ofList
                        
                        OrbitController.controlledControl
                            orbitState
                            OrbitCameraMessage
                            frustum
                            rightControl
                            scene
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
