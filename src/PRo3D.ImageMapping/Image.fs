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

    let redBandSampler =
        sampler2d {
            texture uniform?RedBandImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let greenBandSampler =
        sampler2d {
            texture uniform?GreenBandImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let blueBandSampler =
        sampler2d {
            texture uniform?BlueBandImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue
        member x.UseFalseColor : bool = uniform?UseFalseColor
        member x.DataType : int = uniform?DataType
        member x.OverlayMax : V2d = uniform?OverlayMax
        member x.OverlayMin : V2d = uniform?OverlayMin

        member x.RedMinValue : float = uniform?RedMinValue
        member x.RedMaxValue : float = uniform?RedMaxValue

        member x.GreenMinValue : float = uniform?GreenMinValue
        member x.GreenMaxValue : float = uniform?GreenMaxValue

        member x.BlueMinValue : float = uniform?BlueMinValue
        member x.BlueMaxValue : float = uniform?BlueMaxValue

        member x.RgbDataType : int = uniform?RgbDataType

    [<ReflectedDefinition>]
    let normalizeBand
        (sampledValue  : float)
        (minimum : float)
        (maximum : float)
        (dataType : int) =

        let rawValue =
            if dataType = int DataType.Float then
                sampledValue 
            elif dataType = int DataType.UInt16 then
                sampledValue  * 65535.0
            else
                sampledValue  * 4294967295.0

        let range =
            max 0.0000001 (maximum - minimum)

        clamp 0.0 1.0 ((rawValue - minimum) / range)

    let rgbComposite (v : Vertex) =
        fragment {
            let redSample =
                redBandSampler.Sample(v.tc).X

            let greenSample =
                greenBandSampler.Sample(v.tc).X

            let blueSample =
                blueBandSampler.Sample(v.tc).X

            let r =
                normalizeBand
                    redSample
                    uniform.RedMinValue
                    uniform.RedMaxValue
                    uniform.RgbDataType

            let g =
                normalizeBand
                    greenSample
                    uniform.GreenMinValue
                    uniform.GreenMaxValue
                    uniform.RgbDataType

            let b =
                normalizeBand
                    blueSample
                    uniform.BlueMinValue
                    uniform.BlueMaxValue
                    uniform.RgbDataType

            return V4d(r, g, b, 1.0)
        }


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

    let private textureFromBand
        (img : aval<Option<AdaptiveImage>>)
        : aval<ITexture> =

        img
        |> AVal.bind (function
            | None ->
                DefaultTextures.checkerboard

            | Some img ->
                (img.texture, img.selectedChannel)
                ||> AVal.map2 (fun path channel ->
                    match Path.GetExtension(path).ToLowerInvariant() with
                    | ".tif"
                    | ".tiff" ->
                        match MultiBandReader.tryReadMultiBandTiff path false with
                        | Result.Ok multiBandImage ->
                            let textures =
                                InstrumentImageTextures.instrumentImageToTexture
                                    true
                                    multiBandImage

                            match Array.tryItem channel.idx textures with
                            | Some texture ->
                                PixTexture2d(
                                    texture.pi,
                                    TextureParams.empty
                                ) :> ITexture

                            | None ->
                                Log.warn
                                    "RGB channel %d is out of bounds for %s"
                                    channel.idx
                                    path

                                DefaultTextures.checkerboard.GetValue()

                        | Result.Error error ->
                            Log.warn
                                "Could not read multispectral TIFF %s: %A"
                                path
                                error

                            DefaultTextures.checkerboard.GetValue()

                    | ".exr" ->
                        use stream = File.OpenRead path

                        let texture =
                            TextureLoading.loadImageFromStream
                                stream
                                (ChannelReference.ChannelWithIndex channel.idx)
                                (Some TextureLoading.TextureFormat.OpenEXR)

                        PixTexture2d(
                            texture,
                            TextureParams.empty
                        ) :> ITexture

                    | extension ->
                        Log.warn "Unsupported image extension: %s" extension
                        DefaultTextures.checkerboard.GetValue()
                )
        )

    let createInstrumentScene
        (redImg : aval<Option<AdaptiveImage>>)
        (greenImg : aval<Option<AdaptiveImage>>)
        (blueImg : aval<Option<AdaptiveImage>>) =

        let extract
            (defaultValue : aval<'a>)
            (getter : AdaptiveImage -> aval<'a>)
            (img : aval<Option<AdaptiveImage>>) =

            img
            |> AVal.bind (function
                | Some value -> getter value
                | None -> defaultValue
            )

        let createBandTexture
            (img : aval<Option<AdaptiveImage>>)
            : aval<ITexture> =

            img
            |> extract DefaultTextures.checkerboard (fun image ->
                (image.texture, image.selectedChannel)
                ||> AVal.map2 (fun path channel ->
                    match Path.GetExtension(path).ToLowerInvariant() with
                    | ".exr" ->
                        use stream = File.OpenRead path

                        let texture =
                            TextureLoading.loadImageFromStream
                                stream
                                (ChannelReference.ChannelWithIndex channel.idx)
                                (Some TextureLoading.TextureFormat.OpenEXR)

                        PixTexture2d(
                            texture,
                            TextureParams.empty
                        ) :> ITexture

                    | ".tif"
                    | ".tiff" ->
                        match MultiBandReader.tryReadMultiBandTiff path false with
                        | Result.Ok multiBandImage ->
                            let textures =
                                InstrumentImageTextures.instrumentImageToTexture
                                    true
                                    multiBandImage

                            match Array.tryItem channel.idx textures with
                            | Some texture ->
                                PixTexture2d(
                                    texture.pi,
                                    TextureParams.empty
                                ) :> ITexture

                            | None ->
                                Log.warn
                                    "Channel %d is out of bounds for %s"
                                    channel.idx
                                    path

                                DefaultTextures.checkerboard.GetValue()

                        | Result.Error error ->
                            Log.warn
                                "Could not load TIFF %s: %A"
                                path
                                error

                            DefaultTextures.checkerboard.GetValue()

                    | extension ->
                        Log.warn "Unsupported extension: %s" extension
                        DefaultTextures.checkerboard.GetValue()
                )
            )

        let bandMin img =
            img
            |> extract
                (AVal.constant 0.0)
                (fun band ->
                    band.inputMinValue.value
                    |> AVal.map float
                )

        let bandMax img =
            img
            |> extract
                (AVal.constant 1.0)
                (fun band ->
                    band.inputMaxValue.value
                    |> AVal.map float
                )

        let bandDataType img =
            img
            |> extract
                (AVal.constant (int DataType.Float))
                (fun band ->
                    band.dataType
                    |> AVal.map int
                )

        let redTexture =
            createBandTexture redImg

        let greenTexture =
            createBandTexture greenImg

        let blueTexture =
            createBandTexture blueImg

        let rgbScene =
            Sg.fullScreenQuad
            |> Sg.noEvents

            // InstrumentImage is temporarily bound because
            // placeAspectFittedQuad uses instrumentSampler.Size.
            |> Sg.texture "InstrumentImage" redTexture

            |> Sg.texture "RedBandImage" redTexture
            |> Sg.texture "GreenBandImage" greenTexture
            |> Sg.texture "BlueBandImage" blueTexture

            |> Sg.uniform "RedMinValue" (bandMin redImg)
            |> Sg.uniform "RedMaxValue" (bandMax redImg)

            |> Sg.uniform "GreenMinValue" (bandMin greenImg)
            |> Sg.uniform "GreenMaxValue" (bandMax greenImg)

            |> Sg.uniform "BlueMinValue" (bandMin blueImg)
            |> Sg.uniform "BlueMaxValue" (bandMax blueImg)

            |> Sg.uniform "RgbDataType" (bandDataType redImg)

            // The RGB image occupies the complete render-control region.
            |> Sg.uniform "OverlayMin" (AVal.constant V2d.OO)
            |> Sg.uniform "OverlayMax" (AVal.constant V2d.II)

            |> Sg.shader {
                do! Shaders.rgbComposite
            }

        rgbScene

    let view2DAnd3DImageAbsolute (opacity : aval<float>) (boresightAdjustment : aval<Option<Trafo3d>>) (orbitState : AdaptiveOrbitState) (redImage : aval<Option<AdaptiveImage>>) (greenImage : aval<Option<AdaptiveImage>>) (blueImage : aval<Option<AdaptiveImage>>) =

        let instrumentVisualization =
            createInstrumentScene redImage greenImage blueImage

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum2D = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))
        let farPlaneMars = 30101626.50 * 1000.0
        let frustum = Frustum.perspective 80.0 10.0 farPlaneMars 1.0 |> AVal.constant

        let visualization (red : AdaptiveImage) =
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

            let redProjectedImage =
                currentProjectedImageFromImage red

            let greenProjectedImage =
                currentProjectedImageFromOptionalImage greenImage

            let blueProjectedImage =
                currentProjectedImageFromOptionalImage blueImage

            let redSelectedChannel =
                red.selectedChannel

            let greenSelectedChannel =
                selectedChannelFromOptionalImage greenImage

            let blueSelectedChannel =
                selectedChannelFromOptionalImage blueImage

            let redMin =
                red.inputMinValue.value
                |> AVal.map float

            let redMax =
                red.inputMaxValue.value
                |> AVal.map float

            let greenMin =
                greenImage
                |> extractBandValue 0.0 (fun image ->
                    image.inputMinValue.value
                    |> AVal.map float
                )

            let greenMax =
                greenImage
                |> extractBandValue 1.0 (fun image ->
                    image.inputMaxValue.value
                    |> AVal.map float
                )

            let blueMin =
                blueImage
                |> extractBandValue 0.0 (fun image ->
                    image.inputMinValue.value
                    |> AVal.map float
                )

            let blueMax =
                blueImage
                |> extractBandValue 1.0 (fun image ->
                    image.inputMaxValue.value
                    |> AVal.map float
                )

            let rgbDataType =
                red.dataType
                |> AVal.map int


            let imageSettings = 
                { 
                    VisualizationProperties.empty with 
                        visualizationRange = (redMin, redMax) ||> AVal.map2 (fun min max -> Range1d(min,max))
                        colorMapping = InstrumentImageVisualization.getColorMapTexture "magma.png" |> Some |> AVal.constant
                        projectionOpacity = opacity
                }

            let projectionSetup = 
                // instrument projection
                let p = {
                    target = InstrumentImages.CameraFocus.FocusBody "MARS"
                    cameraSource =  InstrumentImages.CameraSource.InBody "HERA"
                    instrumentReferenceFrame = "HERA_AFC-1"
                    instrumentName = "HERA_AFC-1"
                    supportBody = "SUN"
                    time = DateTime.Now
                    boresightAdjustment = None
                }
                (redProjectedImage, boresightAdjustment) ||> AVal.map2 (fun currentProjectedImage boresight -> 
                    match currentProjectedImage with
                    | Some (f, (Some mbi,_)) -> 
                        // update using selected image metadata
                        let p = 
                            { p with
                                time = mbi.obs_date
                                instrumentName = 
                                    match InstrumentProjection.instrument2SpiceName mbi.instrument with
                                    | None -> failwith "no spice name for the given instrument."
                                    | Some i -> i
                                instrumentReferenceFrame = "J2000"
                                boresightAdjustment = boresight
                            }
                        p, mbi.obs_date
                    | _ -> 
                        let defaultTime = "2025-03-12 11:50:30.000Z"
                        p, DateTime.Parse(defaultTime)
                )

            let projection = projectionSetup |> AVal.map fst
            let time = projectionSetup |> AVal.map snd
            
            let projectPrimaryImage =
                Visualization.creatProjectionFunction
                    observer
                    time
                    referenceFrame
                    redProjectedImage
                    projection

            let projectedRedTexture =
                Visualization.createProjectedTexture
                    redProjectedImage
                    redSelectedChannel

            let projectedGreenTexture =
                Visualization.createProjectedTexture
                    greenProjectedImage
                    greenSelectedChannel

            let projectedBlueTexture =
                Visualization.createProjectedTexture
                    blueProjectedImage
                    blueSelectedChannel      

            let primaryProjectionEnabled = 
                redProjectedImage 
                |> AVal.map (function 
                    | Some (_, (Some _, _)) -> true
                    | _ -> false
                )

            let rgbProjectionDebug =
                AVal.constant true

            Visualization.createSceneGraph
                imageSettings
                referenceFrame
                supportBody
                observer
                time
                projectPrimaryImage
                projectedRedTexture
                projectedGreenTexture
                projectedBlueTexture
                redMin
                redMax
                greenMin
                greenMax
                blueMin
                blueMax
                rgbDataType
                rgbProjectionDebug
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
                        
                        // Render exactly one sphere.
                        // visualization already receives overlayImg and maps it onto that sphere.
                        
                        let scene =
                            redImage
                            |> AVal.map (function
                                | None -> Sg.empty
                                | Some primary -> visualization primary
                            )
                            |> Sg.dynamic

                        OrbitController.controlledControl
                            orbitState
                            OrbitCameraMessage
                            frustum
                            rightControl
                            scene
                    ]
                ]
        )

    let view2DRelative (redImage : aval<Option<AdaptiveImage>>) (greenImage : aval<Option<AdaptiveImage>>) (blueImage : aval<Option<AdaptiveImage>>) =

        let instrumentVisualization = createInstrumentScene redImage greenImage blueImage

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        require Html.semui (
            div [style "width: 100%; height: 200px; display: flex; align-items: center; justify-content: center; margin-top: 10px; border: solid 2px black; background: rgb(0, 0, 0, 0.5);"] [
                let style = [style "position: relative; width: 200px; height: 200px; padding: 2px"; attribute "showLoader" "false"]
                renderControl (AVal.constant (Camera.create cameraView frustum')) style instrumentVisualization
            ]
        )
