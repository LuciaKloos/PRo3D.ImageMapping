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
                Math.Pow(normalized, 2.0 / 1.0) // 1/2.2 is less then one, -> raises dark and mid-range values

            // produces one byte of each of RGB
            gammaCorrected * 255.0
            |> Math.Round
            |> byte


    // constructs four-channel byte image. The original TIFF values may be UInt16, UInt32, Float32, and so forth. 
    // The final RGB image is always an 8-bit-per-channel RGBA image.
    let private createRgbCompositePixImage
        (path : string)
        : Result<PixImage<byte>, string> =

        try
            match MultiBandReader.tryReadMultiBandTiff path false with
            | Result.Error error ->
                Result.Error error

            | Result.Ok image ->

                if image.bands < 24 then
                    Result.Error (
                        sprintf
                            "RGB composite requires at least 24 bands, but %s has %d."
                            path
                            image.bands
                    )
                else
                    // these are example bands using almost the complete wavelength range of the image
                    // -> more false color image

                    let redBandIndex = 24
                    let greenBandIndex = 12
                    let blueBandIndex = 0

                    // convert each band to float[] indexing assumption: index = y * width + x
                    // each array contains one value per pixel
                    let redBand =
                        getBandAsFloat redBandIndex image

                    let greenBand =
                        getBandAsFloat greenBandIndex image

                    let blueBand =
                        getBandAsFloat blueBandIndex image

                    let sharedMin, sharedMax =
                        getSharedDisplayRange [|
                            redBand
                            greenBand
                            blueBand
                        |]

                    Log.line
                        "Shared RGB range: (%f, %f)"
                        sharedMin sharedMax

                    let rgbImage =
                        PixImage<byte>(
                            Col.Format.RGBA,
                            V2i(image.width, image.height)
                        )

                    rgbImage
                        .GetMatrix<C4b>()
                        .SetByCoord(fun (position : V2l) ->

                            let x =
                                int position.X

                            let y =
                                int position.Y

                            let index =
                                y * image.width + x

                            let r =
                                valueToByte sharedMin sharedMax redBand.[index]

                            let g =
                                valueToByte sharedMin sharedMax greenBand.[index]

                            let b =
                                valueToByte sharedMin sharedMax blueBand.[index]
                                
                            let rawR = redBand.[index]
                            let rawG = greenBand.[index]
                            let rawB = blueBand.[index]

                            let signal =
                                max rawR (max rawG rawB)

                            let foregroundThreshold = 0.001

                            let alpha =
                                if signal < foregroundThreshold then
                                    0uy
                                else
                                    255uy

                            C4b(r, g, b, alpha)

                        )
                    |> ignore

                    Result.Ok rgbImage

        with error ->
            Result.Error error.Message

    let private loadRgbCompositeTexture
        (path : string)
        : ITexture =

        match createRgbCompositePixImage path with
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
        : aval<ITexture> =

        path
        |> AVal.map (function
            | Some filePath when File.Exists filePath ->
                loadRgbCompositeTexture filePath

            | Some filePath ->
                Log.warn "RGB source file does not exist: %s" filePath
                DefaultTextures.checkerboard.GetValue()

            | None ->
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
            let p = {
                target = InstrumentImages.CameraFocus.FocusBody "MARS"
                cameraSource =  InstrumentImages.CameraSource.InBody "HERA"
                instrumentReferenceFrame = "HERA_AFC-1"
                instrumentName = "HERA_AFC-1"
                supportBody = "SUN"
                time = DateTime.Now
                boresightAdjustment = None
            }
            (currentProjectedImage, boresightAdjustment) ||> AVal.map2 (fun currentProjectedImage boresight -> 
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
