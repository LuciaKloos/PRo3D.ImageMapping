namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO
open Newtonsoft.Json.Linq

open Aardvark.GeoSpatial.Opc
open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentData
open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE


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

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue
        member x.UseFalseColor : bool = uniform?UseFalseColor
        member x.DataType : int = uniform?DataType

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

    let getDataType (filePath: string) = 
        if not (File.Exists(filePath)) then
            DataType.UInt16
        else 
            let j = JObject.Parse(File.ReadAllText(filePath))
            match j.TryGetValue("data_type") with
            | true, t -> 
                match t.Value<string>() with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float" -> DataType.Float
                | _ -> DataType.UInt16
            | _ -> DataType.UInt16

    let getMinMaxFromStatistics (filePath: string, channel: int) =
        if not (File.Exists(filePath)) then
            (0, 0)
        else 
            let j = JObject.Parse(File.ReadAllText(filePath))
            let rec loop (token:JToken) remaining =
                match remaining with
                | [] -> token.Value<int>()
                | k::rest ->
                    match token.Type with
                    | JTokenType.Object ->
                        let obj = token :?> JObject
                        match obj.TryGetValue(k) with
                            | true, t -> loop t rest
                            | _ -> token.Value<int>()
                    | JTokenType.Array ->
                        let obj = token.[channel] :?> JObject
                        match obj.TryGetValue(k) with
                            | true, t -> loop t rest
                            | _ -> token.Value<int>()
                    | _ -> token.Value<int>()

            (loop (j :> JToken) ["image_statistics"; "minimum"], loop (j :> JToken) ["image_statistics"; "maximum"])

    let getEXRChannelOptions (filePath: string) =
        if not ((File.Exists(filePath)) || (Path.GetExtension(filePath).ToLower() != ".exr")) then
            []
        else 
            let j = JObject.Parse(File.ReadAllText(filePath))
            match j.TryGetValue("channels") with
            | true, t -> 
                let channels = [ 0 .. t.Value<int>() - 1 ]
                channels |> List.map string
            | _ -> []
            

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
        cameraState = OrbitState.create V3d.Zero 0.0 0.0 (2.0 * (3389.5 * 1000.0))
        colorMap = ColorMap.Magma;
        useFalseColor = true;
        channel = { idx = 0; name = None }
        channelOptions = [];
        dataType = DataType.UInt16;
        defaultMinValue = minValue.value;
        defaultMaxValue = maxValue.value;
        customMinValue = minValue;
        customMaxValue = maxValue;
        texture = initialPath;
        projectionOpacity = { Numeric.init with min = 0.0; max = 1.0; value = 1.0 }
    }

    let loadFile (texturePath : string) =
        // this could be a fallback
        let ifUsefulThisIsHowToExtractInfos = MultiBandReader.tryGetChannels texturePath
        let channelOptions = getEXRChannelOptions(texturePath + ".json")
        let channelIdx =
            if channelOptions.IsEmpty() then
                0
            else
                match System.Int32.TryParse(channelOptions.[0]) with
                | true, v -> v
                | false, _ -> 0
        let (min, max) = getMinMaxFromStatistics(texturePath + ".json", channelIdx)
        let dataType = getDataType(texturePath + ".json")
        let (rangeMin, rangeMax) =
            match dataType with
            | DataType.Float -> (min, max)
            | DataType.UInt16 
            | _ -> (0, 65536)
        { initial with
            texture = Path.GetFullPath(texturePath);
            defaultMinValue = min;
            defaultMaxValue = max;
            customMinValue = {minValue with value = min; min = rangeMin; max = rangeMax};
            customMaxValue = {maxValue with value = max; min = rangeMin; max = rangeMax};
            channel = { idx = channelIdx; name = None }
            channelOptions = [ { idx = 0; name = None } ];
            dataType = dataType;
        }

    let update (m : Image) (msg : ImageMessage) =
        match msg with
            | OrbitCameraMessage msg ->
                { m with cameraState = OrbitController.update m.cameraState msg }
            | SetDataTypeAndRange (dataType, min, max) ->
                { m with customMinValue = { minValue with min = min}; customMaxValue = {minValue with max = max} }
            | SetCustomMin v -> 
                { m with customMinValue = {minValue with value = v} }
            | SetCustomMax v -> 
                { m with customMaxValue = {maxValue with value = v} }
            | ResetCustomMinMax ->
                { m with customMinValue = {minValue with value = m.defaultMinValue}; customMaxValue = {maxValue with value = m.defaultMaxValue} }
            | SetColorMap (map : ColorMap) ->
                { m with colorMap = map }
            | SetEXRChannel channel ->
                let (min, max) = getMinMaxFromStatistics(Path.Combine (m.texture, ".json"), channel.idx)
                { m with 
                    defaultMinValue = min;
                    defaultMaxValue = max;
                    customMinValue = {minValue with value = min};
                    customMaxValue = {maxValue with value = max};
                    channel = channel
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
                        Html.SemUi.dropDown' (AList.ofAVal m.channelOptions) m.channel (fun value -> SetEXRChannel value) channelRepr
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
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.customMinValue.value) SetCustomMin
                    br []
                    Numeric.view' [Slider] m.customMinValue
                    |> UI.map (fun action -> 
                        match action with
                        | Numeric.Action.SetValue v ->
                            SetCustomMin v
                        | _ ->
                            ImageMessage.Empty
                        )
                    ]
                Html.row "Maximum:"  [
                    SimplePrimitives.numeric { min = 0.0; max = 65535.0; largeStep = 0.1; smallStep = 0.01 } AttributeMap.empty (m.customMaxValue.value) SetCustomMax
                    br []
                    div [style "width: 100%"] [
                        Numeric.numericField' m.customMaxValue Slider
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

    let view2DAnd3DImageAbsolute (m : AdaptiveImage) =

        let colormapTexture : aval<ITexture> =
            m.colorMap
            |> AVal.map (fun map ->
                let resourceName = ColorMap.getColorMapFileName(map)
                InstrumentImageVisualization.getColorMapTexture resourceName
            )

        let imageTexture : aval<ITexture> =
            (m.texture, m.channel) 
            ||> AVal.map2 (fun (path : string) channel ->
                    match Path.GetExtension(path).ToLower() with
                    | ".exr" ->
                        let stream = File.OpenRead path
                        let exrTexture = TextureLoading.loadImageFromStream stream (ChannelReference.ChannelWithIndex channel.idx) (Some TextureLoading.TextureFormat.OpenEXR)
                        PixTexture2d(exrTexture, TextureParams.empty)
                    | ".tiff" | ".tif" -> 
                        let ifUsefulThisIsHowToExtractInfos = MultiBandReader.tryGetChannels path
                        match MultiBandReader.tryReadMultiBandTiff path false with
                        | Result.Ok img -> 
                            let images = InstrumentImageTextures.instrumentImageToTexture true img 
                            match Array.tryItem channel.idx images with
                            | Some img -> 
                                PixTexture2d(img.pi, TextureParams.empty)
                            | _ -> 
                                Log.warn "channel of out of bounds"
                                DefaultTextures.checkerboard.GetValue()
                        | _ -> 
                            Log.warn "could not load texture"
                            DefaultTextures.checkerboard.GetValue()
                    | ".png" | _ -> whiteTex 
            ) 

        let instrumentVisualization = 
            Sg.fullScreenQuad
            |> Sg.noEvents
            |> Sg.texture "InstrumentImage" imageTexture
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.uniform "MinValue" (m.customMinValue.value |> AVal.map (fun v -> float v)) // float v / 65535.0
            |> Sg.uniform "MaxValue" (m.customMaxValue.value |> AVal.map (fun v -> float v))
            |> Sg.uniform "UseFalseColor" m.useFalseColor
            |> Sg.uniform "DataType" (m.dataType |> AVal.map (fun dt -> int dt))
            |> Sg.shader {
                do! (Shaders.hshColors)
            }

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        let visualization, frustum =
            let observer = cval "MARS" //"HERA_AFC-1" 
            let supportBody = cval "SUN"
            let referenceFrame = cval "ECLIPJ2000"
            let referenceFrame = cval "IAU_MARS"
            let farPlaneMars = 30101626.50 * 1000.0

            let currentProjectedImage = 
                m.texture 
                |> AVal.map (fun path -> 
                    if File.Exists path then
                        Some (path, InstrumentMetadata.tryParseMetadataForImagePath path)
                    else
                        None
                )

            let imageSettings = 
                { 
                    VisualizationProperties.empty with 
                        visualizationRange = (m.customMinValue.value, m.customMaxValue.value) ||> AVal.map2 (fun min max -> Range1d(min,max))
                        colorMapping = InstrumentImageVisualization.getColorMapTexture "magma.png" |> Some |> AVal.constant
                        projectionOpacity = m.projectionOpacity.value
                }

            let projectionSetup = 
                let p = {
                    target = InstrumentImages.CameraFocus.FocusBody "MARS"
                    cameraSource =  InstrumentImages.CameraSource.InBody "HERA"
                    instrumentReferenceFrame = "HERA_AFC-1"
                    instrumentName = "HERA_AFC-1"
                    supportBody = "SUN"
                    time = DateTime.Now
                }
                currentProjectedImage |> AVal.map (function 
                    | Some (f, (Some mbi,_)) -> 
                        let p = 
                            { p with
                                time = mbi.obs_date
                                instrumentName = 
                                    match InstrumentProjection.instrument2SpiceName mbi.instrument with
                                    | None -> failwith "no spice name for the given instrument."
                                    | Some i -> i
                                instrumentReferenceFrame = "J2000"
                            }
                        p, mbi.obs_date
                    | _ -> 
                        let defaultTime = "2025-03-12 11:50:30.000Z"
                        p, DateTime.Parse(defaultTime)
                )

            let projection = projectionSetup |> AVal.map fst
            let time = projectionSetup |> AVal.map snd
            
            let projectImage = Visualization.creatProjectionFunction observer time referenceFrame currentProjectedImage projection
            let projectedTexture = Visualization.createProjectedTexture currentProjectedImage

            let projectionEnabled = 
                currentProjectedImage 
                |> AVal.map (function 
                    | Some (_, (Some _, _)) -> true
                    | _ -> false
                )

            let scene = 
                Visualization.createSceneGraph imageSettings referenceFrame supportBody observer time projectImage projectedTexture projectionEnabled
                |> Sg.noEvents

            let frustum = Frustum.perspective 80.0 10.0 farPlaneMars 1.0 |> AVal.constant
            scene, frustum

        require Html.semui (
                div [] [
                    div [] [
                        // the 2D control
                        let leftControl = [style "position: fixed; left: 0; top: 0; width: 50%; height: 100%"; attribute "showLoader" "false"]
                        renderControl (AVal.constant (Camera.create cameraView frustum')) leftControl instrumentVisualization
                    
                        // the 3D projection view
                        let rightControl = [style "position: fixed; right: 0; top: 0; width: 50%; height: 100%"; attribute "showLoader" "false"] |> AttributeMap.ofList
                        OrbitController.controlledControl m.cameraState OrbitCameraMessage frustum rightControl visualization
                    ]
                ]
        )

    let view2DRelative (m : AdaptiveImage) =

        let colormapTexture : aval<ITexture> =
            m.colorMap
            |> AVal.map (fun map ->
                let resourceName = ColorMap.getColorMapFileName(map)
                InstrumentImageVisualization.getColorMapTexture resourceName
            )

        let imageTexture : aval<ITexture> =
            (m.texture, m.channel) 
            ||> AVal.map2 (fun (path : string) channel ->
                    match Path.GetExtension(path).ToLower() with
                    | ".exr" ->
                        let stream = File.OpenRead path
                        let exrTexture = TextureLoading.loadImageFromStream stream (ChannelReference.ChannelWithIndex channel.idx) (Some TextureLoading.TextureFormat.OpenEXR)
                        PixTexture2d(exrTexture, TextureParams.empty)
                    | ".tiff" | ".tif" -> 
                        let ifUsefulThisIsHowToExtractInfos = MultiBandReader.tryGetChannels path
                        match MultiBandReader.tryReadMultiBandTiff path false with
                        | Result.Ok img -> 
                            let images = InstrumentImageTextures.instrumentImageToTexture true img 
                            match Array.tryItem channel.idx images with
                            | Some img -> 
                                PixTexture2d(img.pi, TextureParams.empty)
                            | _ -> 
                                Log.warn "channel of out of bounds"
                                DefaultTextures.checkerboard.GetValue()
                        | _ -> 
                            Log.warn "could not load texture"
                            DefaultTextures.checkerboard.GetValue()
                    | ".png" | _ -> whiteTex 
            ) 

        let instrumentVisualization = 
            Sg.fullScreenQuad
            |> Sg.noEvents
            |> Sg.texture "InstrumentImage" imageTexture
            |> Sg.texture "ColormapTexture" colormapTexture
            |> Sg.uniform "MinValue" (m.customMinValue.value |> AVal.map (fun v -> float v)) // float v / 65535.0
            |> Sg.uniform "MaxValue" (m.customMaxValue.value |> AVal.map (fun v -> float v))
            |> Sg.uniform "UseFalseColor" m.useFalseColor
            |> Sg.uniform "DataType" (m.dataType |> AVal.map (fun dt -> int dt))
            |> Sg.shader {
                do! (Shaders.hshColors)
            }

        let cameraView = CameraView.look V3d.OOI V3d.OON V3d.OIO
        let frustum' = Frustum.ortho (Box3d.FromMinAndSize(-V3d.III, V3d.III))

        require Html.semui (
            div [style "width: 100%; height: 200px; display: flex; align-items: center; justify-content: center; margin-top: 10px; border: solid 2px black; background: rgb(0, 0, 0, 0.5);"] [
                let style = [style "position: relative; width: 200px; height: 200px; padding: 2px"; attribute "showLoader" "false"]
                renderControl (AVal.constant (Camera.create cameraView frustum')) style instrumentVisualization
            ]
        )
