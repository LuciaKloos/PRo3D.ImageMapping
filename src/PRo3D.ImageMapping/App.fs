namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model
open PRo3D.Extensions.FSharp

open System.IO

type Self = Self

module App =

    let borderColor = "rgba(255,255,255,.1)"

    let initial : Model = {
        images = IndexList.Empty;
        selectedImage = None;
        sourceImagePath = None;
        editImages = List.Empty;
        projectionOpacity = { Numeric.init with min = 0.0; max = 1.0; step = 0.01; value = 1.0 }
        boresightAdjustment = BoresightAdjustment.identity
        cameraState = OrbitState.create V3d.Zero 0.0 0.0 (2.0 * (3389.5 * 1000.0))  
        rgbComposite = RgbComposite.empty
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
        
            let fullPath =
                Path.GetFullPath path

            let bands =
                Image.loadDataset fullPath
                |> IndexList.ofList

            let indices =
                bands
                |> IndexList.mapi (fun index _ -> index)
                |> IndexList.toList

            let firstBand =
                indices
                |> List.tryHead

            let bandCount = 
                bands 
                |> IndexList.toList
                |> List.length

            {
                m with
                    images = bands
                    selectedImage = firstBand
                    sourceImagePath = Some fullPath
                    editImages = []
                    rgbComposite = RgbComposite.fromBandCount bandCount
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
        | SetRgbBand (rgbChannel, rgbBandRole, rowIndex) ->

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
                        rgbComposite =
                            m.rgbComposite
                            |> RgbComposite.set rgbChannel rgbBandRole bandIndex
                }

            | None ->
                m
        | SetRgbLowerPercentile action ->
            {
                m with
                    rgbComposite =
                        {
                            m.rgbComposite with
                                lowerPercentile =
                                    Numeric.update m.rgbComposite.lowerPercentile action
                        }
            }

        | SetRgbUpperPercentile action ->
            {
                m with
                    rgbComposite =
                        {
                            m.rgbComposite with
                                upperPercentile =
                                    Numeric.update m.rgbComposite.upperPercentile action
                        }
            }

        | SetRgbGamma action ->
            {
                m with
                    rgbComposite =
                        {
                            m.rgbComposite with
                                gamma =
                                    Numeric.update m.rgbComposite.gamma action
                        }
            }


    
    let view (m : AdaptiveModel) (showDOM : AdaptiveImage -> DomNode<ImageMessage>) (showRelative2DImage : aval<ITexture> -> DomNode<Message>) (showAbsolute2DAnd3DImage : aval<Option<string>> -> aval<ITexture> -> DomNode<Message>) =

        let rgbTexture =
            Image.createRgbCompositeTexture
                m.images
                m.rgbComposite.redNumeratorBand
                m.rgbComposite.redDenominatorBand
                m.rgbComposite.greenNumeratorBand
                m.rgbComposite.greenDenominatorBand
                m.rgbComposite.blueNumeratorBand
                m.rgbComposite.blueDenominatorBand
                m.rgbComposite.lowerPercentile.value
                m.rgbComposite.upperPercentile.value
                m.rgbComposite.gamma.value

        let listAttributes =
            amap {
                yield clazz "ui divided list inverted segment"
                yield style "overflow-y : hidden"
            } |> AttributeMap.ofAMap

        let jsImportDialog =
            "top.aardvark.dialog.showOpenDialog({title: 'Select multispectral image', filters: [{name: 'Multispectral TIFF', extensions: ['mbi', 'json', 'tif', 'tiff']}], properties: ['openFile']}).then(result => {if (!result.canceled && result.filePaths && result.filePaths.length > 0) {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);}}).catch(error => {console.error('Could not open multispectral image dialog:', error);});"

        let selectedAdaptiveImage (selected : aval<Option<Index>>) =
            adaptive {
                let! selectedImage = selected
                match selectedImage with
                | Some sel -> 
                    let! img = AList.tryGet sel m.images
                    match img with
                    | Some img' -> return Some img'
                    | None -> return None
                | None ->
                    return None
            }

       
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

        let contentImages = 
            let attributesSelect = attribute "style" $"cursor: pointer; width: 50px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesEdit = attribute "style" $"cursor: pointer; width: 50px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesWavelength = attribute "style" $"cursor: pointer; width: 120px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesAttr1 = attribute "style" $"cursor: pointer; width: 120px; height: 40px; border-right: 1px solid {borderColor}; display: flex; justify-content: center; align-items: center;"
            let attributesAttr2 = attribute "style" $"cursor: pointer; width: 120px; height: 40px; display: flex; justify-content: center; align-items: center;"

            let header =
                div [ 
                    // attribute "clazz" "title active inverted"
                    attribute "style" $"display: flex; font-weight: bold; border-bottom: 2px solid black; background: black" 
                ] [
                    div [attributesSelect] [text "R Num"]
                    div [attributesSelect] [text "R Den"]
                    div [attributesSelect] [text "G Num"]
                    div [attributesSelect] [text "G Den"]
                    div [attributesSelect] [text "B Num"]
                    div [attributesSelect] [text "B Den"]
                    div [ attributesEdit ] [text "Edit"]
                    div [ attributesWavelength ] [text "Wavelengths"]
                    div [ attributesAttr1 ] [
                        i [clazz "sort icon"; onClick (fun _ -> SortEntriesByDistance);] []
                        text "Dist. to Planet"
                    ]
                    div [ attributesAttr2 ] [
                        i [clazz "sort icon"; onClick (fun _ -> SortEntriesByDate);] []
                        text "OBS Date"
                    ]
                ]
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

                                                            //match channel.name with
                                                            //| Some wavelength ->
                                                            //    sprintf "%s — Band %d (%s)"
                                                            //        (Path.GetFileName path)
                                                            //        bandNumber
                                                            //        wavelength

                                                            //| None ->
                                                            //    sprintf "%s — Band %d"
                                                            //        (Path.GetFileName path)
                                                            //        bandNumber
                                                        )
                                                    )
                                                ]
                                            div [attribute "style" "display: flex; font-weight: bold"] 
                                                [
                                                    let selectedBandIndex =
                                                        img.bandIndex

                                                    let rgbSelector
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
                                                                (SetRgbBand (channel, role, index))
                                                        ]

                                                    rgbSelector
                                                        m.rgbComposite.redNumeratorBand
                                                        RgbChannel.Red
                                                        RgbBandRole.Numerator

                                                    rgbSelector
                                                        m.rgbComposite.redDenominatorBand
                                                        RgbChannel.Red
                                                        RgbBandRole.Denominator

                                                    rgbSelector
                                                        m.rgbComposite.greenNumeratorBand
                                                        RgbChannel.Green
                                                        RgbBandRole.Numerator

                                                    rgbSelector
                                                        m.rgbComposite.greenDenominatorBand
                                                        RgbChannel.Green
                                                        RgbBandRole.Denominator

                                                    rgbSelector
                                                        m.rgbComposite.blueNumeratorBand
                                                        RgbChannel.Blue
                                                        RgbBandRole.Numerator

                                                    rgbSelector
                                                        m.rgbComposite.blueDenominatorBand
                                                        RgbChannel.Blue
                                                        RgbBandRole.Denominator

                                                    div [attributesEdit] [ Html.SemUi.iconCheckBox (m.editImages |> AVal.map (fun editImages -> List.contains index editImages)) (EditImage index)]
                                                    div [attributesWavelength] [
                                                        Incremental.text (
                                                            img.selectedChannel
                                                            |> AVal.map (fun channel ->
                                                                match channel.name with
                                                                | Some wavelength -> wavelength
                                                                | None -> "-"
                                                            )
                                                        )
                                                    ]
                                                    div [attributesAttr1] [ Incremental.text (img.distance |> AVal.map (fun f -> sprintf "%.2f" f)) ]
                                                    div [attributesAttr2] [ Incremental.text (img.time |> AVal.map string) ]
                                                ]
                                        
                                            Incremental.div AttributeMap.empty (
                                                alist { 
                                                    let! isInEditMode = m.editImages |> AVal.map (fun editEntries -> List.contains index editEntries)
                                                    if isInEditMode then
                                                        div [attribute "style" $"border-top: 1px dotted rgba(255,255,255,0.5)"] [
                                                            showDOM img |> UI.map (fun msg -> Message.ImageMessage (index, msg))
                                                        ]
                                                    else
                                                        div [] []
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
                                text "Import Multispectral Image"
                        ]
                    ]
                    div [clazz "item"; style "border-bottom: solid 1px black; height: 30px; padding: 5px; display: flex; justify-content: space-between; align-items: center;"] [
                        div [] [text "Visualization:"]
                        div [style "margin-left: auto;"] [
                            Numeric.view' [NumericInputType.Slider] m.projectionOpacity |> UI.map SetProjectionOpacity
                        ]
                    ]
                    div [clazz "item"; style "margin-top: 10px;"] [
                        div [style "padding-left: 5px"] [text "RGB contrast:"]
                        Html.table [
                            Html.row "Lower percentile:" [
                                Numeric.view' [NumericInputType.InputBox] m.rgbComposite.lowerPercentile
                                |> UI.map SetRgbLowerPercentile
                            ]

                            Html.row "Upper percentile:" [
                                Numeric.view' [NumericInputType.InputBox] m.rgbComposite.upperPercentile
                                |> UI.map SetRgbUpperPercentile
                            ]

                            Html.row "Gamma:" [
                                Numeric.view' [NumericInputType.InputBox] m.rgbComposite.gamma
                                |> UI.map SetRgbGamma
                            ]
                        ]
                    ]
                    div [clazz "item"; style "margin-top: 10px;"] [
                        div [style "padding-left: 5px"] [text "Registration:"]
                        Html.table [  
                            Html.row "Roll:" [Numeric.view' [NumericInputType.InputBox] m.boresightAdjustment.roll |> UI.map SetRoll]
                            Html.row "Pitch:" [Numeric.view' [NumericInputType.InputBox] m.boresightAdjustment.pitch |> UI.map SetPitch]
                            Html.row "Yaw:" [Numeric.view' [NumericInputType.InputBox] m.boresightAdjustment.yaw |> UI.map SetYaw]
                        ]
                    ]
                ]

                div [] [
                    div [] [showRelative2DImage rgbTexture]   
                    div [style $"border: 2px solid black; margin-top: 10px"] [
                            contentImages
                    ]
                ]
            ]
            

        require Html.semui (
            body [] [

                div [] [
                    showAbsolute2DAnd3DImage m.sourceImagePath rgbTexture   
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