namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO

type Self = Self

module App =

    let initial : Model = {
        images = IndexList.Empty;
        selectedImage = None;
        editImages = None;
    }

    let update (m : Model) (msg : Message) = 
        match msg with
        | LoadImagesDir directory -> 
            let imageExts = [".tif";".tiff";".jpg";".exr"]
            let images' = 
                Directory.EnumerateFiles(directory) 
                |> Seq.filter (fun p -> 
                    let e = Path.GetExtension p
                    List.contains e imageExts 
                )
                |> Seq.map (fun path -> 
                    Image.loadFile(path)
                ) |> IndexList.ofSeq
            { m with images = images' }
        | SelectImage idx -> 
            { m with selectedImage = Some idx }
        | EditImage idx ->
            m
        | ImageMessage (idx, imageMessage) ->
            let images' = m.images |> IndexList.mapi (fun index img ->
                    if index == idx then
                        Image.update img imageMessage
                    else
                        img
                )
            m
        | SortEntriesByDistance ->
            let images' = 
                m.images
                |> IndexList.toList
                |> List.sortBy (fun p -> p.defaultMinValue)
                |> IndexList.ofList
            { m with images = images' }
        | SortEntriesByDate ->
            let images' = 
                m.images
                |> IndexList.toList
                |> List.sortBy (fun p -> p.defaultMaxValue)
                |> IndexList.ofList
            { m with images = images' }

    let view (m : AdaptiveModel) (showImage : AdaptiveImage -> DomNode<ImageMessage>) =
    
        let listAttributes =
            amap {
                yield clazz "ui divided list inverted segment"
                yield style "overflow-y : hidden"
            } |> AttributeMap.ofAMap

        let jsImportDialog =
            "top.aardvark.dialog.showOpenDialog({tile: 'Select directory', filters: [{ name: 'directories'}], properties: ['openDirectory']}).then(result => {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"

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
            let attributesSelect = attribute "style" "cursor: pointer; width: 50px; height: 30px; border-right: 1px solid #ccc; padding-left: 3px;"
            let attributesEdit = attribute "style" "cursor: pointer; width: 50px; height: 30px; border-right: 1px solid #ccc; padding-left: 3px;"
            let attributesAttr1 = attribute "style" "cursor: pointer; width: 120px; height: 30px; border-right: 1px solid #ccc; padding-left: 3px;"
            let attributesAttr2 = attribute "style" "cursor: pointer; width: 120px; height: 30px; padding-left: 3px;"

            let header =
                div [ 
                    // attribute "clazz" "title active inverted"
                    attribute "style" "display: flex; font-weight: bold; border-bottom: 2px solid #ccc;"
                ] [
                    div [ attributesSelect ] [text "Select"]
                    div [ attributesEdit ] [text "Edit"]
                    div [ attributesAttr1 ] [
                        i [clazz "sort icon"; onClick (fun _ -> SortEntriesByDistance);] []
                        text "Dist. to Planet"
                    ]
                    div [ attributesAttr2 ] [
                        i [clazz "sort icon"; onClick (fun _ -> SortEntriesByDate);] []
                        text "Sth else"
                    ]
                ]
            Incremental.div (AttributeMap.ofList [ attribute "class" "table-container" ]) (
                alist {
                    yield header

                    yield Incremental.div (AttributeMap.ofList [ attribute "style" "max-height: 400px; overflow-y: auto; " ]) (
                        alist {
                        let! editEntries = m.editImages

                        let domNodes = 
                            m.images 
                            |> AList.mapi (fun index img ->
                                div [attribute "style" "border-bottom: 1px solid #ccc;"] [
                                    div [
                                        attribute "style" "display: flex; font-weight: bold;"] 
                                        [
                                            div [attributesSelect] [ Html.SemUi.iconCheckBox (m.selectedImage |> AVal.map (fun selIdx -> selIdx = Some index)) (SelectImage index)]
                                            div [attributesEdit] [ i [clazz "edit icon"; onClick (fun _ -> EditImage index);] [] ]
                                            div [attributesAttr1] [ Incremental.text (img.defaultMinValue |> AVal.map string) ]
                                            div [attributesAttr2] [ Incremental.text (img.defaultMaxValue |> AVal.map string) ]
                                        ]
                                    match editEntries with
                                        | Some indices when List.contains index indices -> 
                                            div [attribute "style" "border-style: double"] [
                                                showImage img |> UI.map (fun msg -> Message.ImageMessage (index, msg))
                                            ]
                                        | Some _
                                        | None -> 
                                            div [] []
                                ]
                            )
                        for domNode in domNodes do
                            yield domNode
                    })
                })


        let content = 
            div [] [
                button [
                    clazz "ui button tiny";
                    style "margin-left: 10px";
                    Dialogs.onChooseDirectory (Guid.NewGuid()) (fun (guid, chosen) -> LoadImagesDir (chosen) );
                    clientEvent "onclick" (jsImportDialog)
                ] [
                    text "Import Directory"
                ]
                Incremental.div AttributeMap.empty (
                    AList.ofAVal (
                        AVal.map (fun c ->
                            if c > 0 then
                                [ 
                                    div [style "border: 1px solid #ccc; margin-top: 10px"] [
                                         contentImages
                                    ] 
                                ]
                            else
                                []
                        ) (AList.count m.images)
                    )
                )
            ]
            

        require Html.semui (
            body [] [
                div [style "position: fixed; left: 20px; top: 20px; width: 400px"] [
                    accordion "Texture Mapping" "file image outline" false (clazz "ui inverted segment") [ content ]
                ]
            ])

    let app () =
        {
            initial = initial
            update = update
            view = (fun m -> view m Image.view)
            threads = constF ThreadPool.empty
            unpersist = Unpersist.instance
        }