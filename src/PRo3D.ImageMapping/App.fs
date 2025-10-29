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

type Self = Self

module App =

    let initial : Model = {
        images = IndexList.Empty;
        selectedImage = None;
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
        | SelectImage img -> 
            { m with selectedImage = Some img }
        | ImageMessage (idx, imageMessage) ->
            m

    let view (m : AdaptiveModel) (showImage : AdaptiveImage -> DomNode<ImageMessage>) =
    
        let listAttributes =
            amap {
                yield clazz "ui divided list inverted segment"
                yield style "overflow-y : hidden"
            } |> AttributeMap.ofAMap

        let jsImportDialog =
            "top.aardvark.dialog.showOpenDialog({tile: 'Select directory', filters: [{ name: 'directories'}], properties: ['openDirectory']}).then(result => {aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"

        let content = 
            Incremental.div listAttributes (
                alist {
    
                    let! selected = m.selectedImage
                    let white = sprintf "color: %s" (Html.color C4b.White)

                    let domNodes = 
                        m.images 
                        |> AList.mapi (fun index img ->
                            let t = img |> Image.view
                            div [clazz "item"; style white] [
                                i [clazz "bookmark middle aligned icon"; onClick (fun _ -> SelectImage index);] []
                                div [clazz "content"; style white] [                     
                                    div [style white] [
                                        let descriptionText = sprintf "attr_A %A | attr_B %A" 0 0
                                        yield div [clazz "description"] [text descriptionText]
                                    ]  
                                    match selected with
                                        | Some idx when idx == index -> 
                                            showImage img |> UI.map (fun msg -> Message.ImageMessage (idx, msg))
                                        | Some _
                                        | None -> 
                                            div [] []
                                    ]
                            ])

                    
                    yield                 
                        text "Texture:" 

                    yield
                        button [
                            clazz "ui button tiny";
                            style "margin-left: 10px";
                            Dialogs.onChooseDirectory (Guid.NewGuid()) (fun (guid, chosen) -> LoadImagesDir (chosen) );
                            clientEvent "onclick" (jsImportDialog)
                        ] [
                            text "Import"
                        ]

                    for domNode in domNodes do
                        yield domNode
                })

        let accordion text' icon active content' =
                let title = if active then "title active inverted" else "title inverted"
                let content = if active then "content active" else "content"
               // let arrow = if active then 
                                    
                onBoot "$('#__ID__').accordion();" (
                    div [clazz "ui inverted segment"] [
                        div [clazz "ui inverted accordion fluid"] [
                            div [clazz title; style "background-color: #282828"] [
                                    i [clazz ("dropdown icon")] []
                                    text text'                                
                                    div [style "float:right"] [i [clazz (icon + " icon")] []]
                                
                            ]
                            div [clazz content;  style "overflow-y : auto; "] content' //max-height: 35%
                        ]
                    ]
                )

        require Html.semui (
            body [] [
                div [style "position: fixed; left: 20px; top: 20px; width: 400px"] [
                    accordion "Texture Mapping" "file image outline" false [ content ]
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