namespace PRo3DGui

open System
open System.IO
//open Suave

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open DomainModel
open Aether
open Aether.Operators

module Serialization =
    open MBrace.FsPickler
    open System.IO
    let binarySerializer = FsPickler.CreateBinarySerializer()

    let saveProject (project : DomainModel.Model) (path:string)  = 
        let arr = binarySerializer.Pickle project 
        File.WriteAllBytes(path, arr);


    let loadProject path = 
        let arr = File.ReadAllBytes(path);
        binarySerializer.UnPickle arr

module Helpers = 
    let container a c = div ([clazz "ui container fluid"] @ a) c
    let ribbonMargin = Style ["margin", "0px"]

    let sceneMenu save load = 
        div [clazz "ui buttons"] [
            button [clazz "ui icon button"] [
                    i [clazz "file outline icon"] []
            ]
            button [clazz "ui icon button"; onMouseClick (fun _ -> load)] [
                    i [clazz "folder open icon"] []
            ]
            button [clazz "ui icon button"; onMouseClick (fun _ -> save)] [
                    i [clazz "save icon"] []
            ]
            button [clazz "ui icon button"] [
                    i [clazz "icons"] [
                        i [clazz "save icon"] []
                        i [clazz "corner arrow down icon"] []
                    ]                            
            ]
        ]

    let fileRibbon save load= 
        div [clazz "ui grid"][
                div [clazz "ui two wide column "][sceneMenu save load]
                div [clazz "ui four wide column "][
                    div [clazz "ui buttons"] [
                        button [clazz "ui labeled icon button"; Callback( "onClick", "doit();")] [
                            i [clazz "folder icon"] [] 
                            text "Add Surface"
                        ]
                    ]    
            ]                                             
        ]

    let ribbon save load = 
        div [Style ["height", "9vh"; "padding", "0px"]] [
            div [clazz "ui top attached tabular menu"; ] [                
                    a [clazz "item active"; attribute "data-tab" "first"] [text "File"]
                    a [clazz "item"; attribute "data-tab" "second"] [text "Home"]
                    a [clazz "item"; attribute "data-tab" "third"] [text "Measurements"]                    
                    a [clazz "item"; attribute "data-tab" "fourth"] [text "Viewplanner"]         
                                
            ]            
            div [clazz "ui bottom attached tab segment active"; attribute "data-tab" "first"; ribbonMargin] [fileRibbon save load] 
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "second"; ribbonMargin] [text "Navigation AddObject Place_Object"]
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "third"; ribbonMargin] [text "Measurements Ref_Planes Scalebar"]
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "fourth"; ribbonMargin] [text "Viewplanner"]
        ]
            
    let runModal innerview title accept deny=
        div [clazz "ui modal"] [
            i [clazz "close icon"] []
            div [clazz "header"] [text title]
            div[clazz "content"] [
                innerview
            ]
            div [clazz "actions"] [
                div [clazz "ui button deny"; onMouseClick (fun _ -> deny)] [text "nope"]
                div [clazz "ui button positive"; onMouseClick (fun _ -> accept)] [text "yes"]
            ]
        ]

module DialogShizzle =
    open System.Windows.Forms
    open Microsoft.WindowsAPICodePack.Dialogs

    let openFolder () =
        let dialog = new CommonOpenFileDialog()
        dialog.InitialDirectory <- "C:\\Users"
        dialog.IsFolderPicker <- true
        if dialog.ShowDialog() = CommonFileDialogResult.Ok then
            Some dialog.FileName
        else None


    let openDialog (w : Form) =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new OpenFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r
        

    let saveDialog  (w : Form)  =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new SaveFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r