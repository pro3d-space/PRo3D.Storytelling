namespace PRo3DGui

open System
open Suave

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open DomainModel
open Aether
open Aether.Operators

open GuiExperiments


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

module PRo3dTreeApp = 
    
    open DomHelpers

    type Actions = 
        | Click of Id 
        | Select of Id 
        | DragStart of Id   
        | Drop of Id

    module Tree =        

        let treeView attribs children =  
            div [ clazz  "ui list" ] children

        let leaf id attribs children = 
            div [ yield clazz "item"; yield onMouseClick (fun o -> Click id);  ] [
                i [ yield clazz "file icon"; ] []
                div [ yield clazz "content"; ] children

            ]
        let node id isExpanded isSelected attribs label children =
            let item = 
                i [ 
                    clazz (if isExpanded id then "large caret down icon" else "large caret right icon");  
                    onMouseClick (fun o -> Click id)
                    
                ] []

            let selected = if isSelected id then Style ["text-decoration", "underline"] else Style []
            div [ clazz "item";  ] [
                    item
                    div [ clazz "content"; ] [
                        div [ clazz "header noselect"; selected; onMouseClick (fun o -> Select id)] [ label ]
                        div [ clazz "description noselect"] [ label  ]
                        div [ clazz "list" ] (if isExpanded id then children else [])
                    ]
            ]

    let update (env : Env<Actions>) (model: DomainModel.Model) (action : Actions) = 
        printfn "action: %A" action
        let expanded = model.UI.expanded
        let ui =
            match action with
                | Click i -> 
                    if Set.contains i model.UI.expanded 
                    then { model.UI with expanded = Set.remove i model.UI.expanded }
                    else { model.UI with expanded = Set.add i model.UI.expanded }
                | Select id -> 
                    printf "selected %d" id
                    { model.UI with selected = Some id }
                | DragStart id -> 
                    { model.UI with dragging = Some id }
                | Drop id -> 
                    match model.UI.dragging with
                        | Some source -> 
                            printfn "dragged: %d to %d" source id
                            model.UI
                        | None -> 
                            failwith "drop without start"
        { model with UI = ui}

    let viewOpcs isExpanded (opcs : list<OPC>) =
        [ for opc in opcs do
            yield Tree.leaf opc.id [] [text opc.name]
        ]

    let viewSurface isExpanded isSelected (surf: DomainModel.Surface) = 
        div [] [
            Tree.treeView [] [
                Tree.node surf.Id 
                          isExpanded
                          isSelected 
                          []
                          (text surf.Name) 
                          (viewOpcs isExpanded surf.OPCs)
            ]
        ]

    let viewSurfaces isExpanded isSelected (surfaces : DomainModel.Surfaces) =
        let surfaceViews = surfaces.Surfaces |> List.map (viewSurface isExpanded isSelected)
        Tree.node surfaces.Id isExpanded isSelected [] (text "Surfaces") surfaceViews
        

    let viewMeasurements isExpanded isSelected (measurements : DomainModel.Measurement) = failwith "alalalalal"
//        let sceneObjectViews = sceneObjects |> List.map (viewSurface isExpanded isSelected)
//        Tree.node surfaces.Id isExpanded isSelected [] (text "Surfaces") surfaceViews

    let view (model : DomainModel.Model) : DomNode<Actions> =
        let isExpanded id = Set.contains id model.UI.expanded
        let isSelected id = match model.UI.selected with
                                | Some x -> x = id
                                | None -> false            
        div [] [
            Tree.treeView [] [
                Tree.node model.Scene.Id isExpanded isSelected [] 
                          (text model.Scene.Name) 
                          [viewSurfaces isExpanded isSelected model.Scene.Surfaces]
            ]
        ]
        //div [] (List.init 1000 (fun b -> button [] [Text "ABC"]))
    
    let app : App<_,_,_> =
        {
            initial = DomainModel.empty
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }


type FullModel = {
        DomainModel       : DomainModel.Model
        importSurfaces    : ImportDiscoverApp.Model 
        SurfaceProperties : Option<SurfacePropertiesApp.Model>
    } with
        static member DomainModel_ =
            (fun fm -> fm.DomainModel), (fun dm fm -> { fm with DomainModel = dm})

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FullModel = 
    let initial = {
        DomainModel       = DomainModel.empty
        importSurfaces    = ImportDiscoverApp.app.initial
        SurfaceProperties = None
    }

module SideBarApp =
   
    type Action = 
        | SetTreeView          of PRo3dTreeApp.Actions
        | SetSurfaceProperties of SurfacePropertiesApp.Action

    let update env (model : FullModel) (action : Action) =
        match action with
            | SetTreeView a          -> { model with DomainModel = PRo3dTreeApp.update (env |> Env.map SetTreeView)  model.DomainModel a }
            | SetSurfaceProperties a -> 
                match model.SurfaceProperties with
                    | Some props -> 
                      { model with SurfaceProperties = SurfacePropertiesApp.update (env |> Env.map SetSurfaceProperties) props a |> Some }
                    | None -> model

    let view (model : FullModel) : DomNode<Action> = 
        let properties =
            match model.SurfaceProperties with
                    | Some s ->  [ SurfacePropertiesApp.view s |> Html.map SetSurfaceProperties]
                    | _ -> []
        div [] [
            div [ Style [ "border"           , "1px solid black"
                          "height"           , "35vh"
                          "overflowY"        , "auto"
                          "background-color" , "#eeeeee"]] [
                  PRo3dTreeApp.view model.DomainModel |> Html.map SetTreeView 
                ]            
            

            div [Style [ "border", "1px solid black"; "height", "55vh"; "overflowY", "auto"]] properties
        ]

    let app = {
        initial = FullModel.initial
        update = update
        view = view
        onRendered = OnRendered.ignore
        subscriptions = Subscriptions.none
    }

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
        


module FullLayoutApp =
    
    let container a c = div ([clazz "ui container"] @ a) c

    let (=>) k v = k,v    

    type Action =
        | SidebarAction of SideBarApp.Action
        | ImportSurfacesAction of ImportDiscoverApp.Action
        | LoadSurfaces of ImportDiscoverApp.Model
        | SaveProject
        | LoadProject
        | Abort

    let update form e (m : FullModel) a =
        match a with
            | SidebarAction a -> SideBarApp.update (e |> Env.map SidebarAction) m a
            | ImportSurfacesAction a -> { m with importSurfaces = ImportDiscoverApp.update (e |> Env.map ImportSurfacesAction) m.importSurfaces a }
            | LoadSurfaces surf ->  
                let newDomainModel,newSurface =  DomainModel.addDiscoveredSurfaces surf m.DomainModel 
                { m with importSurfaces = ImportDiscoverApp.initial; DomainModel = newDomainModel; SurfaceProperties = Some newSurface }
            | SaveProject ->
                let path_ = FullModel.DomainModel_ >-> DomainModel.Model.Scene_ >-> Scene.Path_
              
                let targetPath =
                    match m.DomainModel.Path with
                        | Some path ->  Some path
                        | None -> DialogShizzle.saveDialog form
                
                targetPath |> Option.iter (Serialization.saveProject m.DomainModel)
                Optic.set path_ ("hilfe options") m         
            | LoadProject ->
                match DialogShizzle.openDialog form with
                    | None -> m
                    | Some path -> 
                        let loadedModel = Serialization.loadProject path
                        { m with DomainModel =  loadedModel }
            | Abort -> printfn "aborted thing"; m

    let view (m : FullModel) =
        div [Style ["overflowY" => "hidden"
                    "overflowX" => "hidden"] ] [
            div [Style ["clear"  => "both";
                        "height" => "12vh"
                        "border" => "0px solid black"]] [Helpers.ribbon SaveProject LoadProject]

            div [clazz "clearfix"] [
                div [Style [//"display" , "block";
                            "float"   => "left"
                            "height"  => "88vh"                        
                            "width"   => "70%"
                            "opacity" => "0.0"
                            "border"  => "1px solid black"
                            "background-color" => "#FFFFFF"]
                     attribute "id" "renderControl"] []
                
                div [Style [//"display" , "block";
                            "float"   => "left";
                            "border"  => "0px solid black";
                            "height"  => "88vh"
                            "width"   => "30%"
                           // "min-width"   => "400px"
                           // "max-width"   => "400px"
                            ]] [SideBarApp.view m |> Html.map SidebarAction]
            ]
            Helpers.runModal (ImportDiscoverApp.view m.importSurfaces |> Html.map ImportSurfacesAction) "Import Surface" (LoadSurfaces m.importSurfaces) Abort

//            div [Style ["clear"  , "both";
//                        "height", "4vh"
//                        "width"   => "70%"
//                        "border" ,  "1px solid black"]] [text "Footer"]
        ]

    let onRendered onRenderControlChanged model view =
        {
            clientSide = JsLambda """() => { 
                var rect = document.getElementById("renderControl").getBoundingClientRect();
                return { bottom : rect.bottom.toFixed(), height : rect.height.toFixed(), left : rect.left.toFixed(), right : rect.right.toFixed(), top : rect.top.toFixed(), width : rect.width.toFixed() }; 
            } """   
            serverSide = fun (s : string) -> 
                let rect = ClientRect.ofString s
                printfn "clientRect: %A" rect
                onRenderControlChanged rect
                None
        }
        

    let app onRenderControlChanged form = 
        {
            initial    = FullModel.initial
            update     = update form
            view       = view
            onRendered = onRendered onRenderControlChanged
            subscriptions = Subscriptions.none
        }

