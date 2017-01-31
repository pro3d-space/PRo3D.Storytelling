namespace PRo3DGui

open System
open System.IO
open Suave

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open DomainModel
open Aether
open Aether.Operators

open GuiExperiments
   
module PRo3dTreeApp = 
    
    open DomHelpers

    type Actions = 
        | Click of Id 
        | Select of Properties
        | DragStart of Id   
        | Drop of Id
        | SetProperties of PropertiesApp.Action

    module Tree =        

        let treeView attribs children =  
            div [ clazz  "ui list" ] children

        let leaf id attribs children = 
            div [ yield clazz "item"; yield onMouseClick (fun o -> Click id);  ] [
                i [ yield clazz "file icon"; ] []
                div [ yield clazz "content"; ] children
            ]
        let node id isExpanded isSelected attribs label children (prop : Properties) =
            let item = 
                i [ 
                    clazz (if isExpanded id then "large caret down icon" else "large caret right icon");  
                    onMouseClick (fun o -> Click id)
                    
                ] []
                        
            let style = if isSelected id then [("textDecoration", "underline")] else []
            let style  = 
                match prop with
                    | SurfaceProperties p -> if not p.IsVisible.isActive then ("color", "grey") :: style else style
                    | _ -> style            
             
            div [ clazz "item";  ] [
                    item
                    div [ clazz "content"; ] [
                        div [ clazz "header noselect"; Style style; onMouseClick (fun o -> Select prop)] [ label ]
                        div [ clazz "description noselect"] [ label  ]
                        div [ clazz "list" ] (if isExpanded id then children else [])
                    ]
            ]

    let update (env : Env<Actions>) (model: DomainModel.DomainModel) (action : Actions) = 
        printfn "action: %A" action
        let expanded = model.UI.expanded
        match action with
            | Click i -> 
                let ui = if Set.contains i model.UI.expanded 
                            then { model.UI with expanded = Set.remove i model.UI.expanded }
                            else { model.UI with expanded = Set.add i model.UI.expanded }
                { model with UI = ui }
            | Select p ->                     
                let blub = { model with UI = { model.UI with selected = p }}
//                match p with
//                    | SurfaceProperties sa -> PropertiesApp.update (env |> Env.map SetProperties) blub (PropertiesApp.Action.SetSurfaceProperties sa)
//                    | SetProperties sa ->  PropertiesApp.update (env |> Env.map SetProperties) blub p
                blub
            | DragStart id -> 
                { model with UI = { model.UI with dragging = Some id }}
            | Drop id -> 
                match model.UI.dragging with
                    | Some source -> 
                        printfn "dragged: %d to %d" source id
                        model
                    | None -> 
                        failwith "drop without start"
            | SetProperties a ->                    
                PropertiesApp.update (env |> Env.map SetProperties) (model) a
               

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
                          (text (sprintf "%s" surf.Name))
                          (viewOpcs isExpanded surf.OPCs)
                          (SurfaceProperties surf)
            ]
        ]

    let viewSurfaces isExpanded isSelected (surfaces : DomainModel.Surfaces) =
        let surfaceViews = surfaces.Surfaces |> List.map (viewSurface isExpanded isSelected)
        Tree.node surfaces.Id isExpanded isSelected [] (text "Surfaces") surfaceViews NoProperties
        

    let viewMeasurements isExpanded isSelected (measurements : DomainModel.Measurement) = failwith "alalalalal"
    
    let (^) l r = sprintf "%s%s" l r

    let sceneLabel (model : DomainModel) =
        match model.Path with
            | Some _ -> model.Scene.Name 
            | None   -> model.Scene.Name ^ "*"

    let view (model : DomainModel.DomainModel) : DomNode<Actions> =
        let isExpanded id = Set.contains id model.UI.expanded
        let isSelected id = match model.UI.selected with
                                | SceneProperties p -> p.Id = id
                                | SurfaceProperties p -> p.Id = id
                                | NoProperties -> false
        div[] [
            div [ Style [ "border"           , "1px solid black"
                          "height"           , "35vh"
                          "overflowY"        , "auto"
                          "backgroundColor" , "#eeeeee"]] [
            
                div [] [
                    Tree.treeView [] [
                        Tree.node model.Scene.Id isExpanded isSelected []                           
                                  (text (sprintf "%s" (sceneLabel model)))
                                  [viewSurfaces isExpanded isSelected model.Scene.Surfaces]
                                  (SceneProperties model.Scene)
                    ]
                ]
            ] 
            PropertiesApp.view model |> Html.map SetProperties
        ]
    
    let app : App<_,_,_> =
        {
            initial = DomainModel.empty
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

//module SideBarApp =
//   
//    type Action = 
//        | SetTreeView          of PRo3dTreeApp.Actions
//        | SetProperties        of PropertiesApp.Action
//
//
//    let update env (model : FullModel) (action : Action) =
//        let selected_ = FullModel.DomainModel_ >-> DomainModel.Model.UI_ >-> UIState.selected_
//
//        match action with
//            | SetTreeView a   -> { model with DomainModel = PRo3dTreeApp.update (env |> Env.map SetTreeView)  model.DomainModel a }            
//            | SetProperties a -> 
//                let p = PropertiesApp.update (env |> Env.map SetProperties) (model.DomainModel.UI.selected) a
//                Optic.set selected_ p model
//
//    let view (model : FullModel) : DomNode<Action> =         
//        div [] [
//            div [ Style [ "border"           , "1px solid black"
//                          "height"           , "35vh"
//                          "overflowY"        , "auto"
//                          "backgroundColor" , "#eeeeee"]] [
//                  PRo3dTreeApp.view model.DomainModel |> Html.map SetTreeView 
//            ]
//            PropertiesApp.view model.DomainModel.UI.selected |> Html.map SetProperties
//        ]
//
//    let app = {
//        initial = FullModel.initial
//        update = update
//        view = view
//        onRendered = OnRendered.ignore
//        subscriptions = Subscriptions.none
//    }
     
module FullLayoutApp =
    
    let container a c = div ([clazz "ui container"] @ a) c

    let (=>) k v = k,v    

    type Action =
        | TreeAction of PRo3dTreeApp.Actions
        | ImportSurfacesAction of ImportDiscoverApp.Action
        | LoadSurfaces of ImportDiscoverApp.Model
        | NewProject
        | SaveProject
        | SaveAsProject
        | LoadProject
        | Abort

    let saveAndUpdate model targetPath = 
         let path_ = FullModel.DomainModel_ >-> DomainModel.DomainModel.Path_
         let newmodel = 
                    match targetPath with
                        | Some p -> Optic.set path_ targetPath model
                        | None -> model

         targetPath |> Option.iter (Serialization.saveProject newmodel.DomainModel)
         Serialization.saveProject newmodel.DomainModel "./last"
         newmodel 
                 
    let update form e (m : FullModel) a =
        match a with
            | TreeAction a -> 
                { m with DomainModel = PRo3dTreeApp.update (e |> Env.map TreeAction) m.DomainModel a }
            | ImportSurfacesAction a -> 
                { m with importSurfaces = ImportDiscoverApp.update (e |> Env.map ImportSurfacesAction) m.importSurfaces a }
            | LoadSurfaces surf ->                  
                let newDomainModel,newSurface =  DomainModel.addDiscoveredSurfaces surf m.DomainModel                             
                let newM = { m with importSurfaces = ImportDiscoverApp.initial; DomainModel = newDomainModel; }

                Optic.set FullModel.Selected_ (SurfaceProperties newSurface) newM
            | NewProject -> FullModel.initial
            | SaveProject ->                
                let targetPath =
                    match m.DomainModel.Path with
                        | Some path ->  Some path
                        | None -> DialogShizzle.saveDialog form                

                saveAndUpdate m targetPath
            | SaveAsProject ->                
                let targetPath = DialogShizzle.saveDialog form

                saveAndUpdate m targetPath      
            | LoadProject ->
                match DialogShizzle.openDialog form with
                    | None -> m
                    | Some path -> 
                        let loadedModel = Serialization.loadProject path
                        { m with DomainModel =  loadedModel }
            | Abort -> printfn "aborting everything"; m

    let view (m : FullModel) =
        div [Style ["overflowY" => "hidden"
                    "overflowX" => "hidden"] ] [
            div [Style ["clear"  => "both";
                        "height" => "12vh"
                        "border" => "0px solid black"]] [Helpers.ribbon NewProject SaveProject LoadProject]

            div [clazz "clearfix"] [
                div [Style [//"display" , "block";
                            "float"   => "left"
                            "height"  => "88vh"                        
                            "width"   => "70%"
                            "opacity" => "0.0"
                            "border"  => "1px solid black"
                            "backgroundColor" => "#FFFFFF"]
                     attribute "id" "renderControl"] []
                
                div [Style [//"display" , "block";
                            "float"   => "left";
                            "border"  => "0px solid black";
                            "height"  => "88vh"
                            "width"   => "30%"                   
                            ]] [PRo3dTreeApp.view m.DomainModel |> Html.map TreeAction]
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
        
    let fileExists path =
        if File.Exists(path) then
            Some path
        else
            None

    let loadLastScene =
        match fileExists "./last" with
            | Some p -> Serialization.loadProject p
            | None   -> DomainModel.empty

    let app onRenderControlChanged form = 
        {
            initial    = { FullModel.initial with DomainModel = loadLastScene }
            update     = update form
            view       = view
            onRendered = onRendered onRenderControlChanged
            subscriptions = Subscriptions.none
        }

