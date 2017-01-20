namespace PRo3DGui

open System
open Suave

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open DomainModel

open GuiExperiments

module PRo3dTreeApp = 
    
    open DomHelpers

    type Actions = 
        | Click of Id 
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
        let node id isExpanded attribs label children =
            let item = 
                i [ 
                    clazz (if isExpanded id then "large caret down icon" else "large caret right icon");  
                    onMouseClick (fun o -> Click id)
                    
                ] []
            div [ clazz "item";  ] [
                    item
                    div [ clazz "content"; ] [
                        div [ clazz "header"] [ label ]
                        div [ clazz "description"] [ label  ]
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

    let viewSurface isExpanded (surf: DomainModel.Surface) = 
        div [] [
            Tree.treeView [] [
                Tree.node surf.Id isExpanded [] 
                          (text surf.Name) 
                          (viewOpcs isExpanded surf.OPCs)
            ]
        ]

    let viewSurfaces isExpanded (surfaces : DomainModel.Surfaces) =
        let surfaceViews = surfaces.Surfaces |> List.map (viewSurface isExpanded)
        Tree.node surfaces.Id isExpanded [] (text "Surfaces") surfaceViews

    let view (model : DomainModel.Model) : DomNode<Actions> =
        let isExpanded id = Set.contains id model.UI.expanded
        div [] [
            Tree.treeView [] [
                Tree.node model.Scene.Id isExpanded [] 
                          (text model.Scene.Name) 
                          [viewSurfaces isExpanded model.Scene.Surfaces]
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

module SurfacePropertiesApp =
    
    let column c = div [clazz "column"] c
    let row c = div [clazz "row"] c

    type Model = DomainModel.Surface
    
    type Action =
        | Set_ShadingMode       of Choice.Action
        | Set_Texture           of Choice.Action
        | Set_SetMap            of Choice.Action
        | Set_RenderMode        of Choice.Action
        | Set_IsVisible         of Toggle.Action
        | Set_IsActive          of Toggle.Action
        | Set_TriangleThreshold of Numeric.Action
        | Set_Trafo             of Transformation.Action
    
    let update (env: Env<Action>) (model : Model) (action : Action) =
        match action with
            | Set_ShadingMode       a -> { model with ShadingMode = Choice.update model.ShadingMode a }
            | Set_Texture           a -> { model with Texture = Choice.update model.Texture a }
            | Set_SetMap            a -> { model with Map = Choice.update model.Map a }
            | Set_RenderMode        a -> { model with RenderMode = Choice.update model.RenderMode a }
            | Set_IsVisible         a -> { model with IsVisible = Toggle.update model.IsVisible a }
            | Set_IsActive          a -> { model with IsActive = Toggle.update model.IsActive a }
            | Set_TriangleThreshold a -> { model with TriangleThreshold = Numeric.update env model.TriangleThreshold a }   
            | Set_Trafo             a -> { model with Trafo = Transformation.update env model.Trafo a }   
    
    let collapsing = "collapsing"
    let view (model : Model) : DomNode<Action> =
        div [] [
            accordion "Surface Properties" "options" true [
                table [clazz "ui celled striped table unstackable"] [
                    tbody [] [
                        tr [] [
                            td [clazz collapsing] [text "Name:"]; 
                            td [clazz "right aligned"] [text model.Name]
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Visiblity:"]; 
                            td [clazz "right aligned"] [Toggle.view model.IsVisible |> Html.map Set_IsVisible]
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Active State:"]; 
                            td [clazz "right aligned"] [Toggle.view model.IsActive |> Html.map Set_IsActive]
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Shading Mode:"]; 
                            td [clazz "right aligned"] [Choice.view model.ShadingMode |> Html.map Set_ShadingMode] 
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Texture:"]; 
                            td [clazz "right aligned"] [Choice.view model.Texture |> Html.map Set_Texture] 
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Map:"];
                            td [clazz "right aligned"] [Choice.view model.Map |> Html.map Set_SetMap] 
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Render Mode:"];
                            td [clazz "right aligned"] [Choice.view model.RenderMode |> Html.map Set_RenderMode] 
                        ]                    
                        tr [] [
                            td [clazz collapsing] [text "TriangleThreshold:"];
                            td [clazz "right aligned"] [Numeric.view model.TriangleThreshold |> Html.map Set_TriangleThreshold]
                        ]                                 
                    ]
                ]
            ]        

            accordion "Transformation" "maximize" false [
                table [clazz "ui celled striped table unstackable"] [
                        tbody [] [
                            tr [] [
                                td [clazz collapsing] [text "Trafo:"];
                                td [clazz "right aligned"] [Transformation.view model.Trafo |> Html.map Set_Trafo]
                            ]    
                        ]
                    ]
                ]
        ]

//    let app : App<_,_,_> = {
//        initial = DomainModel.empty.Surface
//        update = update
//        view = view
//        onRendered = OnRendered.ignore
//        subscriptions = Subscriptions.none
//    }



module SideBarApp =


    type Model = {
        DomainModel       : DomainModel.Model
        SurfaceProperties : Option<SurfacePropertiesApp.Model>
    }


    let initial = {
        DomainModel       = DomainModel.empty
        SurfaceProperties = None
    }

    type Action = 
        | SetTreeView          of PRo3dTreeApp.Actions
        | SetSurfaceProperties of SurfacePropertiesApp.Action

    let update env (model : Model) (action : Action) =
        match action with
            | SetTreeView a          -> { model with DomainModel = PRo3dTreeApp.update (env |> Env.map SetTreeView)  model.DomainModel a }
            | SetSurfaceProperties a -> 
                match model.SurfaceProperties with
                    | Some props -> 
                      { model with SurfaceProperties = SurfacePropertiesApp.update (env |> Env.map SetSurfaceProperties) props a |> Some }
                    | None -> model

    let view (model : Model) : DomNode<Action> = 
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
        initial = initial
        update = update
        view = view
        onRendered = OnRendered.ignore
        subscriptions = Subscriptions.none
    }

module Helpers = 
    let container a c = div ([clazz "ui container fluid"] @ a) c
    let ribbonMargin = Style ["margin", "0px"]

    let fileMenu() = button [clazz "ui labeled icon button"; Callback( "onClick", "doit();") ] [
                        i [clazz "folder icon"] []
                        text "Load Surface"]

    let ribbon() = 
        div [Style ["height", "9vh"; "padding", "0px"]] [
            div [clazz "ui top attached tabular menu"; ] [                
                    a [clazz "item"; attribute "data-tab" "first"] [text "File"]
                    a [clazz "item active"; attribute "data-tab" "second"] [text "Home"]
                    a [clazz "item"; attribute "data-tab" "third"] [text "Third"]
                    a [clazz "item"; attribute "data-tab" "fourth"] [text "Fourth"]
                                
            ]            
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "first"; ribbonMargin] [fileMenu()] 
            div [clazz "ui bottom attached tab segment active"; attribute "data-tab" "second"; ribbonMargin] [text "Second"] 
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "third"; ribbonMargin] [text "Third"]
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "fourth"; ribbonMargin] [text "Fourth"]
        ]
            


    let runModal innerview accept deny =
        div [clazz "ui modal"] [
            i [clazz "close icon"] []
            div [clazz "header"] [text "Modal Title"]
            div[clazz "content"] [
                innerview
            ]
            div [clazz "actions"] [
                div [clazz "ui button deny"; onMouseClick (fun _ -> deny)] [text "nope"]
                div [clazz "ui button positive"; onMouseClick (fun _ -> accept)] [text "yes"]
            ]
        ]

module FullLayoutApp =
    let container a c = div ([clazz "ui container"] @ a) c

    let (=>) k v = k,v

    type Model = { sidebar : SideBarApp.Model; importSurfaces : ImportDiscoverApp.Model }

    type Action =
        | SidebarAction of SideBarApp.Action
        | ImportSurfacesAction of ImportDiscoverApp.Action
        | LoadSurfaces of ImportDiscoverApp.Model
        | Abort

    let update e m a =
        match a with
            | SidebarAction a -> { m with sidebar = SideBarApp.update (e |> Env.map SidebarAction) m.sidebar a }
            | ImportSurfacesAction a -> { m with importSurfaces = ImportDiscoverApp.update (e |> Env.map ImportSurfacesAction) m.importSurfaces a }
            | LoadSurfaces surf ->  
                let newDomainModel,newSurface =  DomainModel.addDiscoveredSurfaces surf m.sidebar.DomainModel 
                { m with importSurfaces = ImportDiscoverApp.initial; 
                         sidebar = 
                            { m.sidebar with 
                                DomainModel = newDomainModel
                                SurfaceProperties = Some newSurface
                            } 
                }
            | Abort -> printfn "aborted thing"; m

    let view (m : Model) =
        div [Style ["overflowY" => "hidden"
                    "overflowX" => "hidden"] ] [
            div [Style ["clear"  => "both";
                        "height" => "12vh"
                        "border" => "0px solid black"]] [Helpers.ribbon()]

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
                            ]] [SideBarApp.view m.sidebar |> Html.map SidebarAction]
            ]
            Helpers.runModal (ImportDiscoverApp.view m.importSurfaces |> Html.map ImportSurfacesAction) (LoadSurfaces m.importSurfaces) Abort

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
        

    let app onRenderControlChanged = 
        {
            initial    = { sidebar = SideBarApp.initial; importSurfaces = ImportDiscoverApp.app.initial }
            update     = update
            view       = view
            onRendered = onRendered onRenderControlChanged
            subscriptions = Subscriptions.none
        }

