namespace PRo3DGui

open System
open Suave

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open TestModel

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

    let update (model: TestModel.Model) (action : Actions) = 
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

    let viewSurface (surf: TestModel.Surface) = 
        Tree.leaf surf.Id [] [text surf.Name]

    let viewSurfaces isExpanded (surfaces : TestModel.Surfaces) =
        let surfaceViews = surfaces.Surfaces |> List.map viewSurface
        Tree.node surfaces.Id isExpanded [] (text "Surfaces") surfaceViews

    let view (model : TestModel.Model) : DomNode<Actions> =
        let isExpanded id = Set.contains id model.UI.expanded
        div [] [
            Tree.treeView [] [
                Tree.node model.Scene.Id isExpanded [] 
                          (text model.Scene.Name) 
                          [viewSurfaces isExpanded model.Scene.Surfaces]
            ]
        ]
        //div [] (List.init 1000 (fun b -> button [] [Text "ABC"]))
    


    let app =
        {
            initial = TestModel.Model.initial
            update = update
            view = view
            onRendered = OnRendered.ignore
        }

module SurfacePropertiesApp =
    
    let column c = div [clazz "column"] c
    let row c = div [clazz "row"] c

    type Model = TestModel.Surface
    
    type Action =
        | Set_ShadingMode       of Choice.Action
        | Set_Texture           of Choice.Action
        | Set_SetMap            of Choice.Action
        | Set_RenderMode        of Choice.Action
        | Set_IsVisible         of Toggle.Action
        | Set_IsActive          of Toggle.Action
        | Set_TriangleThreshold of Numeric.Action
        | Set_Trafo             of Transformation.Action
    
    let update (model : Model) (action : Action) =
        match action with
            | Set_ShadingMode       a -> { model with ShadingMode = Choice.update model.ShadingMode a }
            | Set_Texture           a -> { model with Texture = Choice.update model.Texture a }
            | Set_SetMap            a -> { model with Map = Choice.update model.Map a }
            | Set_RenderMode        a -> { model with RenderMode = Choice.update model.RenderMode a }
            | Set_IsVisible         a -> { model with IsVisible = Toggle.update model.IsVisible a }
            | Set_IsActive          a -> { model with IsActive = Toggle.update model.IsActive a }
            | Set_TriangleThreshold a -> { model with TriangleThreshold = Numeric.update model.TriangleThreshold a }   
            | Set_Trafo             a -> { model with Trafo = Transformation.update model.Trafo a }   
    
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

    let app = {
        initial = TestModel.Model.initial.Surface
        update = update
        view = view
        onRendered = OnRendered.ignore
    }

module SideBarApp =
    type Model = {
        FullModel         : TestModel.Model
        SurfaceProperties : SurfacePropertiesApp.Model
    }


    let initial = {
        FullModel         = TestModel.Model.initial
        SurfaceProperties = TestModel.Model.initial.Surface
    }

    type Action = 
        | SetTreeView          of PRo3dTreeApp.Actions
        | SetSurfaceProperties of SurfacePropertiesApp.Action

    let update (model : Model) (action : Action) =
        match action with
            | SetTreeView a          -> { model with FullModel = PRo3dTreeApp.update model.FullModel a }
            | SetSurfaceProperties a -> { model with SurfaceProperties = SurfacePropertiesApp.update model.SurfaceProperties a }

    let view (model : Model) : DomNode<Action> = 
        div [] [
            div [Style [ "border"           , "1px solid black"
                         "height"           , "35vh"
                         "overflowY"        , "auto"
                         "background-color" , "#eeeeee"]] [ PRo3dTreeApp.view model.FullModel |> Html.map SetTreeView ]            
            div [Style [ "border", "1px solid black";
                         "height", "55vh";
                         "overflowY", "auto"]] [SurfacePropertiesApp.view initial.SurfaceProperties |> Html.map SetSurfaceProperties]
        ]

    let app = {
        initial = initial
        update = update
        view = view
        onRendered = OnRendered.ignore
    }

module FullLayoutApp2 = 
    let container a c = div ([clazz "ui container fluid"] @ a) c
    let ribbonMargin = Style ["margin", "0px"]

    let fileMenu() = button [clazz "ui labeled icon button"] [
                        i [clazz "folder icon"] []
                        text "Load Surface"]

    let ribbon() = 
        div [Style ["height", "9vh"; "padding", "0px"]] [
            div [clazz "ui top attached tabular menu"; ] [                
                    a [clazz "item active"; attribute "data-tab" "first"] [text "File"]
                    a [clazz "item"; attribute "data-tab" "second"] [text "Second"]
                    a [clazz "item"; attribute "data-tab" "third"] [text "Third"]
                    a [clazz "item"; attribute "data-tab" "fourth"] [text "Fourth"]
                                
            ]            
            div [clazz "ui bottom attached tab segment active"; attribute "data-tab" "first"; ribbonMargin] [fileMenu()] 
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "second"; ribbonMargin] [text "Second"] 
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "third"; ribbonMargin] [text "Third"]
            div [clazz "ui bottom attached tab segment"; attribute "data-tab" "fourth"; ribbonMargin] [text "Fourth"]
        ]
            
    type Model = SideBarApp.Model
    type Action = Inc | Dec
    // interactive module reloading fablish....

    let (=>) k v = attribute k v

    let canvas()  = elem "div" [ Style [ "border", "1px solid #000000"] ] []
    let update m a = SideBarApp.update m a
    let view m = 
        div [] [
            table [Style [ "width", "100%"]] [
                tbody[] [
                    tr [] [ td [Style [ "border", "1px solid black"]] [ribbon()]]
                    tr [] [ td [Style [ "border", "1px solid black"]] [                        
                                table [clazz "ui table"; Style [ "width", "100%"]] [
                                    tbody[] [
                                        tr [] [
                                            td [Style [ "border", "1px solid black"]] [
                                                div [Style ["width","100%"]] [text "render window"]
                                            ]
                                            td [clazz "collapsing"; Style [ "border", "1px solid black"]] [SideBarApp.view m]
                                        ]
                                    ]

                                ]
                            ]
                    ]
                ]
            ]                                   
        ]
    let app = 
        {
            initial = SideBarApp.initial
            update = update
            view = view
            onRendered = OnRendered.ignore
        }

module FullLayoutApp =
    let container a c = div ([clazz "ui container"] @ a) c

    let (=>) k v = k,v

    type Model = SideBarApp.Model
    let update m a = SideBarApp.update m a
    let view m =
        div [Style ["overflowY" => "hidden"
                    "overflowX" => "hidden"] ] [
            div [Style ["clear"  => "both";
                        "height" => "12vh"
                        "border" => "0px solid black"]] [FullLayoutApp2.ribbon()]

            div [clazz "clearfix"] [
                div [Style [//"display" , "block";
                            "float"   => "left"
                            "height"  => "88vh"                        
                            "width"   => "70%"
                            "border"  => "1px solid black"
                            "background-color" => "#CCCCCC"]
                     attribute "id" "renderControl"] []
                
                div [Style [//"display" , "block";
                            "float"   => "left";
                            "border"  => "0px solid black";
                            "height"  => "88vh"
                            "width"   => "30%"
                           // "min-width"   => "400px"
                           // "max-width"   => "400px"
                            ]] [SideBarApp.view m]
            ]

//            div [Style ["clear"  , "both";
//                        "height", "20px"
//                        "border" ,  "1px solid black"]] [text "Footer"]
        ]

    let onRendered onRenderControlChanged model view =
        {
            clientSide = """() => { 
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
            initial    = SideBarApp.initial
            update     = update
            view       = view
            onRendered = onRendered onRenderControlChanged
        }

