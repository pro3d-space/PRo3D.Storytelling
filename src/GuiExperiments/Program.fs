namespace GuiExperiments

open System
open Fablish

open System.Windows.Forms

open System.Collections.Generic
open Aardvark.Base

module PerformanceTest = 

    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
     
     type Model = int

     type Action = Inc | Dec

     let update env (m : Model) (a : Action) =
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

     let view (m : Model) =
        div [] [
            Text (sprintf "%A" m) 
            div [] (List.init m (fun i -> button [onMouseClick (fun _ -> Inc)] [Text (sprintf "%d" i)]))
        ]

    let app =
        {
            initial = 10000
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

module MetroTest =

    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html
    
    type Model = int

    //<div data-role="group" data-group-type="multi-state" data-button-style="class">
    //    <button class="button">1</button>
    //    <button class="button">2</button>
    //    <button class="button">3</button>
    //</div>

    let update env m _ = m


    let view (m : Model) =
        div [] [
            div [attribute "data-role" "group"; attribute "data-group-type" "multi-state"; attribute "data-button-style" "class"] [
                button [attribute "className" "button"] [Text "A"]
                button [attribute "className" "button"] [Text "B"]
                button [attribute "className" "button"] [Text "C"]
            ]
        ]

    let app =
        {
            initial = 10000
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }


open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module TestApp =
    
    type Model = int

    type Action = Inc | Dec

    let update env (m : Model) (a : Action) =
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

    let view (m : Model) : DomNode<Action> =
        div [] [
            text (sprintf "current content: %d" m)
            br []
            button [onMouseClick (fun dontCare -> Inc); attribute "id" "urdar"] [text "increment"]
            button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
        ]

    let onRendered model view =
        {
            clientSide = JsLambda """() => { 
                var rect = document.getElementById("urdar").getBoundingClientRect();
                return { bottom : rect.bottom.toFixed(), height : rect.height.toFixed(), left : rect.left.toFixed(), right : rect.right.toFixed(), top : rect.top.toFixed(), width : rect.width.toFixed() }; 
            } """   
            serverSide = fun (s : string) -> printfn "clientRect: %A" (ClientRect.ofString s); None
        }

    let app =
        {
            initial = 0
            update = update 
            view = view
            onRendered = onRendered
            subscriptions = Subscriptions.none
        }

module ManyTestThings =

    type Model = list<TestApp.Model>

    type Action = 
        | Change of int * TestApp.Action

    let update env (model : Model) (a : Action) =
        match a with
            | Change(i,action) -> List.updateAt i (fun a -> TestApp.update env a action) model

    let view (model : Model) : DomNode<Action> =
        let inner = 
            model |> List.mapi (fun i e ->
                TestApp.view e |> Html.map (fun innerAction -> Change(i, innerAction))
            )
        div [] inner

    let app = 
        {
            initial = [ 1; 2; 3 ]
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

module FileApp =
    
    type Model = list<string>

    type Message = Open | Accept | Deny 

    let update env (m : Model) (msg : Message) =
        match msg with
            | Open -> 
                use dialog = new System.Windows.Forms.OpenFileDialog()
                dialog.Multiselect <- true
                let r = dialog.ShowDialog()
                if r = DialogResult.OK then                    
                    (dialog.FileNames |> Array.toList) @ m
                else m
            | Accept -> m
            | Deny -> []

    let openModalButton =
        div [] [
            
            button [Callback( "onClick", "doit();")] [text "JD"]
        ]
    
    let modal (m) =
        div [clazz "ui modal"] [
            i [clazz "close icon"] []
            div [clazz "header"] [text "Modal Title"]
            div[clazz "content"] [
                div [clazz "image"][text "image here"]
                div [clazz "description"][button [clazz "ui button"; onMouseClick (fun _ -> Open)][text "add files"]]
                div [][
                    for i in m do
                        yield sprintf "file: %s" i |> text
                ]
            ]
            div [clazz "actions"] [
                div [clazz "ui button deny"; onMouseClick (fun _ -> Deny)] [text "nope"]
                div [clazz "ui button positive"; onMouseClick (fun _ -> Accept)] [text "yes"]
            ]
        ]

    let view (m : Model) =
        div [] [
            yield modal m
            yield openModalButton           
        ]


    let app =
        {
            initial = []
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

module Surfaces =
    
    type Model = 
        {
            currentlyLoaded : list<string>
            importer : FileApp.Model
        }
    type Msg = Import of list<string> | FileAppMsg of FileApp.Message

    let update env model msg =
        match msg with
            | Import imported -> 
                { importer = []; currentlyLoaded = model.currentlyLoaded @ imported }
            | FileAppMsg a -> { model with importer = FileApp.update env model.importer a }            

    let view m =
        div [] [
            yield FileApp.view m.importer |> Html.map (fun a -> match a with | FileApp.Accept _ -> Import m.importer | a -> FileAppMsg a)
            for a in m.currentlyLoaded do yield text (sprintf "loaded: %s" a)
        ]


    let app =
        {
            initial = { currentlyLoaded = []; importer = [] }
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

module CoolTest =
    
    type Model = { numeric : Numeric.Model; toggle : Toggle.Model }

    type Action = 
        | NumericAction of Numeric.Action
        | ToggleAction of Toggle.Action

    let update env (m : Model) (action : Action) =
        match action with
            | NumericAction a -> { m with numeric = Numeric.update env m.numeric a }
            | ToggleAction a -> { m with toggle = Toggle.update env m.toggle a }

    let view (m : Model) : DomNode<Action> =
        div [] [
            Numeric.view m.numeric |> Html.map (fun a -> NumericAction a)
            Toggle.view m.toggle |> Html.map (fun a -> ToggleAction a)
        ]

    let app = 
        {
            initial = { numeric = Numeric.initial; toggle = { isActive = true } }
            view = view
            update = update
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Text = 
    type Model = string

    type Action = 
        | Change of Model


    let update env (model : Model) (action : Action) =
        match action with
            | Change x -> x

    let view (model : Model) : DomNode<Action> =
        div [clazz "ui input fluid"] [
            input [
                Style ["textAlign","right"]
                attribute "value" (model.Replace("\\","\\\\"))
                attribute "type" "text"; //attribute "placeholder" "numeric";
                //attribute "size" "6"
                onChange (fun s -> Change (unbox s))
            ]
    ]
    
    let app = {
        initial = "empty"
        update = update
        view = view
        onRendered = OnRendered.ignore
        subscriptions = Subscriptions.none
    }

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Toggle = 
//    type Model = Toggle
//
//    type Action = 
//        | Toggle
//    
//    let update env (model : Model) (action : Action) =
//        match action with
//            | Toggle -> { model with isActive = not model.isActive }
//
//    let view (model : Model) : DomNode<Action> =         
//        let active = if model.isActive then "ui defaultChecked toggle checkbox" else "ui toggle checkbox"
//        let active' = if model.isActive then "checked" else ""
//        div [clazz active] [
//            input [attribute "type" "checkbox"; attribute "defaultChecked" active'; onMouseClick(fun _ -> Toggle)]
//            label [] [text ""]
//            
//        ]     
//        
//    let initial = { isActive = false }
//    
//    let app = {
//        initial = initial
//        update = update
//        view = view
//        subscriptions = Subscriptions.none
//        onRendered = OnRendered.ignore
//    }     
//
//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Choice = 
//    open DomHelpers
//
//    type Model = Choice
//
//    type Action = Select of string    
//
//    let update env (model : Model) (action : Action) =
//        match action with
//            | Select x -> { model with selected = x }
//  
//    let view<'a when 'a : equality> (model : Model) : DomNode<Action> =
//        let onChangeEvt o = o |> unbox |> Select
//        
//        select [clazz "ui dropdown"; attribute "defaultValue" model.selected; onChange onChangeEvt] [
//            for case in model.choices do yield option [] [text (sprintf "%s" case)]
//       ]  
//
//    let initial = {
//        choices = ["Lamber"; "Phong"; "Oren"]
//        selected = "Phong";
//    }
//
//    let app = {
//        initial = initial
//        update = update
//        view = view
//        subscriptions = Subscriptions.none
//        onRendered = OnRendered.ignore
//    }  


module ImportDiscoverApp =
    open PRo3DGui

    type Path = string

    type Model = PRo3DGui.DiscoveredSurfaces

    type Action =
        | SetPath       of Text.Action
        | Discover      of Path
        | SelectSurface of Path
        | AddSurface    of Path
        | Clear

    let discover path = 
        System.IO.Directory.EnumerateDirectories path |> Seq.map (fun a -> a.Replace(@"\", @"\\"))  |> Seq.toList



    let update env (model : Model) (action : Action) =
        match action with
            | SetPath       a -> { model with currentPath =  Text.update env model.currentPath a}
            | Discover      a -> { model with discoveredSurfaces = discover a }
            | SelectSurface a -> { model with selectedSurface = match model.selectedSurface with 
                                                                    | Some old when old = a -> None 
                                                                    | _ -> Some a}
            | _ -> model

    let view (m : Model) : DomNode<Action> =
        div [clazz "ui container"] [
            div [clazz "ui list"] [
                div [clazz "item"] [label [] [text "Surface Path:"]]
                div [clazz "item"] [Text.view m.currentPath |> Html.map SetPath]
                div [clazz "item"] [button [clazz "ui button"; onMouseClick (fun _ -> Discover m.currentPath)] [text "discover"]]
            ]
            div [Style[ "overflowY","auto"; "height", "200px"]] [
                form [clazz "ui divided items"] [
                    for s in m.discoveredSurfaces do
                        let icon =  match m.selectedSurface with
                                        | Some(x) -> if s.Equals x then "circular loading inverted icon folder" else "circular icon folder"
                                        | None -> "circular icon folder"
                        
                        yield 
                            div[clazz "item"; onMouseClick (fun _ -> SelectSurface s)] [
                                i [clazz icon][]
                                div[clazz "ui content"] [
                                    div[Style ["fontweight", "bold"]; clazz "middle aligned content"][text (sprintf "%s" (System.IO.Path.GetFileName(s)))] 
                                    div[clazz "middle aligned content"][text (sprintf "%s" s)] 
                                ]
                            ]                                    
                ]
            ]
        ]
        
    let initial = 
        { currentPath = ""
          discoveredSurfaces = []
          selectedSurface = None
          selectedSurface1 = None
          searchPath = []
          emptySurfaceName = ""
          opcPath = ""
          surfacesInfo = []
          surfaceName = ""
          maxTriangleSize = ""
          selectedOPC = None
          fromOPCPath = ""
          test = ""
        }

    let app =
        {
            initial = initial
            update = update
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

module DialogUtils =
    open System.Windows.Forms
    open Microsoft.WindowsAPICodePack.Dialogs

    let openFolder =
        let mutable path = ""
        let dialog = new Microsoft.WindowsAPICodePack.Dialogs.CommonOpenFileDialog()
        dialog.InitialDirectory <- "C:\\Users"
        dialog.IsFolderPicker <- true
        if dialog.ShowDialog() = CommonFileDialogResult.Ok then
            path <- System.IO.Path.GetDirectoryName dialog.FileName
        else path <- ""
        path

module ListUtils = 

    let addToList (listan:list<string>) (element:string) = element::listan
    let mergeLists a b =  b @ a

    let rec removeFromList list index  =
        match index, list with
            | 0, x::xs -> xs
            | index, x::xs -> x::removeFromList xs (index - 1)
            | index, [] -> failwith "index out of range"


module OPCFileUtils =
    
    // checks if "path" is a valid opc folder
    let isOPCPath (path : string) = 
        let imagePath = Path.combine [path; "images"]
        let patchPath = Path.combine [path; "patches"]
        let dirsExist = (System.IO.Directory.Exists imagePath) && (System.IO.Directory.Exists patchPath)
        let storageExists = System.IO.File.Exists(patchPath + "\\patchhierarchy.xml")
        if dirsExist && storageExists then
            true
        else false

    // checks if "path" is a valid surface folder
    let isSurfaceFolder (path : string) =
        let mutable isValid = false
        let subfolders = System.IO.Directory.GetDirectories(path)
        for s in subfolders do
            isValid <- (isOPCPath(s) || isValid)
        isValid
            
        
    // returns all valid OPCs in "path"
    let getOPCPaths path = 
        if System.IO.Directory.Exists path then
            System.IO.Directory.GetDirectories(path)
                |> Seq.choose (fun a -> if isOPCPath a then Some a else None)
                |> Seq.toList 
        else []



module ImportDiscoverAppLaura =
    open PRo3DGui
   
    
    type Path = string

    type Model = PRo3DGui.DiscoveredSurfaces
     
    type Action =
        | SetPath               of Text.Action
        | SetEmptySurfaceName   of Text.Action
        | SetOPCPath            of Text.Action
        | SearchPath            of Path
        | Discover              of Path
        | OpenFolder           
        | SelectSurface         of SurfaceInfo
        | AddSurface            of Path
        | AddOPCasSurface       of Path
        | DeleteSurface         of Path
        | ShowSurfaceName       of Text.Action
        | ShowTriangleSize      of Text.Action
        | SelectOPC             of Path
        | DeleteOPC             of Path
        | UpdateSurface         of SurfaceInfo
        | SetOPC                of Text.Action
        | AddOPC                of Path
        | Import                of list<SurfaceInfo>
        //| Cancel                of list<SurfaceInfo>
        | Clear

   
    
    // new surfaceInfo from path 
    let newSurfaceInfo (path :  Path) =  {surfaceName = path; opcPaths = OPCFileUtils.getOPCPaths path; maxTriangleSize = infinity}

    // new surfaceInfo
    let updateSurfaceInfo (name : string) (opcs : list<Path>) (maxTS : float) = 
        {surfaceName = name
         opcPaths = opcs
         maxTriangleSize = maxTS
         }
    
    // returns all valid surface folder in "path"
    let discover path = 
        if System.IO.Directory.Exists path then
            System.IO.Directory.EnumerateDirectories path 
                |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                |> Seq.filter (fun a -> OPCFileUtils.isSurfaceFolder a)
                |> Seq.toList 
        else []
    
    // generates new SurfaceInfos from path list
    let convert list =
         list 
            |> Seq.map(fun a -> newSurfaceInfo a) 
            |> Seq.toList 
    
    let addToList1 (listan:list<SurfaceInfo>) (element:SurfaceInfo) = element::listan


    let updateSurfaceName (m : Model) (n : string)= 
        match m.selectedSurface1 with
            | Some s -> if n.IsEmptyOrNull() then System.IO.Path.GetFileName(m.selectedSurface1.Value.surfaceName) else n
            | None -> "" 


    let updateTriangleSize (m : Model) = 
        match m.selectedSurface with
            | Some s -> (m.selectedSurface1.Value.maxTriangleSize).ToString()
            | None -> "Double.PositiveInfinity" 
        
    let deleteOPCPath (m : Model)= 
        match m.selectedOPC with
            | Some s -> m.selectedSurface1.Value.opcPaths |> List.choose (fun e -> if e = s then None else Some e)
            | None -> [] 

    let addOPCPath (m : Model)= 
        if (OPCFileUtils.isOPCPath m.fromOPCPath) then ListUtils.addToList m.selectedSurface1.Value.opcPaths m.fromOPCPath 
                                        else m.selectedSurface1.Value.opcPaths
    
    // checks if surface already exists in surfaceInfo list
    let isExistingSurfaces (m : Model) (p : Path) = m.surfacesInfo 
                                                        |> Seq.map (fun e -> System.IO.Path.GetFileName(e.surfaceName)) 
                                                        |> Seq.contains(System.IO.Path.GetFileName(p))
        
                                 

    let update (f : System.Windows.Forms.Form) env (model : Model) (action : Action) =
        match action with
            | SetPath       a -> { model with currentPath =  Text.update env model.currentPath a}
            | Discover      a -> { model with surfacesInfo = discover a
                                                                |> convert
                                                                |> ListUtils.mergeLists model.surfacesInfo }
            | OpenFolder    -> { model with surfacesInfo = DialogUtils.openFolder
                                                                |> discover
                                                                |> convert
                                                                |> ListUtils.mergeLists model.surfacesInfo }
            | SelectSurface a -> { model with selectedSurface1 = match model.selectedSurface1 with 
                                                                    | Some old when old = a -> None 
                                                                    | _ -> Some a}
            | SearchPath    a -> {model with searchPath = model.searchPath}
           // | SearchPath    a -> {model with surfacesInfo = openFolder |> convert |> mergeLists model.surfacesInfo}
            | SetEmptySurfaceName       a -> { model with emptySurfaceName =  Text.update env model.emptySurfaceName a}
            | SetOPCPath                a -> { model with opcPath =  Text.update env model.opcPath a}
            | AddSurface  a -> {model with surfacesInfo = if a.ToString().IsEmpty() then model.surfacesInfo 
                                                                                    else if isExistingSurfaces model a then model.surfacesInfo 
                                                                                                                        else newSurfaceInfo a |> addToList1 model.surfacesInfo }
            | AddOPCasSurface a -> {model with surfacesInfo = if not(OPCFileUtils.isOPCPath a) then model.surfacesInfo 
                                                                                    else if isExistingSurfaces model a then model.surfacesInfo 
                                                                                                                        else newSurfaceInfo a |> addToList1 model.surfacesInfo }
            | DeleteSurface  a -> 
                { model with 
                      surfacesInfo =
                        match model.selectedSurface1 with
                            | Some s -> model.surfacesInfo |> List.choose (fun e -> if e.surfaceName = s.surfaceName then None else Some e)
                            | None -> model.surfacesInfo

                }
            | ShowSurfaceName   a -> {model with surfaceName =  Text.update env model.surfaceName a}
//            | ShowSurfaceName   a -> {model with selectedSurface1.surfaceName =  match model.selectedSurface1.Value.surfaceName with
//                                                                | Some -> Text.update env model.selectedSurface1.Value.surfaceName a
//                                                                | None -> "" }    
            | ShowTriangleSize a -> { model with maxTriangleSize = Text.update env model.maxTriangleSize a}
            | SelectOPC a -> { model with selectedOPC = match model.selectedOPC with 
                                                                    | Some old when old = a -> None 
                                                                    | _ -> Some a}
            | DeleteOPC  a -> { model with selectedSurface1 = match model.selectedSurface1 with
                                                                | Some s -> Some <| updateSurfaceInfo model.selectedSurface1.Value.surfaceName (deleteOPCPath model) model.selectedSurface1.Value.maxTriangleSize
                                                                | None -> model.selectedSurface1 }                          
            | UpdateSurface a -> { model with surfacesInfo =
                                                match model.selectedSurface1 with
                                                    | Some s -> model.surfacesInfo |> List.choose (fun e -> if e.surfaceName = s.surfaceName then None else Some e)
                                                    | None -> model.surfacesInfo }
            | SetOPC a -> { model with fromOPCPath =  Text.update env model.fromOPCPath a}
            | AddOPC a -> { model with selectedSurface1 =  match model.selectedSurface1 with
                                                                | Some s -> Some <| updateSurfaceInfo model.selectedSurface1.Value.surfaceName (addOPCPath model) model.selectedSurface1.Value.maxTriangleSize
                                                                | None -> model.selectedSurface1 } 
            | Clear -> model
            | _ -> model


    let view (m : Model) : DomNode<Action> =
        div [clazz "ui container"] [
            div [clazz "ui list"] [
                // surface path
                div [clazz "item"] [label [] [text "Surface Path:"]]
                div [clazz "item"] [Text.view m.currentPath |> Html.map SetPath]
                //div [clazz "ui button"; onMouseClick (fun _ -> Discover OpenDialog.openFolder)] [text "..."]
                div [clazz "ui button"; onMouseClick (fun _ -> OpenFolder)] [text "..."]
                div [clazz "ui button"; onMouseClick (fun _ -> Discover m.currentPath)] [text "discover"]

                // empty surface
                div [clazz "item"] [label [] [text "Empty Surface Name:"]]
                div [clazz "item"] [Text.view m.emptySurfaceName |> Html.map SetEmptySurfaceName]
                div [clazz "ui button"; onMouseClick (fun _ -> AddSurface m.emptySurfaceName)] [text "create"]

                // surface from OPC
                div [clazz "item"] [label [] [text "Surface from OPC Path:"]]
                div [clazz "item"] [Text.view m.opcPath |> Html.map SetOPCPath]
                div [clazz "ui button"; onMouseClick (fun _ -> AddOPCasSurface m.opcPath)] [text "create"]
                    
            ]
            // Surfaces List
            div [Style["overflowY","auto"; "height", "200px"]] [
                form [clazz "ui divided items"] [
                    for s in m.surfacesInfo do
                        let icon =  match m.selectedSurface1 with
                                        | Some(x) -> if s.surfaceName.Equals x.surfaceName then "circular red icon folder" else "circular icon folder"
                                        | None -> "circular icon folder"

                        yield 
                            div[clazz "item"; onMouseClick (fun _ -> SelectSurface s )] [
                                i [clazz icon][]
                                div[clazz "ui content"] [
                                    div[Style ["fontweight", "bold"]; clazz "middle aligned content"][text (sprintf "%s" (System.IO.Path.GetFileName(s.surfaceName)))] 
                                    div[clazz "middle aligned content"][text (sprintf "%s" s.surfaceName)] 
                                ]
                            ]                                    
                ]
            ]       
            div [clazz "ui button"; onMouseClick (fun _ -> if m.selectedSurface1.IsNone then DeleteSurface "" else DeleteSurface m.selectedSurface1.Value.surfaceName)] [text "delete"]

            // Surfaces Details
            div [clazz "item"] [label [] [text "Surface Details:"]]

            div [clazz "item"] [label [] [text "Name:"]]
            div [clazz "item"] [Text.view (if m.selectedSurface1.IsSome then m.selectedSurface1.Value.surfaceName else "" ) |> Html.map ShowSurfaceName]
            //div [clazz "item"] [Text.view (updateSurfaceName m m.surfaceName) |> Html.map ShowSurfaceName]

            div [clazz "item"] [label [] [text "MaxTriangleSize:"]]
            div [clazz "item"] [Text.view (updateTriangleSize m) |> Html.map ShowTriangleSize]

            // OPC list
            div [Style[ "overflowY","auto"; "height", "200px"]] [
                form [clazz "ui divided items"] [
                    let opcPath = match m.selectedSurface1 with
                                                | Some s -> m.selectedSurface1.Value.opcPaths
                                                | None -> [] 
                    for o in opcPath do
                        let icon =  match m.selectedOPC with
                                        | Some(x) -> if o.Equals x then "circular red icon folder" else "circular icon folder"
                                        | None -> "circular icon folder"

                        
                        yield 
                            div[clazz "item"; onMouseClick (fun _ -> SelectOPC o)] [
                                i [clazz icon][]
                                div[clazz "ui content"] [
                                    div[clazz "middle aligned content"][text (sprintf "%s" o)] 
                                ]
                            ]                                    
                ]
            ] 
            
            div [clazz "ui button"; onMouseClick (fun _ -> if m.selectedSurface1.IsNone then DeleteOPC "" else DeleteOPC m.selectedOPC.Value)] [text "delete"]
            //div [clazz "ui button"; onMouseClick (fun _ -> DeleteSurface m.selectedSurface)] [text "update"]    
            
            // Add new OPC
            div [clazz "item"] [label [] [text "Add OPC Path:"]]
            div [clazz "item"] [Text.view m.fromOPCPath |> Html.map SetOPC]
            div [clazz "ui button"; onMouseClick (fun _ -> SearchPath m.fromOPCPath)] [text "..."]
            div [clazz "ui button"; onMouseClick (fun _ -> AddOPC m.fromOPCPath)] [text "add"]  
        
            div [clazz "ui button deny"; onMouseClick (fun _ -> Clear)] [text "Cancel"]
            div [clazz "ui button"; onMouseClick (fun _ -> Import m.surfacesInfo)] [text "Import"]  
        ]
        
    let initial = 
        { currentPath = ""
          discoveredSurfaces = []
          selectedSurface = None
          selectedSurface1 = None
          searchPath = []
          emptySurfaceName = ""
          opcPath = ""
          surfacesInfo = []
          surfaceName = ""
          maxTriangleSize = ""
          selectedOPC = None
          fromOPCPath = ""
          test = ""
        }

    let app f =
        {
            initial = initial
            update = update f
            view = view
            onRendered = OnRendered.ignore
            subscriptions = Subscriptions.none
        }

       

module Program = 
    [<EntryPoint;STAThread>]

    let main argv =
        ChromiumUtilities.unpackCef()
        Chromium.init argv

        use w = new Form()
        let app =  ImportDiscoverAppLaura.app w //PerformanceTest.app //FileApp.app // 

        let m : V3dApp.Model = {
                components = [ Numeric.initial; Numeric.initial; Numeric.initial ]            
                }


//        let app = {
//            initial = { isActive = true }
//            view = Toggle.view
//            update = fun e -> Toggle.update
//            onRendered = OnRendered.ignore
//            subscriptions = Subscriptions.none
//        }
        //let app = Text.app
        let runWindow = true        

        if runWindow then
            let browser = Chromium.runControl "8083" app
            w.Controls.Add browser
            w.Text <- "Add New Surface (OPC)"
            w.Width <- 800
            w.Height <- 600
            Application.Run(w) 
        else
            Fablish.RunLocally app

        0
