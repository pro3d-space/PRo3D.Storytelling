namespace PRo3D

module Bookmarks = 
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.Base.Incremental

    open System

    open PRo3D.BookmarksX
    open PRo3D.Navigation2
    open Aardvark.UI.Primitives.Mutable
    open Aardvark.SceneGraph.SgFSharp.Sg
    open PRo3D.Groups
    open Aardvark.Base.MultimethodTest

    type Action =
        | AddBookmark 
        | GroupsMessage   of Groups.Action
    
    let tryGet (ans:plist<Bookmark>) key = 
            ans |> Seq.tryFind(fun x -> x.key = key)

    let getNewBookmark (navigation:NavigationModel) (count:int) =  
        let name = sprintf "bm%d" count
        {
            key = Guid.NewGuid()
            name = name
            navigationState = navigation
        }
        
    
    let update (model : BookmarksModel) (act : Action) (navigation : Lens<'a,NavigationModel>) (outerModel:'a) : ('a*BookmarksModel) = 
        match act with
            | AddBookmark -> 
                let newBm = 
                    getNewBookmark (navigation.Get outerModel) model.bookmarks.flat.Count
                
                let groups = 
                    Groups.addLeafToActiveGroup (Leaf.Bookmarks newBm) true model.bookmarks
                
                outerModel, { model with bookmarks = groups}
            | GroupsMessage msg -> 
                match msg with
                    | Groups.Action.UpdateCam id -> 
                        let bm = model.bookmarks.flat |> HMap.tryFind id
                        match bm with
                            | Some b ->
                                match b with
                                    | Leaf.Bookmarks bkm ->

                                        let navigation' = bkm.navigationState 
                                        let newOuterModel = navigation.Set(outerModel,navigation')
                                        (newOuterModel,model)
                                    | _ -> (outerModel,model) 
                            | None -> (outerModel,model) 
                     | _ -> (outerModel, { model with bookmarks = Groups.update model.bookmarks msg})
                


    let mkColor (model : MGroupsModel) (b : MBookmark) =
            let id = b.key |> Mod.force

            let color =  
                model.selectedLeaves
                    |> ASet.map(fun x -> x.id = id)
                    |> ASet.contains true
                    |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White)
            
            color

    let lastSelected (model : MGroupsModel) (b : MBookmark) =
            let id = b.key |> Mod.force
            model.singleSelectLeaf 
                |> Mod.map( fun x -> match x with 
                                        | Some xx -> xx = id
                                        | _ -> false )

    let viewBookmarks (path:list<Index>) (model:MGroupsModel)(select : MBookmark*list<Index> -> 'outer)(lift : Groups.Action -> 'outer) (bookmarks: alist<Guid>) : alist<DomNode<'outer>> = 
                     
        alist {

            let bookmarks = 
                bookmarks
                  |> AList.map (fun x -> model.flat |> AMap.find x |> Mod.bind(id) |> Mod.force)
                  |> AList.choose( fun x -> 
                     match x with
                       | MBookmarks a -> Some a
                       | _-> None )

            for b in bookmarks do
                                
                let! c = mkColor model b
                let bgc = sprintf "color: %s" (Html.ofC4b c)
                let infoc = sprintf "color: %s" (Html.ofC4b C4b.White)

                let select = fun _ -> select (b,path)
                let! lastSel = lastSelected model b
                let! key = b.key

                yield div [clazz "item"; style bgc] [
                    i [clazz "large Sticky Note middle aligned icon"; onClick select;style bgc][] //i [clazz "large Sticky Note middle aligned icon";onClick select; style bgc][]
                    div [clazz "content"; style infoc] [
                        let desc = b.name
                        yield Incremental.div (AttributeMap.ofList [clazz "header"; style infoc])(
                            alist {
                                    yield i [clazz "home icon"; 
                                             onClick (fun _ -> lift <| Groups.UpdateCam key)][] |> UI.wrapToolTipRight "FlyTo"
                    
                                    yield i [clazz "Remove icon"; 
                                             onClick (fun _ -> lift <| Groups.RemoveChild (key,path))] [] |> UI.wrapToolTip "Remove"
                                } 
                        ) 
                        if lastSel then
                            yield div[onClick select][ u[][Incremental.text b.name]]
                        else
                            yield div[onClick select][Incremental.text b.name]
                        //yield div [clazz "description"; style bgc] [Incremental.text desc]      
                    ]
                ]
        }     
               

    let rec viewTree path (group : MNode) (model : MGroupsModel) : alist<DomNode<Action>> =

        alist {

            let! active = model.activeGroup
            let color = sprintf "color: %s" (Html.ofC4b C4b.White)                
            

            let icon =
                adaptive {                    
                    let! group  =  group.key
                    return if (active.id = group) then "circle icon" else "circle thin icon"
                }
                      
            let map = 
                Groups.mkGroupAttributeMap icon (GroupsMessage(Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)))
               
            let desc =
                div [style color] [       
                    Incremental.text group.name
                    Incremental.i map AList.empty |> UI.wrapToolTip "Set active"
                        
                    i [clazz "plus icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.AddGroup path))] [] 
                    |> UI.wrapToolTip "Add Group"   
                        
                    //i [clazz "remove circle icon"
                    //   onMouseClick (fun _ -> 
                    //     GroupsMessage(Groups.ClearGroup path))] [] 
                    //|> UI.wrapToolTip "Clear Group"
                        
                    //i [clazz "remove icon red"
                    //   onMouseClick (fun _ -> Action.GroupsMessage(Groups.RemoveGroup path))] [] 
                    //|> UI.wrapToolTip "Remove Group"                    
                ]
                 
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                    let! selected = group.expanded
                    if selected 
                    then yield clazz "icon large outline open folder"
                    else yield clazz "icon large outline folder"
                    yield style "overflow-y : visible"
                } |> AttributeMap.ofAMap
            
            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = group.expanded
                    if isExpanded then yield style "visible"
                    else yield style "hidden"
                }         

            let leafDomNodes = AList.collecti (fun i v -> viewTree (i::path) v model) group.subNodes    

            let select = 
                fun (a:MBookmark,path:list<Index>) -> 
                    Action.GroupsMessage(Groups.AddLeafToSelection (path, a.key |> Mod.force, a.name |> Mod.force))

            let lift   = fun (a:Groups.Action) -> (GroupsMessage a)

            yield div [ clazz "item"] [
                Incremental.i itemAttributes AList.empty
                div [ clazz "content" ] [                         
                    div [ clazz "description noselect"] [desc]
                    Incremental.div (AttributeMap.ofAMap childrenAttribs) (                          
                        alist { 
                            let! isExpanded = group.expanded
                            if isExpanded then 
                                yield! leafDomNodes

                            if isExpanded then 
                                yield! viewBookmarks path model select lift group.leaves
                        }
                    )  
                            
                ]
            ]
                
        }


    let viewBookmarksGroups (model:MBookmarksModel) = 
        require Html.semui (
            TreeView.view [] (viewTree [] model.bookmarks.rootGroup model.bookmarks)
        )                                    
 
    module UI =
        let view (model:MBookmark) =
            let view = model.navigationState.camera.view
            require Html.semui (
                Html.table [  
                    Html.row "Change Name:"[Html.SemUi.textBox model.name Groups.SetChildName ]
                    Html.row "Pos:"     [Incremental.text (view |> Mod.map (fun x -> x.Location.ToString("0.00")))] 
                    Html.row "LookAt:"  [Incremental.text (view |> Mod.map (fun x -> x.Forward.ToString("0.00")))]
                    Html.row "Up:"      [Incremental.text (view |> Mod.map (fun x -> x.Up.ToString("0.00")))]
                    Html.row "Sky:"     [Incremental.text (view |> Mod.map (fun x -> x.Sky.ToString("0.00")))]
                ]
            )

        let viewGUI = 
         div [clazz "ui buttons inverted"] [
                    //onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                        button [clazz "ui icon button"; onMouseClick (fun _ -> AddBookmark )] [ //
                                i [clazz "plus icon"] [] ] |> UI.wrapToolTipBottom "Add Bookmark"
                   // )
                ] 

       