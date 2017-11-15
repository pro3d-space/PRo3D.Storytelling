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
        //| UpdateCam of Guid  
        //| Select of Guid
        //| Remove of Guid
        //| SetBookmarkName          of string
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
                //(outerModel,model) // in Groups
                let newBm = getNewBookmark (navigation.Get outerModel) model.bookmarks.flat.Count
                let root = Groups.addToActiveGroup model.bookmarks (Child.Bookmarks newBm)
                let groups = 
                    Groups.updateGroups model.bookmarks root (Groups.flatten root)
                (outerModel,{ model with bookmarks = groups})
                
            //| UpdateCam id -> 
            //    let bm = tryGet model.bookmarks id
            //    match bm with
            //        | Some b ->
            //            let navigation' = b.navigationState 
            //            let newOuterModel = navigation.Set(outerModel,navigation')
            //            (newOuterModel,model)                      
            //        | None -> (outerModel,model)
            //| Select id ->
            //    let sb = tryGet model.bookmarks id   
            //    (outerModel,{ model with selectedBookmark = sb })
            //| Remove id -> (outerModel,model) // in Groups
                //let toRemove = model.bookmarks |> PList.toList |> List.tryFindIndex (fun d -> d.key = id)
                //match toRemove with
                //    | Some index ->
                //        let b' = model.bookmarks |> PList.removeAt index
                //        (outerModel, { model with bookmarks = b'})
                //    | None -> (outerModel, model) 
            //| SetBookmarkName n -> 
            //    match model.bookmarks.activeChild with //selectedBookmark with
            //            | Some x -> let b' = { x with name = n }
            //                        let bookmarks' = 
            //                            model.bookmarks.AsList |> List.updateIf (fun x -> x.key = b'.key) (fun _ -> b')   
            //                        (outerModel, {model with bookmarks = bookmarks' |> PList.ofList })
            //            | None -> (outerModel,model)
            | GroupsMessage msg -> 
                match msg with
                    | Groups.Action.UpdateCam id -> 
                        let bm = Groups.tryGetChild model.bookmarks.rootGroup id
                        match bm with
                            | Some b ->
                                match b with
                                    | Child.Bookmarks bkm ->
                                        let navigation' = bkm.navigationState 
                                        let newOuterModel = navigation.Set(outerModel,navigation')
                                        (newOuterModel,model)
                                    | _ -> (outerModel,model) 
                            | None -> (outerModel,model) 
                     | _ -> (outerModel, { model with bookmarks = Groups.update model.bookmarks msg})
                


    let mkColor (model : MGroupsModel) (b : MBookmark) =
            let id = b.key |> Mod.force

            let color =  
                model.selectedChilds
                    |> ASet.map(fun x -> x.id = id)
                    |> ASet.contains true
                    |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White)
            
            color


    let viewGUI = 
        
         div [clazz "ui buttons inverted"] [
                    onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                        button [clazz "ui icon button"; attribute "data-content" "Add Bookmark"; onMouseClick (fun _ -> AddBookmark )] [ //
                                i [clazz "plus icon"] [] ] 
                    )
                ] 

    let viewBookmarks (path:list<Index>) (model:MGroupsModel)(select : MBookmark*list<Index> -> 'outer)(lift : Groups.Action -> 'outer) (bookmarks: alist<IMod<MChild>>) : alist<DomNode<'outer>> = 
                     
                        alist {

                            let color = sprintf "color: %s" (Html.ofC4b C4b.White)      
                            let bookmarks = bookmarks
                                                |> AList.map (fun x -> Mod.force x)
                                                |> AList.choose( fun x -> match x with
                                                                            | MBookmarks a -> Some a
                                                                            | _-> None )
                            for b in bookmarks do
                                
                                let! c = mkColor model b
                                let bgc = sprintf "color: %s" (Html.ofC4b c)

                                let select = fun _ -> select (b,path)
                                let! key = b.key


                                yield div [clazz "item"; onClick select; style bgc] [
                                        i [clazz "large Sticky Note middle aligned icon";style bgc][] //i [clazz "large Sticky Note middle aligned icon";onClick select; style bgc][]
                                        div [clazz "content"; style bgc] [
                                            let desc = b.name
                                            yield Incremental.div (AttributeMap.ofList [clazz "header"])(
                                                alist {
                                                    yield i [clazz "home icon"                                                
                                                             onClick (fun _ -> lift <| Groups.UpdateCam key)
                                                             style bgc][] |> UI.wrapToolTipRight "FlyTo"               
                                                    yield i [clazz "Remove icon"
                                                             onClick (fun _ -> lift <| Groups.RemoveChild (key,path)); style color] [] |> UI.wrapToolTip "Remove"      
                                                    } 
                                            ) 
                                            yield div [clazz "description"; style bgc] [Incremental.text desc]      
                                        ]
                                    ]
                               
                        }     
               

    let getAmap icon action =
             amap {
                    let! icon = icon
                    yield clazz icon
                    yield onClick (fun _ -> action)
                } |> AttributeMap.ofAMap

    let rec viewTree path (group : MGroup) (model : MGroupsModel) : alist<DomNode<Action>> =

        alist {

            let! s = model.activeGroup
            let color = sprintf "color: %s" (Html.ofC4b C4b.White)                
            let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subGroups    

            let icon =
                adaptive {
                    let active = s.id
                    let! group  =  group.key

                    return if (active = group) then "circle icon" else "circle thin icon"
                }
                      
            let map = getAmap icon (GroupsMessage(Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)))
                   
                
            //let toggleIcon =
            //    adaptive {
            //        let! toggle = group.visible
            //        return if toggle then "unhide icon" else "hide icon"
            //    }

            //let toggleMap = getAmap toggleIcon (GroupsMessage(Groups.ToggleGroup path))
               
            let desc =
                div [style color] [       
                    Incremental.text group.name
                    Incremental.i map AList.empty |> UI.wrapToolTip "Set active"
                        
                    i [clazz "plus icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.AddGroup path))] [] 
                    |> UI.wrapToolTip "Add Group"   
                        
                    i [clazz "remove circle icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.ClearGroup path))] [] 
                    |> UI.wrapToolTip "Clear Group"
                        
                    i [clazz "remove icon red"
                       onMouseClick (fun _ -> Action.GroupsMessage(Groups.RemoveGroup path))] [] 
                    |> UI.wrapToolTip "Remove Group"

                    //Incremental.i toggleMap AList.empty |> UI.wrapToolTip "Toggle Group"
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


            let select = fun (a:MBookmark,path:list<Index>) -> Action.GroupsMessage(Groups.SelectChilds (path, a.key |> Mod.force))  // Select (path, a.key |> Mod.force)
            let lift   = fun (a:Groups.Action) -> (GroupsMessage a)

            yield div [ clazz "item"] [
                    Incremental.i itemAttributes AList.empty
                    div [ clazz "content" ] [                         
                        div [ clazz "description noselect"] [desc]
                        Incremental.div (AttributeMap.ofAMap childrenAttribs) (                          
                            alist { 
                                let! isExpanded = group.expanded
                                if isExpanded then yield! children

                                if isExpanded then 
                                    yield! viewBookmarks path model select lift group.childList
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

       