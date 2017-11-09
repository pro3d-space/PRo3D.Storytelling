namespace PRo3D.Groups

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.Base.Incremental

open System

open PRo3D
//open PRo3D.Surfaces


module Groups =
    open DomainModel
    open Aardvark.Base.MultimethodTest
    //open Aardvark.UI.Static.Svg
   
    type Action =
        | ActiveGroup           of Guid*list<Index>*string
        | ToggleExpand          of list<Index>
        | AddGroup              of list<Index>
        | RemoveGroup           of list<Index>
        | AddChild              of list<Index>*plist<Child>    
        | RemoveChild           of Guid*list<Index> 
        | ToggleChildVisibility of Guid*list<Index> 
        | ActiveChild           of Guid*list<Index>
        | UpdateChildProperties of Child*list<Index>
        | SetGroupName          of string
        | ClearGroup            of list<Index>
        | SelectChilds          of list<Index>*Guid      
        | MoveChilds      
        | ClearSelection
        | Nop
      
    //let initRoot2 = {
    //    name = "Annotations"
    //    key =  Guid.NewGuid()
    //    childList = plist.Empty
    //    subGroups = plist.Empty 
    //    visible = true
    //    expanded = true
    //    }

    //let initTreeSelection =  { 
    //    id = Guid.Empty
    //    path = list.Empty
    //    name = "" 
    //    }

    let tryGet' (ans:hset<Child>) id = 
            ans |> Seq.tryFind(fun x -> x.id = id)

    let updateAnnotationGroups (model:GroupsModel) (root:Group) (flat:hset<Child>) =        
        { model with rootGroup = root; flat = flat }      
        
    let rec flatten (g:Group) : hset<Child> =
        match ((PList.toList g.childList ), (PList.toList g.subGroups)) with
            | ([], []) -> HSet.empty
            | (c, [] ) -> c |> HSet.ofList
            | ([], sg) -> sg |> HSet.ofList |> HSet.collect flatten 
            | (c, sg)  -> 
                let sga = 
                    sg |> HSet.ofList |> HSet.collect flatten                    
                HSet.union (c |> HSet.ofList) sga

    let loadAnnotationGroups (path:string) =
        let groups = Serialization.loadAs<PRo3D.Groups.GroupsModel> path          
        { groups with flat = (flatten groups.rootGroup) } 

    let tryGetChild (ag:Group) id = 
        flatten ag |> Seq.tryFind(fun x -> x.id = id)

    let updateItemAt (p : list<Index>) (f : Group -> Group) (t : Group) = 
        let rec go (p : list<Index>) (t : Group) =
            match p with
              | [] -> f t
              | x::rest ->
                    match PList.tryGet x t.subGroups with
                      | Some c -> { t with subGroups = PList.set x (go rest c) t.subGroups }                                                  
                      | None   -> t
        go (List.rev p) t 

    let deleteItemAt (p : list<Index>) (f : Group -> Guid -> Group) (t : Group) = 
        let rec go (p : list<Index>) (t : Group)  =
            match p with
              | x::rest -> 
                match rest with
                  | [] -> 
                    match PList.tryGet x t.subGroups with
                      | Some c -> f t c.key
                      | None -> t
                  | _ -> 
                    match PList.tryGet x t.subGroups with
                      | Some c -> { t with subGroups = PList.set x (go rest c) t.subGroups }                                 
                      | None   -> t
              | _ -> t
        go (List.rev p) t 
        
    let removeGroup (ag:Group) (id:Guid) = 
        ag.subGroups |> PList.filter (fun x -> x.key <> id) 
    
    let removeChild (childs:plist<Child>) (id:Guid) = 
        childs |> PList.filter (fun x -> x.id <> id)    

    let toggleChild (childs:plist<Child>) (id:Guid) =        
        childs |> PList.map(fun x -> if x.id = id then Child.toggleVisibility x else x)        
    
    let updateChild (childs:plist<Child>) (c:Child) = 
        childs |> PList.map(fun x -> if x.id = c.id then c else x)

    let addToActiveGroup model (a:Child) = 
        let func = (fun (x : Group) -> { x with childList = x.childList |> PList.prepend a })
        updateItemAt model.activeGroup.path func model.rootGroup

    let rec removeSelectionfromTree (selection:list<TreeSelection> ) (model:GroupsModel) =
        match selection with
          | x::rest ->
                let func = (fun (y:Group) -> { y with childList = if PList.count y.childList > 0 then removeChild y.childList x.id else y.childList })
                let newRoot = updateItemAt x.path func model.rootGroup 
                let m' = updateAnnotationGroups model newRoot (flatten newRoot)
                removeSelectionfromTree rest m'
          |_ -> model

    let removeFromSelection model id path =
        { model with selectedChilds = model.selectedChilds |> HSet.remove( { id = id; path = path; name = "" } ) }

    let moveChildren model =
        // get annotations
        let toMove = model.selectedChilds
                        |> HSet.choose ( fun x -> (tryGetChild model.rootGroup x.id))
                        |> HSet.toSeq
                        |> PList.ofSeq
                               
        // delete from groups
        let m = removeSelectionfromTree (model.selectedChilds |> HSet.toList) model
                
        // add annotations to active group
        let func1 = (fun (x:Group) -> { x with childList = x.childList |> PList.append' toMove })
        let newRoot1 = updateItemAt m.activeGroup.path func1 m.rootGroup
        let m' = updateAnnotationGroups m newRoot1 (flatten newRoot1)

        // update selection paths
        let sel = (model.selectedChilds |> HSet.map( fun x -> {x with path = m.activeGroup.path }))
        { m' with selectedChilds = sel}
        
    let update (model : GroupsModel) (action : Action) =
        printfn "action: %A" action
        match action with
            | ActiveGroup (g, p, s) -> 
                let selection = {
                    id = g
                    path = p
                    name = s}
                { model with 
                    activeGroup   = selection
                    lastSelectedItem = SelectedItem.Group } 
            | ActiveChild (g, p) -> 
                { model with
                    activeChild = { id = g; path = p; name = "" }
                    lastSelectedItem = SelectedItem.Child } 
            | ToggleExpand p -> 
                let func = (fun (x:Group) -> { x with expanded = not x.expanded })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | AddGroup p -> 
                let newGroup = 
                    {    
                        name = "newGroup" 
                        key =  Guid.NewGuid()
                        childList = plist.Empty
                        subGroups = plist.Empty
                        visible = true
                        expanded = true
                    }

                let func = 
                    (fun (x:Group) -> { x with subGroups = PList.append newGroup x.subGroups })

                { model with rootGroup = updateItemAt p func model.rootGroup }
            | RemoveGroup p -> 
                let func = 
                    fun (x:Group) (id:Guid) -> 
                        { x with subGroups = if PList.count x.subGroups > 0 then removeGroup x id else x.subGroups }

                let newRoot = deleteItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flatten newRoot)
                m'
            | AddChild (p,c) -> 
                let func = (fun (x:Group) -> { x with childList = c })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flatten newRoot)
                m'
            | RemoveChild (k,p) -> 
                let func = (fun (x:Group) -> { x with childList = if PList.count x.childList > 0 then removeChild x.childList k else x.childList })
                let newRoot = updateItemAt p func model.rootGroup                
                let m = removeFromSelection model k p
                
                updateAnnotationGroups m newRoot (flatten newRoot)
                
            | ToggleChildVisibility (c,p) -> 
                let func = (fun (x:Group) -> { x with childList = if PList.count x.childList > 0 then toggleChild x.childList c else x.childList })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flatten newRoot)
                m'
            | UpdateChildProperties (a,p) ->
                let func = (fun (x:Group) -> { x with childList = if PList.count x.childList > 0 then updateChild x.childList a else x.childList })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flatten newRoot)
                m'
            | SetGroupName t -> 
                let func = (fun (x:Group) -> { x with name = t })
              
                { model with rootGroup = updateItemAt model.activeGroup.path func model.rootGroup; activeGroup = {model.activeGroup with name = t} }
            | ClearGroup p -> 
                let func = (function (x:Group) -> { x with childList = plist.Empty })
                let newRoot = updateItemAt p func model.rootGroup
                let selection = model.selectedChilds |> HSet.choose (fun x -> match x.path = p with
                                                                                | true -> Some x
                                                                                | _ -> None )
                let m = { model with selectedChilds = selection }
                let m' = updateAnnotationGroups m newRoot (flatten newRoot)
                m'
            | SelectChilds (p,id) ->
                let selected = model.selectedChilds
                let tSelection = {
                    id = id
                    path = p
                    name = ""
                    }
               
                let selection, child =         
                    if (not model.selectedChilds.IsEmpty) then
                        if HSet.contains tSelection selected
                        then 
                            HSet.remove tSelection selected, None
                        else 
                            HSet.add tSelection selected, Group.tryGet' model.flat id  
                    else
                        HSet.add tSelection hset.Empty, Group.tryGet' model.flat id   

                { model with 
                    selectedChilds    = selection; 
                    activeChild       = tSelection; 
                    lastSelectedItem  = SelectedItem.Child;
                    lastSelectedChild = child }
            | MoveChilds  -> 
                let m = moveChildren model
                m
            | ClearSelection ->
                { model with selectedChilds = hset.Empty }
            | _ -> model
    

    let mkColor (activeId : Guid) (groupId : Guid) = 
        if activeId = groupId then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White
             
    //let rec viewTree path (group : MGroup) (model : MGroupsModel) =

    //    alist {
    //        let! s = model.activeGroup
    //        let! c = mkColor s.id (group.key |> Mod.force)
    //        let color = sprintf "color: %s" (Html.ofC4b c)
    //        let c = sprintf "color: %s" (Html.ofC4b C4b.White)
    //        let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subGroups
    //        let desc =
    //            div [style color] [
    //                    Incremental.text group.name
    //                    onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
    //                        i [ clazz "circle thin icon" 
    //                            attribute "title" "Set active"
    //                            attribute "data-position" "left center"
    //                            attribute "data-variation" "mini"
    //                            onClick (fun _ -> ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force))] [] 
    //                        )
    //                    onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
    //                        i [ clazz "plus icon" 
    //                            attribute "title" "Add Group"
    //                            attribute "data-position" "left center"
    //                            attribute "data-variation" "mini"
    //                            onMouseClick (fun _ -> AddGroup path)] [] 
    //                        )

    //                    i [ clazz "remove icon"; onMouseClick (fun _ -> RemoveGroup path)] [] |> UI.wrapToolTip "Remove Group"
    //            ]
                        
     
    //        let itemAttributes =
    //            amap {
    //                yield onMouseClick (fun _ -> ToggleExpand path)
    //                let! selected = group.expanded
    //                if selected then 
    //                    yield clazz "icon large outline open folder"
    //                else
    //                    yield clazz "icon large outline folder"                    
    //            } |> AttributeMap.ofAMap

            
    //        let childrenAttribs =
    //            amap {
    //                yield clazz "list"
    //                let! isExpanded = group.expanded
    //                if isExpanded then yield style "visible"
    //                else yield style "hidden"
    //            }

    //        yield div [ clazz "item" ] [
    //             Incremental.i itemAttributes AList.empty
    //             div [ clazz "content" ] [
    //                 //div [ clazz "header"] [desc]
    //                 div [ clazz "description noselect"] [desc]
    //                 Incremental.div (AttributeMap.ofAMap childrenAttribs) 
    //                    <| alist { 
    //                        let! isExpanded = group.expanded
    //                        if isExpanded then yield! children
    //                    }
    //             ]
    //        ]
                
    //    }

            

    let viewUI (model : MGroupsModel) =   
        let ts = model.activeGroup
        require Html.semui (
            Html.table [                            
                Html.row "Change Groupname:"[Html.SemUi.textBox (ts |> Mod.map (fun x -> x.name)) SetGroupName ]
            ]
        )

    //let view (model : MGroupsModel) =
    //    require Html.semui (
    //        TreeView.view [] (viewTree [] model.rootGroup model)
    //    )


