namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.Base.Incremental

open System

open PRo3D.Surfaces
open PRo3D


module AnnotationGroups =
   
    type Action =
        | ActiveGroup           of Guid*list<Index>*string
        | ToggleExpand          of list<Index>
        | AddGroup              of list<Index>
        | RemoveGroup           of list<Index>
        | AddChild              of list<Index>*plist<Child>    // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | RemoveChild           of Guid*list<Index> // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | ToggleChildVisibility of Guid*list<Index> // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | ActiveChild           of Guid*list<Index>
        | UpdateChildProperties of Child*list<Index>
        | SetGroupName          of string
        | Nop

    //
         
    let updateAnnotationGroups (model:AnnotationGroupsModel) (root:AnnotationGroup) (flat:plist<Child>) = //(flat:plist<Annotation>) =
        let flatAnnotations = flat 
                                |> PList.choose (fun x -> match x with
                                                            | Child.Annotations a -> Some a
                                                            | _-> None)
        {model with rootGroup = root; annotationsFlat = flatAnnotations}
        
    let rec flattenChilds (g:AnnotationGroup) =
        match ((PList.toList g.childList ), (PList.toList g.subGroups)) with
            | ([], []) -> []
            | (c, [] ) -> c
            | ([], sg) -> sg |> List.collect flattenChilds
            | (c, sg)  -> let sga = sg |> List.collect flattenChilds
                          c@sga

    let loadAnnotationGroups (path:string) =
        let annotationGroups = Serialization.loadAs<PRo3D.AnnotationGroupsModel> path
        let flatA = flattenChilds annotationGroups.rootGroup 
                        |> Seq.ofList 
                        |> Seq.choose (fun x -> match x with
                                                | Child.Annotations a -> Some a
                                                | _-> None)
                        |> PList.ofSeq
        { annotationGroups with annotationsFlat = flatA } 

    let tryGetChild (ag:AnnotationGroup) key = 
        let childs = flattenChilds ag 
        childs |> Seq.tryFind(fun x -> match x with 
                                        | Child.Surfaces s -> s.guid = key
                                        | Child.Annotations a -> a.key = key
                                        )

    let updateItemAt (p : list<Index>) (f : AnnotationGroup -> AnnotationGroup) (t : AnnotationGroup) = 
        let rec go (p : list<Index>) (t : AnnotationGroup) =
            match p with
              | [] -> f t
              | x::rest ->
                    match PList.tryGet x t.subGroups with
                      | Some c -> { t with subGroups = PList.set x (go rest c) t.subGroups }                                                  
                      | None   -> t
        go (List.rev p) t 

    let deleteItemAt (p : list<Index>) (f : AnnotationGroup -> Guid -> AnnotationGroup) (t : AnnotationGroup) = 
        let rec go (p : list<Index>) (t : AnnotationGroup)  =
            match p with
                | x::rest -> match rest with
                                | [] -> match PList.tryGet x t.subGroups with
                                            | Some c -> f t c.key
                                            | None -> t
                                | _ -> match PList.tryGet x t.subGroups with
                                            | Some c -> let ag = {
                                                            name = t.name
                                                            key = t.key
                                                            childList = t.childList
                                                            subGroups = PList.set x (go rest c) t.subGroups
                                                            visible = t.visible
                                                            expanded = t.expanded
                                                            }
                                                        ag
                                            | None   -> t
                |_ -> t
        go (List.rev p) t 
        
    let removeGroup (ag:AnnotationGroup) (id:Guid) = ag.subGroups |> PList.filter (fun x -> x.key <> id) 
    //let removeAnnotation (ans:plist<Annotation>) (id:Guid) = ans |> PList.filter (fun x -> x.key <> id) 
    let removeChild (childs:plist<Child>) (id:Guid) = childs |> PList.filter (fun x -> match x with
                                                                                        | Child.Annotations a -> a.key <> id
                                                                                        | Child.Surfaces s -> s.guid <> id) 
    let toggleAnnotation (ans:plist<Annotation>) (id:Guid) = 
        ans |> PList.map( fun x -> if x.key = id then { x with visible = not x.visible }
                                                else x )
            |> PList.map( fun x -> Child.Annotations x)


    let toggleSurface (surfs:plist<Surface>) (id:Guid) = 
        surfs |> PList.map( fun x -> if x.guid = id then { x with isVisible = not x.isVisible }
                                                else x )
              |> PList.map( fun x -> Child.Surfaces x)

    let toggleChild (childs:plist<Child>) (id:Guid) =
        let c = childs.[0]
        match c with
            | Child.Annotations a -> toggleAnnotation (childs |> PList.choose( fun a -> match a with 
                                                                                        | Child.Annotations x -> Some x  
                                                                                        | _ -> None)) id
            | Child.Surfaces s -> toggleSurface (childs |> PList.choose( fun s -> match s with 
                                                                                    | Child.Surfaces x -> Some x
                                                                                    | _-> None)) id


    let updateAnnotation (ans:plist<Annotation>) (a:Annotation) = ans.AsList |> List.updateIf (fun x -> x.key = a.key) (fun _ -> a) |> PList.ofList
    let updateSurface (surfs:plist<Surface>) (s:Surface) = surfs.AsList |> List.updateIf (fun x -> x.guid = s.guid) (fun _ -> s) |> PList.ofList
    let updateChild (childs:plist<Child>) (c:Child) = 
        match c with
            | Child.Annotations a -> updateAnnotation (childs |> PList.choose( fun x -> match x with
                                                                                        | Child.Annotations xa -> Some xa
                                                                                        | _-> None )) a
                                        |> PList.map( fun x -> Child.Annotations x)
            | Child.Surfaces s -> updateSurface (childs |> PList.choose( fun x -> match x with
                                                                                    | Child.Surfaces xs -> Some xs
                                                                                    | _-> None )) s
                                    |> PList.map( fun x -> Child.Surfaces x)
        
    let update (model : AnnotationGroupsModel) (action : Action) =
        printfn "action: %A" action
        match action with
            | ActiveGroup (g, p, s) -> { model with activeGroupKey = g; activeGroupPath = p; activeGroupName = s; lastSelectedItem = SelectedItem.AnnotationGroup } 
            | ActiveChild (g, p) -> { model with activeChildKey = g; activeChildPath = p; lastSelectedItem = SelectedItem.Annotation} 
            | ToggleExpand p -> 
                let func = (function (x:AnnotationGroup) -> { x with expanded = not x.expanded })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | AddGroup p -> 
                let newGroup = {    name = "newGroup" // TODO: get group name from user
                                    key =  Guid.NewGuid()
                                    childList = plist.Empty
                                    subGroups = plist.Empty
                                    visible = true
                                    expanded = true
                                    }
                let func = (function (x:AnnotationGroup) -> { x with subGroups = PList.append newGroup x.subGroups })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | RemoveGroup p -> 
                let func = (fun (x:AnnotationGroup) (id:Guid) -> { x with subGroups = if PList.count x.subGroups > 0 then removeGroup x id else x.subGroups })
                let newRoot = deleteItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flattenChilds newRoot |> PList.ofList)
                m'
            | AddChild (p,c) -> 
                let func = (function (x:AnnotationGroup) -> { x with childList = c })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flattenChilds newRoot |> PList.ofList)
                m'
            | RemoveChild (k,p) -> 
                let func = (function (x:AnnotationGroup) -> { x with childList = if PList.count x.childList > 0 then removeChild x.childList k else x.childList })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flattenChilds newRoot |> PList.ofList)
                m'
            | ToggleChildVisibility (c,p) -> 
                let func = (function (x:AnnotationGroup) -> { x with childList = if PList.count x.childList > 0 then toggleChild x.childList c else x.childList })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flattenChilds newRoot |> PList.ofList)
                m'
            | UpdateChildProperties (a,p) ->
                let func = (fun (x:AnnotationGroup) -> { x with childList = if PList.count x.childList > 0 then updateChild x.childList a else x.childList })
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateAnnotationGroups model newRoot (flattenChilds newRoot |> PList.ofList)
                m'
            | SetGroupName t -> 
                let func = (fun (x:AnnotationGroup) -> { x with name = t })
                { model with rootGroup = updateItemAt model.activeGroupPath func model.rootGroup; activeGroupName = t }
            | _ -> model
    

    let mkColor (activeId : IMod<Guid>) (groupId : IMod<Guid>) = 
        if activeId = groupId then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White
             
    let rec viewTree path (group : MAnnotationGroup) (model : MAnnotationGroupsModel) =

        alist {

            let! c = mkColor model.activeGroupKey group.key
            let color = sprintf "color: %s" (Html.ofC4b c)
            let c = sprintf "color: %s" (Html.ofC4b C4b.White)
            let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subGroups
            let desc =
                div [style color] [
                        Incremental.text group.name
                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                            i [ clazz "circle thin icon" 
                                attribute "title" "Set active"
                                attribute "data-position" "left center"
                                attribute "data-variation" "mini"
                                onClick (fun _ -> ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force))] [] 
                            )
                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                            i [ clazz "plus icon" 
                                attribute "title" "Add Group"
                                attribute "data-position" "left center"
                                attribute "data-variation" "mini"
                                onMouseClick (fun _ -> AddGroup path)] [] 
                            )
                        //onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                        //    i [ clazz "remove icon" 
                        //        attribute "title" "Remove Group"
                        //        attribute "data-position" "left center"
                        //        attribute "data-variation" "mini" 
                        //        onMouseClick (fun _ -> RemoveGroup path)] [] 
                        //    )

                        i [ clazz "remove icon"; onMouseClick (fun _ -> RemoveGroup path)] [] |> UI.wrapToolTip "Remove Group"
                ]
                        
     
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> ToggleExpand path)
                    let! selected = group.expanded
                    if selected then 
                        yield clazz "icon large outline open folder"
                    else
                        yield clazz "icon large outline folder"                    
                } |> AttributeMap.ofAMap

            
            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = group.expanded
                    if isExpanded then yield style "visible"
                    else yield style "hidden"
                }

            yield div [ clazz "item" ] [
                 Incremental.i itemAttributes AList.empty
                 div [ clazz "content" ] [
                     //div [ clazz "header"] [desc]
                     div [ clazz "description noselect"] [desc]
                     Incremental.div (AttributeMap.ofAMap childrenAttribs) 
                        <| alist { 
                            let! isExpanded = group.expanded
                            if isExpanded then yield! children
                        }
                 ]
            ]
                
        }

    let viewUI (model : MAnnotationGroupsModel) =   
        require Html.semui (
            Html.table [                            
                Html.row "Change Groupname:"[Html.SemUi.textBox model.activeGroupName SetGroupName ]
            ]
        )

    let view (model : MAnnotationGroupsModel) =
        require Html.semui (
            TreeView.view [] (viewTree [] model.rootGroup model)
        )


