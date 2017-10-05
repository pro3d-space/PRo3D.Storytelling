namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.Base.Incremental

open System

open PRo3D


module AnnotationGroups =
   
    type Action =
        | ActiveGroup           of Guid*list<Index>*string
        | ToggleExpand          of list<Index>
        | AddGroup              of list<Index>
        | RemoveGroup           of list<Index>
        | AddChild              of list<Index>*plist<Annotation>    // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | RemoveChild           of Guid*list<Index> // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | ToggleChildVisibility of Guid*list<Index> // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | ActiveChild           of Guid*list<Index>
        | UpdateChildProperties of Annotation*list<Index>
        | SetGroupName          of string
        | Nop

    //
    let rec flattenAnnotations (g:AnnotationGroup) =
        match ((PList.toList g.annotations), (PList.toList g.subGroups)) with
            | ([], []) -> []
            | (c, [] ) -> c
            | ([], sg) -> sg |> List.collect flattenAnnotations
            | (c, sg)  -> let sga = sg |> List.collect flattenAnnotations
                          c@sga

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
                                                            annotations = t.annotations
                                                            subGroups = PList.set x (go rest c) t.subGroups
                                                            visible = t.visible
                                                            expanded = t.expanded
                                                            }
                                                        ag
                                            | None   -> t
                |_ -> t
        go (List.rev p) t 
        
    let removeGroup (ag:AnnotationGroup) (id:Guid) = ag.subGroups |> PList.filter (fun x -> x.key <> id) 
    let removeAnnotation (ans:plist<Annotation>) (id:Guid) = ans |> PList.filter (fun x -> x.key <> id) 
    let toggleAnnotation (ans:plist<Annotation>) (id:Guid) = 
        ans |> PList.map( fun x -> if x.key = id then { x with visible = not x.visible }
                                                else x )
    let updateChild (ans:plist<Annotation>) (a:Annotation) = ans.AsList |> List.updateIf (fun x -> x.key = a.key) (fun _ -> a) |> PList.ofList
        
    let update (model : AnnotationGroupsModel) (action : Action) =
        printfn "action: %A" action
        match action with
            | ActiveGroup (g, p, s) -> { model with activeGroupKey = g; activeGroupPath = p; activeGroupName = s } 
            | ActiveChild (g, p) -> { model with activeChildKey = g; activeChildPath = p } 
            | ToggleExpand p -> 
                let func = (function (x:AnnotationGroup) -> { x with expanded = not x.expanded })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | AddGroup p -> 
                let newGroup = {    name = "newGroup" // TODO: get group name from user
                                    key =  Guid.NewGuid()
                                    annotations = plist.Empty
                                    subGroups = plist.Empty
                                    visible = true
                                    expanded = true
                                    }
                let func = (function (x:AnnotationGroup) -> { x with subGroups = PList.append newGroup x.subGroups })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | RemoveGroup p -> 
                let func = (fun (x:AnnotationGroup) (id:Guid) -> { x with subGroups = if PList.count x.subGroups > 0 then removeGroup x id else x.subGroups })
                let newRoot = deleteItemAt p func model.rootGroup
                let flatAnnos = flattenAnnotations newRoot |> PList.ofList
                { model with rootGroup = newRoot; flattenChild = flatAnnos}
            | AddChild (p,c) -> 
                let func = (function (x:AnnotationGroup) -> { x with annotations = c })
                { model with rootGroup = updateItemAt p func model.rootGroup; flattenChild = c }
            | RemoveChild (k,p) -> 
                let func = (function (x:AnnotationGroup) -> { x with annotations = if PList.count x.annotations > 0 then removeAnnotation x.annotations k else x.annotations })
                let flatAnnos = removeAnnotation model.flattenChild k
                { model with rootGroup = updateItemAt p func model.rootGroup; flattenChild = flatAnnos }
            | ToggleChildVisibility (c,p) -> 
                let func = (function (x:AnnotationGroup) -> { x with annotations = if PList.count x.annotations > 0 then toggleAnnotation x.annotations c else x.annotations })
                let flatAnnos = toggleAnnotation model.flattenChild c
                { model with rootGroup = updateItemAt p func model.rootGroup; flattenChild = flatAnnos }
            | UpdateChildProperties (a,p) ->
                let func = (fun (x:AnnotationGroup) -> { x with annotations = if PList.count x.annotations > 0 then updateChild x.annotations a else x.annotations })
                let flatA = model.flattenChild.AsList |> List.updateIf (fun x -> x.key = a.key) (fun _ -> a) |> PList.ofList  
                { model with rootGroup = updateItemAt p func model.rootGroup; flattenChild = flatA }
            | SetGroupName t -> 
                let func = (fun (x:AnnotationGroup) -> { x with name = t })
                { model with rootGroup = updateItemAt model.activeGroupPath func model.rootGroup; activeGroupName = t }
            | _ -> model
    
    let rec getAllAnnotationsFromSubGroups (root:MAnnotationGroup) = 
        let anns = root.annotations //|> AList.map(fun x -> Mod.force)
        let l = (root.subGroups |> AList.map(fun x -> getAllAnnotationsFromSubGroups(x))) |> AList.concat 
        l    
    
    let getAllAnnotations (root:MAnnotationGroup) =
        let subAnnos = getAllAnnotationsFromSubGroups root
        let anns = root.annotations //|> AList.map(fun x -> Mod.force)
        subAnnos |> AList.map (fun x -> anns |> AList.append x) |> AList.concat

    //let tryGetChild (ag:AnnotationGroup) key = 
    //    let annos = getAllAnnotations ag |> PList.toSeq |> Seq.concat |> PList.ofSeq
    //    annos |> Seq.tryFind(fun x -> x.key = key)


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


