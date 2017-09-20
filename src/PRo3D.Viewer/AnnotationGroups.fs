namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.Base.Incremental

open System

open PRo3D


module AnnotationGroups =
   
    type Action =
        | ActiveGroup    of Guid*list<Index>
        | ToggleExpand of list<Index>
        | AddGroup of list<Index>
        | RemoveGroup of list<Index>
        | AddChild of list<Index>*Annotation    // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | RemoveChild of list<Index>*Annotation // TODO: change Annotation to ChildNode (|Annotation | Surface)
        | ToggleChildVisibility of list<Index>
        | Nop


    let updateItemAt (p : list<Index>) (f : AnnotationGroup -> AnnotationGroup) (t : AnnotationGroup) = 
        let rec go (p : list<Index>) (t : AnnotationGroup)  =
            match p with
                | [] -> f t
                | x::rest -> match PList.tryGet x t.subGroups with
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
        
    let update (model : AnnotationGroupsModel) (action : Action) =
        printfn "action: %A" action
        match action with
            | ActiveGroup (g, p) -> { model with activeKey = g; activePath = p } 
            | ToggleExpand p -> 
                let funcT = (function x -> { name = x.name
                                             key = x.key
                                             annotations = x.annotations
                                             subGroups = x.subGroups
                                             visible = x.visible
                                             expanded = not x.expanded
                                             })
                { model with rootGroup = updateItemAt p funcT model.rootGroup }
            | AddGroup p -> 
                let newGroup = {    name = "newGroup" // TODO: get group name from user
                                    key =  Guid.NewGuid()
                                    annotations = plist.Empty
                                    subGroups = plist.Empty
                                    visible = true
                                    expanded = true
                                    }
                let func = (function x -> { name = x.name
                                            key = x.key
                                            annotations = x.annotations
                                            subGroups = PList.append newGroup x.subGroups
                                            visible = x.visible
                                            expanded = x.expanded
                                            })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | RemoveGroup p -> 
                let func = (fun (x:AnnotationGroup) (id:Guid) -> {  name = x.name
                                                                    key = x.key
                                                                    annotations = x.annotations
                                                                    subGroups = if PList.count x.subGroups > 0 then removeGroup x id else x.subGroups
                                                                    visible = x.visible
                                                                    expanded = x.expanded
                                                                })
                { model with rootGroup = deleteItemAt p func model.rootGroup }
            | AddChild (p,c) -> 
                let func = (function x -> {  name = x.name
                                             key = x.key
                                             annotations = x.annotations |> PList.prepend c
                                             subGroups = x.subGroups
                                             visible = x.visible
                                             expanded = x.expanded
                                             })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | RemoveChild (p,c) -> 
                let func = (fun (x:AnnotationGroup) (id:Guid) -> {  name = x.name
                                                                    key = x.key
                                                                    annotations = if PList.count x.annotations > 0 then removeAnnotation x.annotations c.key else x.annotations
                                                                    subGroups = x.subGroups
                                                                    visible = x.visible
                                                                    expanded = x.expanded
                                                                    })
                { model with rootGroup = deleteItemAt p func model.rootGroup }
            | ToggleChildVisibility p -> model // TODO
            | _ -> model
    
    
    let mkColor (activeId : IMod<Guid>) (groupId : IMod<Guid>) = 
        if activeId = groupId then Mod.constant C4b.Red else Mod.constant C4b.White
   
    let rec viewTree path (group : MAnnotationGroup) (model : MAnnotationGroupsModel) =

        alist {

            let! c = mkColor model.activeKey group.key
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
                                onClick (fun _ -> ActiveGroup (group.key |> Mod.force, path))] [] 
                            )
                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                            i [ clazz "plus icon" 
                                attribute "title" "Add Group"
                                attribute "data-position" "left center"
                                attribute "data-variation" "mini"
                                onMouseClick (fun _ -> AddGroup path)] [] 
                            )
                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                            i [ clazz "remove icon" 
                                attribute "title" "Remove Group"
                                attribute "data-position" "left center"
                                attribute "data-variation" "mini" 
                                onMouseClick (fun _ -> RemoveGroup path)] [] 
                            )
                ]
            
     
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> ToggleExpand path)
                    let! selected = group.expanded
                    if selected then yield clazz "icon large outline open folder"
                    else             yield clazz "icon large outline folder"
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

    let view (model : MAnnotationGroupsModel) =
        require Html.semui (
            TreeView.view [] (viewTree [] model.rootGroup model)
        )