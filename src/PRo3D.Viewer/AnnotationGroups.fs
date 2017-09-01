namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.Base.Incremental

open System

open PRo3D


module AnnotationGroups =
   
    type Action =
        | ActiveGroup    of Guid
        //| Click of list<Index>
        | ToggleExpand of list<Index>
        //| AddGroup of list<Index>
        //| RemoveGroup of list<Index>
        //| Nop

    //let click v () = Click v
    //let toggle v () = ToggleExpand v
    //let addChild v () = AddGroup v
    //let remChild v () = RemoveGroup v

    let updateGroupAt (p : list<Index>) (t : AnnotationGroup) = //(f : AnnotationGroup -> AnnotationGroup) (t : AnnotationGroup) =
        let rec go (p : list<Index>) (t : AnnotationGroup)  =
            match p with
                | [] -> t
                | x::rest -> match PList.tryGet x t.subGroups with
                                | Some c -> let ag = {
                                                name = c.name
                                                key = c.key
                                                annotations = c.annotations
                                                subGroups = PList.set x (go rest c) t.subGroups
                                                visible = c.visible
                                                active = c.active
                                                expanded = c.expanded
                                                }
                                            ag
                                | None   -> t
        go (List.rev p) t

    let update (model : AnnotationGroupsModel) (action : Action) =
        printfn "action: %A" action
        match action with
            | ActiveGroup g         -> { model with activeGroup = g }
            //| Click p ->                 
            //    { model with
            //        rootGroup = updateAt p (function | Leaf v ->( match v with 
            //                                                    | LeafValue.Number n -> Leaf ( LeafValue.Number (n + 1))
            //                                                    | LeafValue.Text t -> Leaf ( LeafValue.Text (sprintf "%s a" t)))
            //                                            | p -> p) model.data
            //    }
            //| ToggleExpand p -> 
            //    { model with
            //        rootGroup = 
            //            updateGroupAt p ( 
            //                function | Leaf v -> Leaf v
            //                         | Node(l,p,xs) -> 
            //                             Node(l, { p with isExpanded = not p.isExpanded}, xs)
            //            ) model.data
            //    }
            //| AddGroup p -> 
            //    { model with
            //        data = updateAt p (
            //                 function | Leaf v -> Leaf v
            //                          | Node(l,p,xs) -> 
            //                                let value = match l with
            //                                               | Number n -> Number (PList.count xs + 1)
            //                                               | Text   t -> LeafValue.Text t
            //                                Node(l,p, PList.append (Leaf value) xs)
            //               ) model.data
            //    }
            //| RemoveGroup p -> 
            //    { model with
            //        data = updateAt p (
            //                 function | Leaf v -> Leaf v
            //                          | Node(l,p,xs) -> 
            //                              Node(l,p, if PList.count xs > 0 then PList.removeAt 0 xs else xs)
            //               ) model.data
            //    }
            //| Nop -> model
    
    
    let mkColor (active : IMod<bool>) (ag : MAnnotationGroup) =
        let a = active |> Mod.force
        let color = match a with
                        | true -> Mod.constant C4b.Red
                        | _ ->  Mod.constant C4b.Blue
        color

    let rec viewTree path (model : MAnnotationGroup) =
        alist {

            let color = sprintf "color: %s" (Html.ofC4b C4b.White)
            let children = AList.collecti (fun i v -> viewTree (i::path) v) model.subGroups
            let desc =
                div [style color] [
                        Incremental.text model.name
                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                            i [ clazz "plus icon";  
                                attribute "title" "Add Group";
                                attribute "data-position" "left center";
                                 attribute "data-variation" "mini" ] [] //onClick (remChild path)
                            )
                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (            
                            i [ clazz "remove icon";  
                                attribute "title" "Remove Group";
                                attribute "data-position" "left center";
                                attribute "data-variation" "mini" ] [] //onClick (remChild path)
                            )
                ]
           
            let! expanded = model.expanded
            let visibleIcon = if expanded then "icon large outline open folder" else "icon large outline folder"
            let select = fun _ -> ActiveGroup (model.key |> Mod.force)

            let! c = mkColor model.active model
            let bgc = sprintf "color: %s" (Html.ofC4b c)
           
            let itemAttributes =
                amap {
                    //yield onMouseClick (fun _ -> clickMsg ())
                    let! selected = model.expanded
                    if selected then yield clazz "icon large outline open folder"
                    else             yield clazz "icon large outline folder"
                } |> AttributeMap.ofAMap

            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = model.expanded
                    if isExpanded then yield style "visible"
                    else yield style "hidden"
                }

            yield div [ clazz "item" ] [
                 i [clazz visibleIcon; onClick select; style bgc][]
                 //Incremental.i itemAttributes AList.empty
                 div [ clazz "content" ] [
                     div [ clazz "header"] [desc]
                     Incremental.div (AttributeMap.ofAMap childrenAttribs) 
                        <| alist { 
                            let! isExpanded = model.expanded
                            if isExpanded then yield! children
                        }
                 ]
            ]
                
        }

    let view (model : MAnnotationGroupsModel) =
        require Html.semui (
            TreeView.view [] (viewTree [] model.rootGroup)
        )