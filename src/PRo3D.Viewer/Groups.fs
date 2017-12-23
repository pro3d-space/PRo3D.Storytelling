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
        | AddChildren           of list<Index>*plist<Leaf>    
        | RemoveChild           of Guid*list<Index> 
        | ToggleChildVisibility of Guid*list<Index> 
        | ActiveChild           of Guid*list<Index>*string
        | UpdateChildProperties of Leaf
        | SetGroupName          of string
        | SetChildName          of string
        | ClearGroup            of list<Index>
        | SelectChilds          of list<Index>*Guid*string 
        | ToggleGroup           of list<Index>
        | MoveChilds      
        | ClearSelection
        | UpdateCam             of Guid
        | Nop

    let tryGet' (ans:hset<Leaf>) id = 
            ans |> Seq.tryFind(fun x -> x.id = id)   
        
    //let rec flatten (g:Node) : hset<Leaf> =
    //    HSet.empty

    //    //match ((PList.toList g.childList ), (PList.toList g.subGroups)) with
    //    //    | ([], []) -> HSet.empty
    //    //    | (c, [] ) -> c |> HSet.ofList
    //    //    | ([], sg) -> sg |> HSet.ofList |> HSet.collect flatten 
    //    //    | (c, sg)  -> 
    //    //        let sga = 
    //    //            sg |> HSet.ofList |> HSet.collect flatten                    
    //    //        HSet.union (c |> HSet.ofList) sga    
    
    let loadAnnotationGroups (path:string) =
        Serialization.loadAs<PRo3D.Groups.GroupsModel> path                  

    let updateNodeAt (p : list<Index>) (f : Node -> Node) (t : Node) = 
        let rec go (p : list<Index>) (t : Node) =
            match p with
              | [] -> f t
              | x::rest ->
                    match PList.tryGet x t.subNodes with
                      | Some c -> { t with subNodes = PList.set x (go rest c) t.subNodes }                                                  
                      | None   -> t
        go (List.rev p) t 
    
    let updateActiveGroup (f : Node -> Node) (m : GroupsModel) =
        let root = updateNodeAt m.activeGroup.path f m.rootGroup
        { m with rootGroup = root }
    
    let deleteItemAt (p : list<Index>) (f : Node -> Guid -> Node) (t : Node) = 
        let rec go (p : list<Index>) (t : Node)  =
            match p with
              | x::rest -> 
                match rest with
                  | [] -> 
                    match PList.tryGet x t.subNodes with
                      | Some c -> f t c.key
                      | None -> t
                  | _ -> 
                    match PList.tryGet x t.subNodes with
                      | Some c -> { t with subNodes = PList.set x (go rest c) t.subNodes }                                 
                      | None   -> t
              | _ -> t
        go (List.rev p) t 
        
    let removeGroup (ag:Node) (id:Guid) = 
        ag.subNodes |> PList.filter (fun x -> x.key <> id)        

    //let toggleChild (id:Guid) (childs:plist<Leaf>) =
    //    childs |> PList.map(fun x -> if x.id = id then Child.toggleVisibility x else x) 
        
    //let toggleChildList (childs:plist<Leaf>) =        
    //    childs |> PList.map(fun x -> Child.toggleVisibility x)
    
    //let updateChild (c:Leaf) (childs:plist<Leaf>) = 
    //    childs |> PList.map(fun x -> if x.id = c.id then c else x)

    // add one child to children of active group ... maybe move to active group
    let addToActiveGroup model (a:Guid) = 
        let func = (fun (x : Node) -> { x with leaves = x.leaves |> PList.prepend a })
        let newRoot = updateNodeAt model.activeGroup.path func model.rootGroup
        //updateGroups model newRoot (flatten newRoot)
        failwith "addToActiveGroup"

    // add a list of children to children of active group
    let AddChildren path (cs : plist<Leaf>) model = 
        let func = (fun (x:Node) -> { x with leaves = x.leaves |> PList.append' (cs |> PList.map (fun x -> x.id)) })

        //add ids to group
        let newRoot = updateNodeAt path func model.rootGroup

        //add child nodes to flat
        let cs = 
            cs |> PList.toList |> List.map(fun x -> (x.id,x))|> HMap.ofList
        
        { model with rootGroup = newRoot; flat = model.flat |> HMap.union cs }            

    let updateChildProperties (m:GroupsModel) (l:Leaf) : GroupsModel =

        let flat' = 
            m.flat 
              |> HMap.update l.id (fun x ->
                match x with 
                  | Some k -> l
                  | None -> failwith "not found"
                )

        { m with flat = flat' }

        //failwith "updateChildProperties"
        //let func = (fun (x:Group) -> { x with childList = x.childList |> PList.applyNonEmpty (updateChild c)})
        //let newRoot = updateItemAt model.activeChild.path func model.rootGroup
        //let m' = updateGroups model newRoot (flatten newRoot)
        //{ m' with lastSelectedChild = Some c }
    
    let rec updateChildList (children:list<Leaf> ) (model:GroupsModel) =
        match children with
          | x::rest ->
                let m' = updateChildProperties model x
                updateChildList rest m'
          |_ -> model
    
    let removeFromSelection id path model =
        { model with selectedChilds = model.selectedChilds |> HSet.remove( { id = id; path = path; name = "" } ) }
    
    let removeLeaf model (id:Guid) path removeFromFlat =
        let func = (fun (x:Node) -> { x with leaves = x.leaves |> PList.remove' id })
        let root' = updateNodeAt path func model.rootGroup

        let m = model |> removeFromSelection id path

        if removeFromFlat then
            let flat' = m.flat |> HMap.remove id
            { m with rootGroup = root'; flat = flat' }
        else
            { m with rootGroup = root' } 

    let rec removeSelected (selection:list<TreeSelection>) (removeFromFlat:bool) (m:GroupsModel)  =
        match selection with
          | x::rest ->                
                let m' = removeLeaf m x.id x.path removeFromFlat
                removeSelected rest removeFromFlat m'
          |_ -> m

    
    let moveChildren m =
       
        let toMove = 
            m.selectedChilds
              |> HSet.choose (fun x -> m.flat |> HMap.tryFind x.id)
              |> HSet.toSeq
              |> PList.ofSeq
                
        // * delete leaves in source group
        // * add leaves to destination group
        let m = 
          m
            |> removeSelected    (m.selectedChilds |> HSet.toList) false
            |> updateActiveGroup (fun (x:Node) -> { x with leaves = x.leaves |> PList.append' (toMove |> PList.map(fun x -> x.id)) })
         
        // update selection paths
        let sel = (m.selectedChilds |> HSet.map( fun x -> {x with path = m.activeGroup.path }))
        { m with selectedChilds = sel}        

    
    let update (model : GroupsModel) (action : Action) =
        printfn "action: %A" action
        match action with
            | ActiveGroup (g, p, s) -> 
                let selection = {
                    id   = g
                    path = p
                    name = s}
                { model with 
                    activeGroup      = selection
                    lastSelectedItem = SelectedItem.Group } 
            | ActiveChild (g, p, s) -> 
                { model with
                    activeChild = { id = g; path = p; name = s }
                    lastSelectedItem = SelectedItem.Child } 
            | ToggleExpand p -> 
                let func = (fun (x:Node) -> { x with expanded = not x.expanded })
                { model with rootGroup = updateNodeAt p func model.rootGroup }
            | AddGroup p -> 
                let newGroup = 
                    {    
                        name      = "newGroup" 
                        key       = Guid.NewGuid()
                        leaves = plist.Empty
                        subNodes = plist.Empty
                        visible   = true
                        expanded  = true
                    }

                let func = 
                    (fun (x:Node) -> { x with subNodes = PList.append newGroup x.subNodes })

                { model with rootGroup = updateNodeAt p func model.rootGroup }
            | RemoveGroup p -> 
                failwith "remove group"
                //let func = 
                //    fun (x:Group) (id:Guid) -> 
                //        { x with subGroups = if PList.count x.subGroups > 0 then removeGroup x id else x.subGroups }

                //let newRoot = deleteItemAt p func model.rootGroup
                //let m' = updateGroups model newRoot (flatten newRoot)
                //m'
                model
            | AddChildren (p,cs) ->
                failwith "addchildren"
                //AddChildren p cs model
                model
            | RemoveChild (k,p) ->
                removeLeaf model k p true
            | ToggleChildVisibility (c,p) -> 
                failwith "togglechild visible"
                //let func = (fun (x:Group) -> { x with childList = x.childList |> PList.applyNonEmpty (toggleChild c)})
                //let newRoot = updateItemAt p func model.rootGroup
                //let m' = updateGroups model newRoot (flatten newRoot)
                //m'
                model
            | UpdateChildProperties a ->
                updateChildProperties model a
            | SetGroupName t -> 
                let func = 
                    fun (x:Node) -> { x with name = t }
              
                { model with 
                    rootGroup   = updateNodeAt model.activeGroup.path func model.rootGroup
                    activeGroup = { model.activeGroup with name = t }
                }
            | SetChildName t -> 
                match model.lastSelectedChild with
                    | Some c -> let child = Leaf.setName c t
                                updateChildProperties model child
                    | None -> model
            | ClearGroup p -> 
                failwith "clear group"

                //let selection = 
                //  model.selectedChilds 
                //    |> HSet.choose (fun x -> 
                //        match x.path = p with
                //          | true -> Some x
                //          | _    -> None )

                //let func = (function (x:Group) -> { x with childList = plist.Empty })
                //let newRoot = updateItemAt p func model.rootGroup
                //let m = { model with selectedChilds = selection }
                //let m' = updateGroups m newRoot (flatten newRoot)
                //m'
                model
            | SelectChilds (p,id,s) ->
                let selected = model.selectedChilds
                let treeSelection = {
                    id = id
                    path = p
                    name = s
                    }

                //multiselect selected, singleselece hset.empty
                let selection, child =
                    if HSet.contains treeSelection selected
                    then 
                        HSet.remove treeSelection selected, None
                    else
                        HSet.add treeSelection HSet.empty, (model.flat |> HMap.tryFind id)

                { model with 
                    selectedChilds    = selection; 
                    activeChild       = treeSelection; 
                    lastSelectedItem  = SelectedItem.Child;
                    lastSelectedChild = child }
            | ToggleGroup p ->
                failwith "togglegroup"

                let func = (fun (x:Node) -> x) //{ x with childList = if PList.count x.childList > 0 then toggleChildList x.childList else x.childList })
                let func1 = (fun (x:Node) -> { x with visible = not x.visible })

                let newRoot = updateNodeAt p func model.rootGroup
                let newRoot1 = updateNodeAt p func1 newRoot
                //let m' = updateGroups model newRoot1 hmap.Empty //(flatten newRoot1)
                //m'
                model
            | MoveChilds  -> 
                let m = moveChildren model
                m
            | ClearSelection ->
                { model with selectedChilds = hset.Empty }
            | UpdateCam id -> model
            | _ -> model
    

    let mkColor (activeId : Guid) (groupId : Guid) = 
        if activeId = groupId then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White
    
    let mkGroupAttributeMap icon action =
        amap {
            let! icon = icon
            yield clazz icon
            yield onClick (fun _ -> action)
        } |> AttributeMap.ofAMap

    let viewGroupButtons =
        div [clazz "ui buttons inverted"] [
            onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                button [clazz "ui icon button"; attribute "data-content" "Move Selection"; onMouseClick (fun _ -> MoveChilds)] [
                        i [clazz "Move icon"] [] ] 
            )
            onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                button [clazz "ui icon button"; attribute "data-content" "Clear Selection"; onMouseClick (fun _ -> ClearSelection)] [
                        i [clazz "Remove icon"] [] ]
            )
        ] 

    let viewUI (model : MGroupsModel) =   
        let ts = model.activeGroup
        require Html.semui (
            Html.table [                            
                Html.row "Change Groupname:"[Html.SemUi.textBox (ts |> Mod.map (fun x -> x.name)) SetGroupName ]
            ]
        )

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SurfaceModel =
    open MBrace.FsPickler
    open MBrace.FsPickler.Combinators    

    open Aardvark.Opc
    open PRo3D.Groups
    open PRo3D.Surfaces

    let withSgSurfaces sgSurfaces m =
        { m with sgSurfaces = sgSurfaces }

    let initSurfaceGroups group flat selected = 
        let surfaces = GroupsModel.initial
        { surfaces with rootGroup = group; lastSelectedChild = selected; flat = flat }

    let initSurfaceModel group flat selected =
        let surfaces = initSurfaceGroups group flat selected
        {
            surfaces    = surfaces
            sgSurfaces  = hmap.Empty
            sgGrouped   = plist.Empty
            kdTreeCache = hmap.Empty
        }

    let surfaceModelPickler : Pickler<SurfaceModel> =
        Pickler.product initSurfaceModel
        ^+ Pickler.field (fun s -> s.surfaces.rootGroup)             Pickler.auto<Node> //Surface
        ^+ Pickler.field (fun s -> s.surfaces.flat)                  Pickler.auto<hmap<Guid,Leaf>> //Surface
        ^. Pickler.field (fun s -> s.surfaces.lastSelectedChild)     Pickler.auto<option<Leaf>> //Pickler.auto<option<Surface>>

    let initial = initSurfaceModel Group.initRoot hmap.Empty None   
    
    registry.RegisterFactory (fun _ -> surfaceModelPickler)

    let getSurface model guid =
        model.surfaces.flat |> HMap.tryFind guid        

    let groupSurfaces (sgSurfaces : hmap<Guid, SgSurface>) =
        sgSurfaces
            |> HMap.toList
            |> List.groupBy(fun (g,x) -> x.surface.priority.value)
            |> List.map( fun (p,k) -> (p, k |> HMap.ofList))
            |> List.sortBy fst
            |> List.map snd
            |> PList.ofList
    
    let triggerSgGrouping (model:SurfaceModel) =
        { model with sgGrouped = (groupSurfaces model.sgSurfaces) }           
   
    let updateSingleSurface (s':PRo3D.Surfaces.Surface) (model:SurfaceModel) =
        let surfaces' = 
            Groups.updateChildProperties model.surfaces (Leaf.Surfaces s')

        // update sgsurfaces as well !!!
        let sgSurfaces' = 
            model.sgSurfaces 
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)   

        // update sggroup as well !!!
        let group' = 
            model.sgGrouped
              |> PList.map(fun surfs ->
                surfs
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)
                )

        { model with surfaces = surfaces'; sgSurfaces = sgSurfaces'; sgGrouped = group' }

    let updateSurfaces model (surfaces' : plist<PRo3D.Surfaces.Surface>) =
        let sgSurfaces' =
            surfaces' 
                |> PList.toList
                |> List.map (
                    fun x -> 
                        let update = 
                            (fun (d : Option<SgSurface>) ->
                                match d with 
                                    | Some k -> Some { k with surface = x }
                                    | None -> None)

                        HMap.alter x.guid update model.sgSurfaces ) 
                |> List.fold (fun a b -> HMap.union a b) HMap.empty

        { model with sgSurfaces = sgSurfaces' }


