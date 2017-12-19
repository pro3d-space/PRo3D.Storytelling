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
        | AddChildren           of list<Index>*plist<Child>    
        | RemoveChild           of Guid*list<Index> 
        | ToggleChildVisibility of Guid*list<Index> 
        | ActiveChild           of Guid*list<Index>*string
        | UpdateChildProperties of Child
        | SetGroupName          of string
        | SetChildName          of string
        | ClearGroup            of list<Index>
        | SelectChilds          of list<Index>*Guid*string 
        | ToggleGroup           of list<Index>
        | MoveChilds      
        | ClearSelection
        | UpdateCam             of Guid
        | Nop

    let tryGet' (ans:hset<Child>) id = 
            ans |> Seq.tryFind(fun x -> x.id = id)

    let updateGroups (model:GroupsModel) (root:Group) (flat:hset<Child>) =        
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
    
    let removeChild (id:Guid) (childs:plist<Child>) = 
        childs |> PList.filter (fun x -> x.id <> id)   

    let toggleChild (id:Guid) (childs:plist<Child>) =
        childs |> PList.map(fun x -> if x.id = id then Child.toggleVisibility x else x) 
        
    let toggleChildList (childs:plist<Child>) =        
        childs |> PList.map(fun x -> Child.toggleVisibility x)
    
    let updateChild (c:Child) (childs:plist<Child>) = 
        childs |> PList.map(fun x -> if x.id = c.id then c else x)

    // add one child to children of active group
    let addToActiveGroup model (a:Child) = 
        let func = (fun (x : Group) -> { x with childList = x.childList |> PList.prepend a })
        let newRoot = updateItemAt model.activeGroup.path func model.rootGroup
        updateGroups model newRoot (flatten newRoot)

    // add a list of children to children of active group
    let AddChildren path cs model = 
        let func = (fun (x:Group) -> { x with childList = x.childList |> PList.append' cs })
        let newRoot = updateItemAt path func model.rootGroup
        updateGroups model newRoot (flatten newRoot)
    
    let bla = updateChild

    let updateChildProperties (model:GroupsModel) (c:Child) =
        let func = (fun (x:Group) -> { x with childList = x.childList |> PList.applyNonEmpty (updateChild c)})
        let newRoot = updateItemAt model.activeChild.path func model.rootGroup
        let m' = updateGroups model newRoot (flatten newRoot)
        { m' with lastSelectedChild = Some c }
    
    let rec updateChildList (children:list<Child> ) (model:GroupsModel) =
        match children with
          | x::rest ->
                let m' = updateChildProperties model x
                updateChildList rest m'
          |_ -> model
    
    let removeFromSelection model id path =
        { model with selectedChilds = model.selectedChilds |> HSet.remove( { id = id; path = path; name = "" } ) }
    
    let removeChild2 model id path =        
        let func = (fun (x:Group) -> { x with childList = x.childList |> PList.applyNonEmpty (removeChild id) })
        let newRoot = updateItemAt path func model.rootGroup                
        let m = removeFromSelection model id path
        let flat = flatten newRoot
        Log.error "annotations number: %d" flat.Count
        updateGroups m newRoot flat

    let rec removeSelectionfromTree (selection:list<TreeSelection> ) (m:GroupsModel) =
        match selection with
          | x::rest ->                
                let m' = removeChild2 m x.id x.path
                removeSelectionfromTree rest m'
          |_ -> m

    
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
        let m' = updateGroups m newRoot1 (flatten newRoot1)

        // update selection paths
        let sel = (model.selectedChilds |> HSet.map( fun x -> {x with path = m.activeGroup.path }))
        { m' with selectedChilds = sel}

    
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
                let func = (fun (x:Group) -> { x with expanded = not x.expanded })
                { model with rootGroup = updateItemAt p func model.rootGroup }
            | AddGroup p -> 
                let newGroup = 
                    {    
                        name      = "newGroup" 
                        key       = Guid.NewGuid()
                        childList = plist.Empty
                        subGroups = plist.Empty
                        visible   = true
                        expanded  = true
                    }

                let func = 
                    (fun (x:Group) -> { x with subGroups = PList.append newGroup x.subGroups })

                { model with rootGroup = updateItemAt p func model.rootGroup }
            | RemoveGroup p -> 
                let func = 
                    fun (x:Group) (id:Guid) -> 
                        { x with subGroups = if PList.count x.subGroups > 0 then removeGroup x id else x.subGroups }

                let newRoot = deleteItemAt p func model.rootGroup
                let m' = updateGroups model newRoot (flatten newRoot)
                m'
            | AddChildren (p,cs) ->
                AddChildren p cs model
            | RemoveChild (k,p) ->
                removeChild2 model k p
            | ToggleChildVisibility (c,p) -> 
                let func = (fun (x:Group) -> { x with childList = x.childList |> PList.applyNonEmpty (toggleChild c)})
                let newRoot = updateItemAt p func model.rootGroup
                let m' = updateGroups model newRoot (flatten newRoot)
                m'
            | UpdateChildProperties a ->
                updateChildProperties model a
            | SetGroupName t -> 
                let func = 
                    fun (x:Group) -> { x with name = t }
              
                { model with 
                    rootGroup   = updateItemAt model.activeGroup.path func model.rootGroup
                    activeGroup = { model.activeGroup with name = t }
                }
            | SetChildName t -> 
                match model.lastSelectedChild with
                    | Some c -> let child = Child.setName c t
                                updateChildProperties model child
                    | None -> model
            | ClearGroup p -> 
                let selection = 
                  model.selectedChilds 
                    |> HSet.choose (fun x -> 
                        match x.path = p with
                          | true -> Some x
                          | _    -> None )

                let func = (function (x:Group) -> { x with childList = plist.Empty })
                let newRoot = updateItemAt p func model.rootGroup
                let m = { model with selectedChilds = selection }
                let m' = updateGroups m newRoot (flatten newRoot)
                m'
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
                        HSet.add treeSelection HSet.empty, Group.tryGet' model.flat id

                { model with 
                    selectedChilds    = selection; 
                    activeChild       = treeSelection; 
                    lastSelectedItem  = SelectedItem.Child;
                    lastSelectedChild = child }
            | ToggleGroup p ->
                let func = (fun (x:Group) -> { x with childList = if PList.count x.childList > 0 then toggleChildList x.childList else x.childList })
                let func1 = (fun (x:Group) -> { x with visible = not x.visible })

                let newRoot = updateItemAt p func model.rootGroup
                let newRoot1 = updateItemAt p func1 newRoot
                let m' = updateGroups model newRoot1 (flatten newRoot1)
                m'
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

    let initSurfaceGroups group selected = 
        let surfaces = GroupsModel.initial
        { surfaces with rootGroup = group; lastSelectedChild = selected }

    let initSurfaceModel group selected =
        let surfaces = initSurfaceGroups group selected
        {
            surfaces    = surfaces
            sgSurfaces  = hmap.Empty
            sgGrouped   = plist.Empty
            kdTreeCache = hmap.Empty
        }

    let surfaceModelPickler : Pickler<SurfaceModel> =
        Pickler.product initSurfaceModel
        ^+ Pickler.field (fun s -> s.surfaces.rootGroup)             Pickler.auto<Group> //Surface
        ^. Pickler.field (fun s -> s.surfaces.lastSelectedChild)     Pickler.auto<option<Child>> //Pickler.auto<option<Surface>>

    let initial = initSurfaceModel Group.initRoot None   
    
    registry.RegisterFactory (fun _ -> surfaceModelPickler)

    let getSurface model guid =        
        Groups.tryGetChild model.surfaces.rootGroup guid

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
            Groups.updateChildProperties model.surfaces (Child.Surfaces s')

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


