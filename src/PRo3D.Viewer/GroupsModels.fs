namespace PRo3D.Groups

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D
open PRo3D.Surfaces
open PRo3D.BookmarksX
open PRo3D.Navigation2
//open PRo3D.Groups

open Aardvark.VRVis
open Aardvark.Opc 


//type Groupable =
//    | Annotations of plist<Annotation>
//    | Surfaces    of plist<Surface>

//[<DomainType>]
//type Group<'a> = {
//    key         : Guid
//    name        : string
//    content     : plist<'a> 
//    subGroups   : plist<Group<'a>>
//    visible     : bool
//    expanded    : bool
//    }
[<DomainType>]
type Child = 
    | Annotations of value : Annotation
    | Surfaces    of value : Surface
    | Bookmarks   of value : Bookmark
    with 
    [<PrimaryKey>] 
        member x.id =
            match x with
                | Annotations a -> a.key
                | Surfaces    s -> s.guid
                | Bookmarks   b -> b.key
            
module Child =
    open Aardvark.UI.Incremental

    let toggleVisibility (c : Child) =
        match c with
          | Annotations a -> Annotations { a with visible   = not a.visible }
          | Surfaces    s -> Surfaces    { s with isVisible = not s.isVisible }
          | _ -> c

    let setName (c: Child) (name: string) =
        match c with
          | Surfaces s  -> Surfaces    { s with name = name }
          | Bookmarks b -> Bookmarks   { b with name = name }
          | _ -> c

    let childrenToSurfaces surfaces =
        surfaces
            |> HSet.choose(fun x -> match x with
                                        | Child.Surfaces s -> Some s
                                        | _ -> None )
    let childrenToAnnotations annos =
        annos
            |> HSet.choose(fun x -> match x with
                                        | Child.Annotations a -> Some a
                                        | _ -> None )
    let childrenToBookmarks bms =
        bms
            |> HSet.choose(fun x -> match x with
                                        | Child.Bookmarks b -> Some b
                                        | _ -> None )

    

[<DomainType>]
type Group = {
    key         : Guid
    name        : string
    childList   : plist<Child> //plist<Annotation> 
    subGroups   : plist<Group>
    visible     : bool
    expanded    : bool
}

type TreeSelection = {
    [<PrimaryKey>]
    id : Guid
    path : list<Index>
    name : string
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Group =
    let initRoot = {
        name = "root"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = plist.Empty 
        visible = true
        expanded = true
        }
    let initTreeSelection =  { 
        id = Guid.Empty
        path = list.Empty
        name = "" 
        }

    let tryGet' (ans:hset<Child>) id = 
            ans |> Seq.tryFind(fun x -> x.id = id)

type SelectedItem = Child = 0 | Group = 1


[<DomainType>]
type GroupsModel = {
    rootGroup            : Group
    activeGroup          : TreeSelection
    activeChild          : TreeSelection
    flat                 : hset<Child>
    lastSelectedItem     : SelectedItem
    selectedChilds       : hset<TreeSelection> 
    lastSelectedChild    : option<Child>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GroupsModel =
    let initial = {
        rootGroup = Group.initRoot
        activeGroup = Group.initTreeSelection
        activeChild = Group.initTreeSelection
        flat = hset.Empty
        lastSelectedItem = SelectedItem.Child
        selectedChilds = hset.Empty
        lastSelectedChild = None
        }

        
[<DomainType>]
type AnnotationGroupsImporterModel = {
    rootGroupI       : plist<Group>
}

[<DomainType>]
type BookmarksModel = {
    bookmarks   : GroupsModel 
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BookmarksModel =
    let initial = {
        bookmarks        = GroupsModel.initial
        }

[<DomainType>]
type SurfaceModel = {
    surfaces        : GroupsModel
    sgSurfaces      : hmap<Guid,SgSurface>
    sgGrouped       : plist<hmap<Guid,SgSurface>>
    kdTreeCache     : hmap<string, ConcreteKdIntersectionTree>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SurfaceModel =
    open MBrace.FsPickler
    open MBrace.FsPickler.Combinators    

    let fillSurfaceGroup a b = 
        let surfaces = GroupsModel.initial
        { surfaces with rootGroup = { surfaces.rootGroup with childList = a}; lastSelectedChild = b }

    let makeSurfaceModel a b =
        let surfaces = fillSurfaceGroup a b
        {
            surfaces = surfaces
            //selectedSurface = b
            sgSurfaces = hmap.Empty
            sgGrouped = plist.Empty
            kdTreeCache = hmap.Empty
        }

    let surfaceModelPickler : Pickler<SurfaceModel> =
        Pickler.product makeSurfaceModel
        ^+ Pickler.field (fun s -> s.surfaces.rootGroup.childList)   Pickler.auto<plist<Child>> //Surface
        ^. Pickler.field (fun s -> s.surfaces.lastSelectedChild)     Pickler.auto<option<Child>> //Pickler.auto<option<Surface>>

    let initial = makeSurfaceModel PList.empty None   

    
    registry.RegisterFactory (fun _ -> surfaceModelPickler)

    //FsPickler.RegisterPicklerFactory (fun _ -> surfaceModelPickler)