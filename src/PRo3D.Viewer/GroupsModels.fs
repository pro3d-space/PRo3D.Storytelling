namespace PRo3D.Groups

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D
open PRo3D.Surfaces
open PRo3D.BookmarksX
open PRo3D.Navigation2

open Aardvark.VRVis
open Aardvark.Geometry
open Aardvark.SceneGraph.Opc 

[<DomainType>]
type Leaf = 
    | Annotations of value : Annotation
    | Surfaces    of value : Surface
    | Bookmarks   of value : Bookmark
    with     
      member x.id =
        match x with
          | Annotations a -> a.key
          | Surfaces    s -> s.guid
          | Bookmarks   b -> b.key

      member x.visible =
        match x with
          | Annotations a -> a.visible
          | Surfaces    s -> s.isVisible
          | Bookmarks   b -> true

      member x.setVisible (v:bool) =
        match x with
          | Annotations a -> Leaf.Annotations { a with visible = v }
          | Surfaces    s -> Leaf.Surfaces    { s with isVisible = v }
          | Bookmarks   _ -> x

       member x.active =
        match x with
          | Annotations a -> false
          | Surfaces    s -> s.isActive
          | Bookmarks   b -> false
                
[<DomainType>]
type Node = {
    key         : Guid
    name        : string
    leaves      : plist<Guid>
    subNodes    : plist<Node>
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
        leaves = plist.Empty
        subNodes = plist.Empty 
        visible = true
        expanded = true
        }
    let initTreeSelection =  { 
        id = Guid.Empty
        path = list.Empty
        name = "" 
        }

    let tryGet' (ans:hset<Leaf>) id = 
            ans |> Seq.tryFind(fun x -> x.id = id)

type SelectedItem = Child = 0 | Group = 1


[<DomainType>]
type GroupsModel = {
    rootGroup            : Node
    activeGroup          : TreeSelection
    activeChild          : TreeSelection
    flat                 : hmap<Guid,Leaf>
    groupsLookup         : hmap<Guid,string>
    lastSelectedItem     : SelectedItem
    selectedLeaves       : hset<TreeSelection> 
    singleSelectLeaf     : option<Guid>
    }

module Leaf =
    open Aardvark.UI.Incremental

    let toggleVisibility (c : Leaf) =
        match c with
          | Annotations a -> Annotations { a with visible   = not a.visible }
          | Surfaces    s -> Surfaces    { s with isVisible = not s.isVisible }
          | _ -> c

    let setName (c: Leaf) (name: string) =
        match c with
          | Surfaces s  -> Surfaces    { s with name = name }
          | Bookmarks b -> Bookmarks   { b with name = name }
          | _ -> c

    let toSurfaces surfaces =
        surfaces
            |> HMap.choose(fun _ x -> 
                match x with
                | Leaf.Surfaces s -> Some s
                | _ -> None )

    let toSurface leaf =
        match leaf with 
          | Leaf.Surfaces s -> s
          | _ -> failwith "wrong type"

    let toSurfaces' surfaces =
        surfaces |> PList.choose(fun x -> match x with | Leaf.Surfaces s -> Some s| _-> None)

    let toAnnotation leaf =
        match leaf with 
          | Leaf.Annotations a -> a
          | _ -> failwith "wrong type"

    let toAnnotations annotations =
        annotations
            |> HMap.choose(fun _ x -> 
                match x with
                | Leaf.Annotations s -> Some s
                | _ -> None )

    let childrenToAnnotations annos =
        annos
            |> HSet.choose(fun x -> 
                match x with
                    | Leaf.Annotations a -> Some a
                    | _ -> None )

    let childrenToBookmarks bms =
      bms
        |> HSet.choose(fun x -> 
          match x with
            | Leaf.Bookmarks b -> Some b
            | _ -> None )

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GroupsModel =
    let initial = {
        rootGroup        = Group.initRoot
        activeGroup      = Group.initTreeSelection
        activeChild      = Group.initTreeSelection
        flat             = hmap.Empty
        groupsLookup     = hmap.Empty
        lastSelectedItem = SelectedItem.Child
        selectedLeaves   = hset.Empty
        singleSelectLeaf = None
    }

        
[<DomainType>]
type AnnotationGroupsImporterModel = {
    rootGroupI       : plist<Node>
    flatI             : hmap<Guid,Leaf>
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

type saveAnnotations = {
    annotations: GroupsModel 
    colorLegend : FalseColorsModel
}

//[<DomainType>]
//type Grouping<'a> = {
//  root                 : Node
//  flat                 : hmap<Guid,'a>
//  activeGroup          : TreeSelection
//  activeChild          : TreeSelection  
//  groupsLookup         : hmap<Guid,string>
//  lastSelectedItem     : SelectedItem
//  selectedLeaves       : hset<TreeSelection> 
//  singleSelectLeaf     : option<Guid>
//}

//type SaveAnnotations = {
//    annotations: Grouping<Annotation>
//    colorLegend : FalseColorsModel
//}

    //FsPickler.RegisterPicklerFactory (fun _ -> surfaceModelPickler)