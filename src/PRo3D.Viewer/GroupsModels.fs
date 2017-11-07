namespace PRo3D.Groups

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D
open PRo3D.Surfaces

[<DomainType>]
type Child = 
    | Annotations of value : Annotation
    | Surfaces of value : Surface //PRo3D.Surfaces.

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
    let initRoot2 = {
        name = "Annotations"
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

    let tryGet' (ans:hset<Annotation>) key = 
            ans |> Seq.tryFind(fun x -> x.key = key)

type SelectedItem = Annotation = 0 | Surface = 1 | AnnotationGroup = 2


[<DomainType>]
type AnnotationGroupsModel = {
    rootGroup            : Group
    activeGroup          : TreeSelection
    activeChild          : TreeSelection
    annotationsFlat      : hset<Annotation>
    lastSelectedItem     : SelectedItem
    selectedChilds       : hset<TreeSelection> 
    lastSelectedChild    : option<Annotation>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationGroupsModel =
    let initial = {
        rootGroup = Group.initRoot2
        activeGroup = Group.initTreeSelection
        activeChild = Group.initTreeSelection
        annotationsFlat = hset.Empty
        lastSelectedItem = SelectedItem.Annotation
        selectedChilds = hset.Empty
        lastSelectedChild = None
        }

        
[<DomainType>]
type AnnotationGroupsImporterModel = {
    rootGroupI       : plist<Group>
}

//[<DomainType>]
//type SurfaceGroupsModel = {
//    rootGroup            : AnnotationGroup
//    activeGroup          : option<AnnotationGroup>
//    activeGroupKey       : Guid
//    activeGroupPath      : list<Index>
//    activeGroupName      : string
//    activeChild          : option<Surface>
//    activeChildKey       : Guid
//    activeChildPath      : list<Index>
//    surfacesFlat         : plist<Surface>
//    lastSelectedItem     : SelectedItem
//    }

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module SurfaceGroupsModel =
    //let initial = {
    //    rootGroup = AnnotationGroup.initRoot2
    //    activeGroup = None
    //    activeGroupKey =  Guid.Empty
    //    activeGroupPath = list.Empty
    //    activeGroupName  = ""
    //    activeChild = None
    //    activeChildKey = Guid.Empty
    //    activeChildPath = list.Empty // (is path of parent group!)
    //    annotationsFlat = plist.Empty
    //    //lastSelectedItem = SelectedItem.Annotation
    //    }