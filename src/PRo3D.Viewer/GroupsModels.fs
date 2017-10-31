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
type AnnotationGroup = {
    key         : Guid
    name        : string
    childList   : plist<Child> //plist<Annotation> 
    subGroups   : plist<AnnotationGroup>
    visible     : bool
    expanded    : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationGroup =

    let Linear = {
        name = "Linear"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = plist.Empty
        visible = true
        expanded = true
        }
    let ViewPoint = {
        name = "ViewPoint"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = plist.Empty
        visible = true
        expanded = true
        }
    let Sky = {
        name = "Sky"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = plist.Empty
        visible = true
        expanded = true
        }
    let Point = {
        name = "Point"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = plist.Empty
        visible = true
        expanded = true
        }
    let Line = {
        name = "Line"
        key =  Guid.NewGuid()
        childList = plist.Empty
        //subGroups = [ Linear; ViewPoint; Sky ] |> PList.ofList
        subGroups = PList.empty
        visible = true
        expanded = true
        }
    let Polyline = {
        name = "Polyline"
        key =  Guid.NewGuid()
        childList = plist.Empty
       // subGroups = [ Linear; ViewPoint; Sky ] |> PList.ofList
        subGroups = PList.empty
        visible = true
        expanded = true
        }
    let Polygon = {
        name = "Polygon"
        key =  Guid.NewGuid()
        childList = plist.Empty
        //subGroups = [ Linear; ViewPoint; Sky ] |> PList.ofList
        subGroups = PList.empty
        visible = true
        expanded = true
        }
    let surface1Group = {
        name = "surface1"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = [Point; Line; Polyline; Polygon] |> PList.ofList
        visible = true
        expanded = true
        }
    let surface2Group = {
        name = "surface2"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = [Point; Polyline] |> PList.ofList
        visible = true
        expanded = true
        }
    let initRoot = {
        name = "Annotations"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = [surface1Group; ] |> PList.ofList //surface2Group
        visible = true
        expanded = true
        }
    let initRoot2 = {
        name = "Annotations"
        key =  Guid.NewGuid()
        childList = plist.Empty
        subGroups = plist.Empty 
        visible = true
        expanded = true
        }

type SelectedItem = Annotation = 0 | Surface = 1 | AnnotationGroup = 2

[<DomainType>]
type AnnotationGroupsModel = {
    rootGroup       : AnnotationGroup
    activeGroupKey       : Guid
    activeGroupPath      : list<Index>
    activeGroupName      : string
    activeChildKey       : Guid
    activeChildPath      : list<Index>
    annotationsFlat      : hset<Annotation>
    lastSelectedItem     : SelectedItem
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationGroupsModel =
    let initial = {
        rootGroup = AnnotationGroup.initRoot2
        activeGroupKey =  Guid.Empty
        activeGroupPath = list.Empty
        activeGroupName  = ""
        activeChildKey = Guid.Empty
        activeChildPath = list.Empty // (is path of parent group!)
        annotationsFlat = hset.Empty
        lastSelectedItem = SelectedItem.Annotation
        }

        
[<DomainType>]
type AnnotationGroupsImporterModel = {
    rootGroupI       : plist<AnnotationGroup>
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