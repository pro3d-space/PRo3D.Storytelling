namespace PRo3D.Groups

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D
open PRo3D.Surfaces


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
    //[<PrimaryKey>] //???
    with 
    [<PrimaryKey>] 
        member x.id =
            match x with
                | Annotations a -> a.key
                | Surfaces    s -> s.guid
            
module Child =
    let toggleVisibility (c : Child) =
        match c with
          | Annotations a -> Annotations { a with visible   = not a.visible }
          | Surfaces    s -> Surfaces    { s with isVisible = not s.isVisible }

//type Content = {
//    id      : Guid
//    visible : bool
//    child   : Child
//}
   
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
        rootGroup = Group.initRoot2
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