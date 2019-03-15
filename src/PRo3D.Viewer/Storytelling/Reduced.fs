namespace Provenance.Reduced

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open PRo3D
open PRo3D.Drawing
open PRo3D.Viewer
open PRo3D.Groups

open Model

type OCameraView = CameraView

// We need to redefine this because we want structural equality but CameraView is a class :/
[<DomainType>]
type CameraView = {
    sky : V3d
    location : V3d
    forward : V3d
    up : V3d
    right : V3d
}

type OMessage = AppAction

[<DomainType>]
type Message =
    | AddAnnotation
    | EditAnnotation
    | DeleteAnnotation
    | LoadAnnotations
    | GroupAction
    | Unknown 

    override x.ToString () =
        match x with
            | AddAnnotation     -> "A"
            | EditAnnotation    -> "E"
            | DeleteAnnotation  -> "D"
            | LoadAnnotations   -> "L"
            | GroupAction       -> "G"
            | Unknown           -> ""

// We want to compare the raw content of lists with the (=) operator; plist contains
// indices that might change by removing and adding elements, while the elements themselves remain
// the same (i think thats the case???).
[<CustomEquality; NoComparison>]
type rlist<'a when 'a : equality> =
    { inner : 'a plist }

    override x.GetHashCode () =
        x.inner |> PList.toList |> hash

    override x.Equals y =
        match y with 
            | :? rlist<'a> as y -> PList.toList x.inner = PList.toList y.inner
            | _ -> false

type OSegment = Segment

[<DomainType>]
type Segment = {
    startPoint  : V3d
    endPoint    : V3d
    points      : V3d rlist 
}

type OAnnotation = Annotation

[<DomainType>]
type Annotation = {
    [<PrimaryKey; NonIncremental>]
    key         : Guid

    modelTrafo  : Trafo3d

    geometry    : Geometry
    projection  : Projection
    semantic    : Semantic

    points      : V3d rlist
    segments    : Segment rlist 
    color       : ColorInput
    thickness   : NumericInput
    results     : AnnotationResults option
    dnsResults  : DipAndStrikeResults option

    visible     : bool
    showDns     : bool
    text        : string
    textsize    : NumericInput

    surfaceName : string
    view        : CameraView
}

type OAnnotationGroup = Node

[<DomainType>]
type AnnotationGroupState = {
    key         : Guid
    name        : string
    leaves      : Guid rlist
    subNodes    : AnnotationGroup rlist
    visible     : bool
}

and [<DomainType; CustomEquality; NoComparison>] AnnotationGroup = 
    { state     : AnnotationGroupState
      expanded  : bool }

    override x.GetHashCode () =
        hash x.state

    override x.Equals y =
        match y with 
            | :? AnnotationGroup as y -> x.state = y.state
            | _ -> false

type OAnnotations = GroupsModel

[<DomainType>]
type Annotations = {
    flat : hmap<Guid, Annotation>
    root : AnnotationGroup
}

type OState = AppModel

[<DomainType>]
type State = {
    annotations : Annotations
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CameraView =
    
    let create (v : OCameraView) : CameraView =
        { sky = v.Sky
          location = v.Location
          forward = v.Forward
          up = v.Up
          right = v.Right }

    let restore (v : CameraView) : OCameraView =
        OCameraView (v.sky, v.location, v.forward, v.up, v.right)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create (current : State) (next : State) (msg : OMessage) =
        let f1 = current.annotations.flat
        let f2 = next.annotations.flat
        let c1 = HMap.count f1
        let c2 = HMap.count f2

        match msg with
            | ImportAnnotationGroups _
            | DrawingMessage (Load _) ->
                LoadAnnotations
            | _ ->
                if c2 > c1 then
                    AddAnnotation                   // One or more annotations were added
                else if c2 < c1 then
                    DeleteAnnotation                // One or more annotations were deleted
                else
                    if f1 <> f2 then
                        EditAnnotation              // The map of annotations changed, i.e. at least one annotation was edited
                    else
                        if current <> next then
                            GroupAction             // The annotations remained unchanged, so the grouping must have changed
                        else
                            Unknown                 // No relevant changes detected

    // Returns if two messages of the given type can be coalesced; i.e. can
    // an existing node with the same message be modified instead of adding a new node
    let coalesce = function
        | AddAnnotation -> false
        | _ -> true

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OMessage =

    let isRelevant = function
        | DrawingMessage _
        | AnnotationMessage _ 
        | AnnotationGroupsMessageViewer _ 
        | ImportAnnotationGroups _ -> true
        | _ -> false

    let isCamera = function
        | SetCamera _
        | SetCameraAndFrustum _
        | SetCameraAndFrustum2 _
        | NavigationMessage _ -> true
        | _ -> false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Segment =

    let create (s : OSegment) = { 
        startPoint = s.startPoint
        endPoint = s.endPoint
        points = { inner = s.points }
    }

    let restore (s : Segment) : OSegment = {
        startPoint = s.startPoint
        endPoint = s.endPoint
        points = s.points.inner
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =

    let create (a : OAnnotation) : Annotation = {
        key         = a.key
        modelTrafo  = a.modelTrafo
        geometry    = a.geometry
        projection  = a.projection
        semantic    = a.semantic
        points      = { inner = a.points }
        segments    = { inner = a.segments |> PList.map Segment.create }
        color       = a.color
        thickness   = a.thickness
        results     = a.results
        dnsResults  = a.dnsResults
        visible     = a.visible
        showDns     = a.showDns
        text        = a.text
        textsize    = a.textsize
        surfaceName = a.surfaceName
        view        = a.view |> CameraView.create
    }

    let restore (a : Annotation) : OAnnotation = {
        key         = a.key
        modelTrafo  = a.modelTrafo
        geometry    = a.geometry
        projection  = a.projection
        semantic    = a.semantic
        points      = a.points.inner
        segments    = a.segments.inner |> PList.map Segment.restore
        color       = a.color
        thickness   = a.thickness
        results     = a.results
        dnsResults  = a.dnsResults
        visible     = a.visible
        showDns     = a.showDns
        text        = a.text
        textsize    = a.textsize
        surfaceName = a.surfaceName
        view        = a.view |> CameraView.restore
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationGroup =

    let rec create (a : OAnnotations) (g : OAnnotationGroup) =
        let s = {
            key         = g.key
            name        = g.name
            leaves      = { inner = g.leaves }
            subNodes    = { inner = g.subNodes |> PList.map (create a) }
            visible     = g.visible
        }
        
        {
            state = s
            expanded = g.expanded
        }

    let rec restore (g : AnnotationGroup) : OAnnotationGroup = {
        key         = g.state.key
        name        = g.state.name
        leaves      = g.state.leaves.inner
        subNodes    = g.state.subNodes.inner |> PList.map restore
        visible     = g.state.visible
        expanded    = g.expanded
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotations =

    let create (a : OAnnotations) =
        let getAnnotation = function
            | Annotations a -> Annotation.create a
            | _ -> raise (ArgumentException ("Expected an annotation"))
    
        {
            flat = a.flat |> HMap.map (fun _ l -> getAnnotation l)
            root = a.rootGroup |> AnnotationGroup.create a
        }
        

    let restore (current : OAnnotations) (a : Annotations) : OAnnotations =

        let checkSelection (contains : Guid -> 'a -> bool) (collection : 'a) (s : TreeSelection) =
            if contains s.id collection then s else Group.initTreeSelection
            
        let groups =
            let rec get (g : AnnotationGroup) : Guid hset =  
                g.state.subNodes.inner
                    |> PList.map get
                    |> PList.toList
                    |> fun xs -> HSet.ofList [g.state.key] :: xs
                    |> HSet.unionMany
        
            get a.root

        {
            current with rootGroup = AnnotationGroup.restore a.root
                         flat = a.flat |> HMap.map (fun _ a -> a |> Annotation.restore |> Annotations)
                         singleSelectLeaf = current.singleSelectLeaf |> Option.filter (fun x -> a.flat |> HMap.containsKey x)

                         // TODO: The following checks are not really sufficient, since we would
                         // also have to check the validity of the path; these checks are better implemented in the
                         // groups app anyway, so I won't bother too much with it here.
                         selectedLeaves = current.selectedLeaves |> HSet.filter (fun x -> a.flat |> HMap.containsKey x.id)
                         activeChild = current.activeChild |> (checkSelection HMap.containsKey a.flat)
                         activeGroup = current.activeGroup |> (checkSelection HSet.contains groups)
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : OState) =
        { annotations = Annotations.create s.drawing.annotations }

    let restore (current : OState) (s : State) =
        { current with drawing = { current.drawing with annotations = Annotations.restore current.drawing.annotations s.annotations } }