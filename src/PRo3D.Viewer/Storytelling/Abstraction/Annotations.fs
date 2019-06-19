namespace Provenance.Abstraction.Annotations

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open PRo3D
open PRo3D.Versioned
open PRo3D.Groups

open Provenance.Abstraction.RList
open Provenance.Abstraction.Camera

type OSegment = Segment

[<DomainType>]
type Segment = {
    startPoint  : V3d
    endPoint    : V3d
    points      : V3d rlist 
}

type OAnnotation = Annotation'

[<DomainType>]
type Annotation = {
    version     : int

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
    results     : AnnotationResults' option
    dnsResults  : DipAndStrikeResults' option

    visible     : bool
    showDns     : bool
    text        : string
    textsize    : NumericInput

    surfaceName : string
    view        : CameraView

    semanticId   : SemanticId
    semanticType : SemanticType
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
        version      = a.version
        key          = a.key
        modelTrafo   = a.modelTrafo
        geometry     = a.geometry
        projection   = a.projection
        semantic     = a.semantic
        points       = { inner = a.points }
        segments     = { inner = a.segments |> PList.map Segment.create }
        color        = a.color
        thickness    = a.thickness
        results      = a.results
        dnsResults   = a.dnsResults
        visible      = a.visible
        showDns      = a.showDns
        text         = a.text
        textsize     = a.textsize
        surfaceName  = a.surfaceName
        view         = a.view |> CameraView.create
        semanticId   = a.semanticId
        semanticType = a.semanticType
    }

    let restore (a : Annotation) : OAnnotation = {
        version      = a.version
        key          = a.key
        modelTrafo   = a.modelTrafo
        geometry     = a.geometry
        projection   = a.projection
        semantic     = a.semantic
        points       = a.points.inner
        segments     = a.segments.inner |> PList.map Segment.restore
        color        = a.color
        thickness    = a.thickness
        results      = a.results
        dnsResults   = a.dnsResults
        visible      = a.visible
        showDns      = a.showDns
        text         = a.text
        textsize     = a.textsize
        surfaceName  = a.surfaceName
        view         = a.view |> CameraView.restore
        semanticId   = a.semanticId
        semanticType = a.semanticType
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
            | Annotations' a -> Annotation.create a
            | _ -> raise (ArgumentException ("Expected an annotation"))
    
        {
            flat = a.flat |> HMap.map (fun _ l -> getAnnotation l)
            root = a.rootGroup |> AnnotationGroup.create a
        }
        
    let restore (current : OAnnotations) (a : Annotations) : OAnnotations =

        let checkSelection (contains : Guid -> 'a -> bool) (collection : 'a) (def : TreeSelection) (s : TreeSelection) =
            if contains s.id collection then s else def
            
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
                         flat = a.flat |> HMap.map (fun _ a -> a |> Annotation.restore |> Annotations')
                         singleSelectLeaf = current.singleSelectLeaf |> Option.filter (fun x -> a.flat |> HMap.containsKey x)

                         // TODO: The following checks are not really sufficient, since we would
                         // also have to check the validity of the path; these checks are better implemented in the
                         // groups app anyway, so I won't bother too much with it here.
                         selectedLeaves = current.selectedLeaves |> HSet.filter (fun x -> a.flat |> HMap.containsKey x.id)
                         activeChild = current.activeChild |> (checkSelection HMap.containsKey a.flat Group.initChildSelection)
                         activeGroup = current.activeGroup |> (checkSelection HSet.contains groups Group.initGroupSelection)
        }