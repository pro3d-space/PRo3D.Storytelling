namespace PRo3D.Drawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.Opc
open Aardvark.SceneGraph
open Aardvark.VRVis

open PRo3D

type Action =
    | SetSemantic      of Semantic
    | SetGeometry      of Geometry
    | SetProjection    of Projection
    | SetExportPath    of string
    | Move             of V3d
    | Exit    
    | AddPoint         of V3d
    | AddPointAdv      of V3d * (V3d -> Option<V3d>)
    | SetSegment       of int * list<V3d>
    | KeyDown          of key : Aardvark.Application.Keys
    | KeyUp            of key : Aardvark.Application.Keys      
    | Export
    | Save              
    | Load             
    | Clear
    | Select           of Guid


[<DomainType>]
type DrawingModel = {

    draw    : bool 
    hoverPosition : option<Trafo3d>    

    working : Option<Annotation>
    projection : Projection
    geometry : Geometry
    semantic : Semantic

    annotations : plist<Annotation>
    exportPath : string

    selected : hset<Guid>
    selectedAnnotation : option<Annotation>

    pendingIntersections : ThreadPool<Action>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]    
module Model =
    let initialdrawing = {
        hoverPosition = None
        draw = false                    

        working = None
        projection = Projection.Viewpoint
        geometry = Geometry.Polyline
        semantic = Semantic.Horizon3

        annotations = PList.empty

        exportPath = @"."

        selected = HSet.empty
        selectedAnnotation = None
        pendingIntersections = ThreadPool.empty
    }




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]    
module DrawingModel =
    let tryGet (m:DrawingModel) key = 
            m.annotations |> Seq.tryFind(fun x -> x.key = key)

    let update (m : DrawingModel) (ann : Annotation) = 
        m.annotations.AsList 
            |> List.updateIf (fun x -> x.key = ann.key) (fun x -> ann) 
            |> PList.ofList