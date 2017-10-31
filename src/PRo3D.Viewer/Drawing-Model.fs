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
open PRo3D.ReferenceSystem

open PRo3D
open PRo3D.Groups

type Action =
        | SetSemantic      of Semantic
        | SetGeometry      of Geometry
        | SetProjection    of Projection
        | SetExportPath    of string
        | Move             of V3d
        | Exit            
        | StartDrawing            
        | StopDrawing        
        | AddPointAdv      of V3d * (V3d -> Option<V3d>) * string
        | SetSegment       of int * Segment
        | Finish                
        | Undo
        | Redo
        | Select                  of list<Index>*Guid      
        | ToggleVisiblity         of Guid
        | RemoveAnnotation        of list<Index>*Guid       
        | Send
        | Export
        | Save              
        | Load             
        | QuickLoad
        | Clear
        | Nop
        | AnnotationPropertiesMsg of AnnotationProperties.Action
        | UpVectorChanged         of V3d
        | NorthVectorChanged      of V3d
        | AnnotationGroupsMessage   of AnnotationGroups.Action

[<DomainType>]
type DrawingModel = {

    draw    : bool 
    hoverPosition : option<Trafo3d>    

    working : Option<Annotation>
    projection : Projection
    geometry : Geometry
    semantic : Semantic

    annotations: AnnotationGroupsModel    
    exportPath : string

    selected : hset<Guid>
    selectedAnnotation : option<Annotation>

    pendingIntersections : ThreadPool<Action>
    //config : ViewConfigModel
  //  coordinates : ReferenceSystem

    [<TreatAsValue>]
    past : Option<DrawingModel> 

    [<TreatAsValue>]
    future : Option<DrawingModel>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]    
module DrawingModel =
    //let tryGet (m:DrawingModel) key = 
    //        m.annotations |> Seq.tryFind(fun x -> x.key = key)

    //let update (m : DrawingModel) (ann : Annotation) = 
    //    m.annotations.AsList 
    //        |> List.updateIf (fun x -> x.key = ann.key) (fun x -> ann) 
    //        |> PList.ofList

    let tryGet (ans:plist<Annotation>) key = 
            ans |> Seq.tryFind(fun x -> x.key = key)

    let tryGet' (ans:hset<Annotation>) key = 
            ans |> Seq.tryFind(fun x -> x.key = key)

    let update (ans:plist<Annotation>) (ann : Annotation) = 
        ans.AsList 
            |> List.updateIf (fun x -> x.key = ann.key) (fun x -> ann) 
            |> PList.ofList

    let initialdrawing = {
        hoverPosition = None
        draw          = false                    

        working     = None
        projection  = Projection.Viewpoint
        geometry    = Geometry.Polyline
        semantic    = Semantic.Horizon3

        annotations = AnnotationGroupsModel.initial

        exportPath  = @"."

        selected                = HSet.empty
        selectedAnnotation      = None
        pendingIntersections    = ThreadPool.empty            

        past    = None
        future  = None
    }