namespace PRo3D.Base

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

type Planet = 
  | Earth = 0
  | Mars  = 1
  | None  = 2

type SphericalCoo = {
  longitude : double
  latitude  : double
  altitude  : double
  radian    : double
}


module AnnotationDraft =
  type Projection = Linear = 0 | Viewpoint = 1 | Sky = 2
  type Geometry = Point = 0 | Line = 1 | Polyline = 2 | Polygon = 3 | DnS = 4
  type Semantic = 
    | Horizon0 = 0 
    | Horizon1 = 1 
    | Horizon2 = 2 
    | Horizon3 = 3 
    | Horizon4 = 4 
    | Crossbed = 5 
    | GrainSize = 6 
    | None = 7
  
  type Statistics = {
    average      : float
    min          : float
    max          : float
    stdev        : float
    sumOfSquares : float
  }
  
  
  type DipAndStrikeResults = {
      plane           : Plane3d
      dipAngle        : float
      dipDirection    : V3d
      strikeDirection : V3d
      dipAzimuth      : float
      strikeAzimuth   : float
      centerOfMass    : V3d
      error           : Statistics
  }
  
  type AnnotationResults = {
      height      : float
      heightDelta : float
      avgAltitude : float
      length      : float
      wayLength   : float
      bearing     : float
      slope       : float
  }
  
  type Segment = {
      startPoint : V3d
      endPoint   : V3d
      
      points : plist<V3d> 
  }
  
  type Annotation = {
      [<NonIncremental>]
      version     : int
  
      [<PrimaryKey; NonIncremental>]
      key         : Guid
  
      modelTrafo  : Trafo3d
  
      geometry    : Geometry
      projection  : Projection
      semantic    : Semantic
  
      points      : plist<V3d>
      segments    : plist<Segment> 
      color       : ColorInput
      thickness   : NumericInput
      results     : Option<AnnotationResults>
      dnsResults  : Option<DipAndStrikeResults>
  
      visible     : bool
      showDns     : bool
      text        : string
      textsize    : NumericInput
  
      surfaceName : string
      view        : CameraView
  }
  
  type Style = {
      color : C4b
      thickness : NumericInput
  }
  
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Annotation =
      open Aardvark.Base.IndexedGeometryPrimitives
      open Aardvark.UI.Primitives
  
      let current = 0
      let thickness = [1.0; 2.0; 3.0; 4.0; 5.0; 1.0; 1.0]
      let color = [new C4b(241,238,246); new C4b(189,201,225); new C4b(116,169,207); new C4b(43,140,190); new C4b(4,90,141); new C4b(241,163,64); new C4b(153,142,195) ]
  
      let thickn = {
          value   = 3.0
          min     = 1.0
          max     = 8.0
          step    = 1.0
          format  = "{0:0}"
      }
  
      let texts = {
          value   = 0.05
          min     = 0.01
          max     = 5.0
          step    = 0.01
          format  = "{0:0.00}"
      }
          
      let make (projection) (geometry) (color) (thickness) (surfName) : Annotation  = 
                 
          {
              version     = current
              key         = Guid.NewGuid()
              geometry    = geometry
              semantic    = Semantic.None
              points      = plist.Empty
              segments    = plist.Empty //[]
              color       = color
              thickness   = thickness
              results     = None
              dnsResults  = None            
              projection  = projection
              visible     = true
              text        = ""
              textsize    = texts
              modelTrafo  = Trafo3d.Identity
              showDns     = 
                  match geometry with 
                    | Geometry.DnS -> true 
                    | _ -> false 
              surfaceName = surfName
              view = FreeFlyController.initial.view
          }
  
      let initial =
          make Projection.Viewpoint Geometry.Polyline { c = C4b.Magenta } thickn ""

