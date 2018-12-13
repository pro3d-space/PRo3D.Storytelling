namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis
open Aardvark.Base.Rendering
open Aardvark.Base.CameraView
type NavigationMode = FreeFly = 0 | ArcBall = 1
type InteractionMode = PickOrbitCenter = 0 | Draw = 1 | PlaceObject = 2

type Planet = 
    | Earth = 0
    | Mars  = 1
    | None  = 2

module List =
    let rec updateIf (p : 'a -> bool) (f : 'a -> 'a) (xs : list<'a>) = 
        match xs with
            | x :: xs ->
                if(p x) then (f x) :: updateIf p f xs
                else x :: updateIf p f xs
            | [] -> []

    let addWithoutDup e f list =
        match List.exists f list with
            | true -> list
            | false -> e :: list

module PList =
    let append' (a : plist<_>) (b : plist<_>) =
        let rec doIt xs =
            match xs with
                | x::xs -> PList.prepend x (doIt xs)
                | [] -> b
        doIt (PList.toList a)

    let tryHead (a: plist<_>) =
        a |> PList.tryAt 0

    let rev (a: plist<_>) =
        a |> PList.toList |> List.rev |> PList.ofList

    let applyNonEmpty (func : plist<_> -> plist<_>) (a : plist<_>) =
        if PList.count a > 0 then (a |> func) else a

    let remove' (v : 'a) (list: plist<'a>) : plist<'a> =
        list |> PList.filter(fun x -> x <> v)

module Option = 
    let fromBool v b =
        match b with 
          | true  -> Some v
          | false -> None

type SphericalCoo = {
        latitude  : double
        longitude : double
        altitude  : double
        radian    : double
    }

type Points = list<V3d>

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

[<DomainType>]
type Statistics = {
  average      : float
  min          : float
  max          : float
  stdev        : float
  sumOfSquares : float
}

module Statistics = 
  let init = {
    average      = Double.NaN
    min          = Double.NaN
    max          = Double.NaN
    stdev        = Double.NaN
    sumOfSquares = Double.NaN
  }

[<DomainType>]
type FalseColorsModel = {
    useFalseColors  : bool
    lowerBound      : NumericInput
    upperBound      : NumericInput
    interval        : NumericInput
    invertMapping   : bool
    lowerColor      : ColorInput //C4b
    upperColor      : ColorInput //C4b
}

type FalseColorsShaderParams = {
    hsvStart : V3d //(h, s, v)
    hsvEnd   : V3d //(h, s, v)
    interval : float
    inverted : bool
    lowerBound : float
    upperBound : float
    stepS     : float
    numOfRG  : float
    //rParams  : V4d // (lb, ub, step, numOfRG)
}


module FalseColorsModel = 
    let dnSInterv  = {
        value   = 5.0
        min     = 0.0
        max     = 90.0
        step    = 1.0
        format  = "{000:0}"
    } 
    let initMinAngle = {
        value   = 0.0
        min     = 0.0
        max     = 90.0
        step    = 1.0
        format  = "{0:0.0}"
    }
    let initMaxAngle = {
        value   = 90.0
        min     = 1.0
        max     = 90.0
        step    = 1.0
        format  = "{0:0.0}"
    }

    let initDnSLegend = 
        {
            useFalseColors  = false
            lowerBound      = initMinAngle
            upperBound      = initMaxAngle
            interval        = dnSInterv
            invertMapping   = false
            lowerColor      = { c = C4b.Blue }
            upperColor      = { c = C4b.Red }
        }

   
    let scalarsInterv  = {
        value   = 5.0
        min     = 0.0
        max     = 90.0
        step    = 1.0
        format  = "{000:0}"
    } 
    let initlb (range: Range1d) = {
        value   = range.Min
        min     = range.Min
        max     = range.Max
        step    = 1.0
        format  = "{0:0.0}"
    }
    let initub (range: Range1d) = {
        value   = range.Max
        min     = range.Min
        max     = range.Max
        step    = 1.0
        format  = "{0:0.0}"
    }
    let initDefinedScalarsLegend (range: Range1d) = 
        {
            useFalseColors  = false
            lowerBound      = initlb range
            upperBound      = initub range 
            interval        = scalarsInterv 
            invertMapping   = false
            lowerColor      = { c = C4b.Blue }
            upperColor      = { c = C4b.Red }
        }

    let initShaderParams = 
        {
            hsvStart = V3d(0.0, 0.0, 0.0) 
            hsvEnd   = V3d(0.0, 0.0, 0.0) 
            interval = 1.0
            inverted = false
            lowerBound = 0.0
            upperBound = 1.0
            stepS     = 1.0
            numOfRG  = 1.0
        }

[<DomainType>]
type OrientationCubeModel =
  {
    camera  : CameraControllerState
  }

[<DomainType>]
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

[<DomainType>]
type AnnotationResults = {
    height      : float
    heightDelta : float
    avgAltitude : float
    length      : float
    wayLength   : float
}

[<DomainType>]
type Segment = {
    startPoint : V3d
    endPoint   : V3d
    
    points : plist<V3d> 
}

[<DomainType>]
type Annotation = {
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
}


type Style = {
    color : C4b
    thickness : NumericInput
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
    open Aardvark.Base.IndexedGeometryPrimitives

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
        }

    let initial =
        make Projection.Viewpoint Geometry.Polyline { c = C4b.Magenta } thickn ""

module JsonTypes =
    type _V3d = {
        X : double
        Y : double
        Z : double
    }

    type _Points = list<_V3d>

    type _Segment = list<_V3d>

    type _Annotation = {       
        semantic : string
        geometry : _Points 
        segments : list<_Segment>
        color : string
        thickness : double        
        projection : string
        elevation : double
        distance : double
        azimuth : double
        angle   : double
        surfid  : string
    }

    let ofV3d (v:V3d) : _V3d = { X = v.X; Y = v.Y; Z = v.Z }

    let ofPolygon (p:Points) : _Points = p  |> List.map ofV3d

    let ofSegment (s:Segment) : _Segment = s.points  |> PList.map ofV3d 
                                                     |> PList.toList

    let ofSegment1 (s:plist<V3d>) : _Segment = s  |> PList.map ofV3d
                                                  |> PList.toList


    let rec fold f s xs =
        match xs with
            | x::xs -> 
                    let r = fold f s xs
                    f x r
            | [] -> s

    let sum = [ 1 .. 10 ] |> List.fold (fun s e -> s * e) 1

    let sumDistance (polyline : Points) : double =
        polyline  |> List.pairwise |> List.fold (fun s (a,b) -> s + (b - a).LengthSquared) 0.0 |> Math.Sqrt

    let ofAnnotation (a:Annotation) : _Annotation =
        let polygon = ofPolygon (a.points |> PList.toList)
        let avgHeight = (polygon |> List.map (fun v -> v.Z ) |> List.sum) / double polygon.Length
        let distance = sumDistance (a.points |> PList.toList)
        let angle, azimuth =
            match a.dnsResults with
                | Some dns -> 
                    dns.dipAngle, dns.dipAzimuth
                | None -> -1.0, -1.0

        {            
            semantic = a.semantic.ToString()
            geometry = polygon
            segments = a.segments |> PList.map (fun x -> ofSegment1 x.points) |> PList.toList //|> List.map (fun x -> ofSegment x)
            color = a.color.ToString()
            thickness = a.thickness.value
            
            projection = a.projection.ToString()
            elevation = avgHeight
            distance = distance

            azimuth = azimuth
            angle = angle
            surfid = a.surfaceName
        }  

    //let ofDrawing (m : Drawing) : list<_Annotation> =
    //    m.finished.AsList |> List.map ofAnnotation

[<DomainType>]
type PathProxy = {
    absolutePath : option<string>
    relativePath : option<string>
}

[<DomainType>]
type MeasurementsImporterModel = {
    annotations : plist<Annotation>
}

type SurfaceTrafo = {
    id : string
    trafo: Trafo3d
}

type SurfaceShift = {
    id : string
    shift: float
}

[<DomainType>]
type SurfaceTrafoImporterModel = {
    trafos : plist<SurfaceTrafo>
}

[<DomainType>]
type ViewConfigModel = {
    nearPlane               : NumericInput
    farPlane                : NumericInput
    navigationSensitivity   : NumericInput
    arrowLength             : NumericInput
    arrowThickness          : NumericInput
    dnsPlaneSize            : NumericInput
    lodColoring             : bool
    drawOrientationCube     : bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ViewConfigModel =
    let initNearPlane = {
        value   = 0.1
        min     = 0.01
        max     = 1000.0
        step    = 0.01
        format  = "{0:0.00}"
    }
    let initFarPlane = {
        value   = 50000.0
        min     = 1.0
        max     = 10000000.0
        step    = 10.0
        format  = "{0:0.0}"
    }
    let initNavSens = {
        value   = 2.0
        min     = -1.0
        max     = 8.0
        step    = 0.25
        format  = "{0:0.00}"
    }
    let initArrowLength = {
        value   = 1.0
        min     = 0.00
        max     = 10.0
        step    = 0.01
        format  = "{0:0.00}"
    }
    let initArrowThickness = {
        value   = 2.0
        min     = 0.0
        max     = 10.0
        step    = 0.1
        format  = "{0:0.00}"
    }
    let initPlaneSize = {
        value   = 0.8
        min     = 0.0
        max     = 10.0
        step    = 0.1
        format  = "{0:0.00}"
    }
    
    let initial = {
        nearPlane = initNearPlane
        farPlane = initFarPlane
        navigationSensitivity = initNavSens
        arrowLength = initArrowLength
        arrowThickness = initArrowThickness
        dnsPlaneSize = initPlaneSize
        lodColoring = false
        //planet = Planet.Mars
        drawOrientationCube = false
    }




