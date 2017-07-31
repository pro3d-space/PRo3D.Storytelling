namespace PRo3D

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
open Aardvark.Base.Rendering

//open Aardvark.Data.PhotoNetwork

type NavigationMode = FreeFly = 0 | ArcBall = 1
type InteractionMode = PickOrbitCenter = 0 | Draw = 1 | PlaceObject = 2

module List =
    let rec updateIf (p : 'a -> bool) (f : 'a -> 'a) (xs : list<'a>) = 
        match xs with
            | x :: xs ->
                if(p x) then (f x) :: updateIf p f xs
                else x :: updateIf p f xs
            | [] -> []

module OpcUtils =
    
    /// checks if "path" is a valid opc folder
    let isOPCPath (path : string) = 
        let imagePath = Path.combine [path; "images"]
        let patchPath = Path.combine [path; "patches"]
        let dirsExist = (System.IO.Directory.Exists imagePath) && (System.IO.Directory.Exists patchPath)
        let storageExists = System.IO.File.Exists(patchPath + "\\patchhierarchy.xml")
        if dirsExist && storageExists then
            true
        else false

    /// checks if "path" is a valid surface folder
    let isSurfaceFolder (path : string) =
        let mutable isValid = false
        let subfolders = System.IO.Directory.GetDirectories(path)
        for s in subfolders do
            isValid <- (isOPCPath(s) || isValid)
        isValid
            
        
    /// returns all valid OPCs in "path"
    let getOPCPaths path = 
        if path = String.Empty then failwith "encountered empty opcpath"

        if isOPCPath path then
           [path.Replace(@"\", @"\\\\")]
        elif System.IO.Directory.Exists path then
            System.IO.Directory.GetDirectories(path)
                |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                |> Seq.choose (fun a -> if isOPCPath a then Some a else None)
                |> Seq.toList 
        else []

    /// returns all valid surface folders in "path"   
    let discover path = 
        if System.IO.Directory.Exists path then
            System.IO.Directory.EnumerateDirectories path 
               // |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                |> Seq.filter (fun a -> isSurfaceFolder a)
                |> Seq.toList 
        else []

type Points = list<V3d>

type Projection = Linear = 0 | Viewpoint = 1 | Sky = 2
type Geometry = Point = 0 | Line = 1 | Polyline = 2 | Polygon = 3 | DnS = 4
type Semantic = Horizon0 = 0 | Horizon1 = 1 | Horizon2 = 2 | Horizon3 = 3 | Horizon4 = 4 | Crossbed = 5 | GrainSize = 6

[<DomainType>]
type DipAndStrikeResults = {

    plane : Plane3d

    dipAngle : float
    dipDirection : V3d
    strikeDirection : V3d
    dipAzimuth : float
    strikeAzimuth : float

    centerOfMass : V3d
}

[<DomainType>]
type AnnotationResults = {

    height : float
    heightDelta : float
    length : float
    wayLength : float
}
[<DomainType>]
type Segment = {
    startPoint : V3d
    endPoint : V3d
    
    points : plist<V3d> 
}

[<DomainType>]
type Annotation = {
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

    
    let getHeightDelta (p:V3d) (upVec:V3d) = (p * upVec).Length

    let calcResultsPoint (point:V3d) (trafo:Trafo3d) (upVec:V3d) =
        let p = point //trafo.Forward.TransformPos(point)
        {   height = getHeightDelta p upVec
            heightDelta = 0.0
            length = 0.0
            wayLength = 0.0
            }

    let getDistance (points:list<V3d>) = points
                                            |> List.pairwise
                                            |> List.map (fun (a,b) -> V3d.Distance(a,b))
                                            |> List.sum

    let getSegmentDistance (s:Segment) = getDistance
                                            [ yield s.startPoint ; 
                                               for p in s.points do
                                                    yield p; 
                                              yield s.endPoint 
                                            ] 
   
    let getWay (segments:plist<Segment>) = [ for s in segments do
                                                yield getSegmentDistance s
                                           ] |> List.sum
                                                               

    let calcResultsLine (model:Annotation) (upVec:V3d) =
        let count = model.points.Count
        let dist = V3d.Distance(model.points.[0], model.points.[count-1]);
        let way = match model.segments.Count with
                        | x when x > 0 -> getWay model.segments
                        |_ -> dist

        let heights = model.points 
                       // |> PList.map(fun x -> model.modelTrafo.Forward.TransformPos(x))
                        |> PList.map(fun p -> getHeightDelta p upVec) 
                        |> PList.toList

        let hcount = heights.Length

        {   height = heights |> List.max
            heightDelta = Fun.Abs (heights.[hcount-1] - heights.[0])
            length = dist
            wayLength = way
            }

    let calculateAnnotationResults (model:Annotation) (upVec:V3d) =
        match model.points.Count with
            | x when x > 1 -> calcResultsLine model upVec
            | _ -> calcResultsPoint model.points.[0] model.modelTrafo upVec

    let make (projection) (geometry) (semantic) (surfName) : Annotation  = 
        let thickness = thickness.[int semantic]
        let color = color.[int semantic]
        {
            key = Guid.NewGuid()
            geometry = geometry
            semantic = semantic
            points = plist.Empty
            segments = plist.Empty //[]
            color = { c = color }
            thickness = { thickn with value = thickness}
            results = None
            dnsResults = None            
            projection = projection
            visible = true
            text = ""
            modelTrafo = Trafo3d.Identity
            showDns = true
            surfaceName = surfName
        }

    let initial =
        make Projection.Linear Geometry.Polyline Semantic.Horizon0 ""

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

[<DomainType>]
type Surface = {    
    
    [<PrimaryKey>]
    guid        : System.Guid
    
    name        : string
    importPath  : string
    opcPaths    : list<string>
    
    fillMode    : FillMode
    cullMode    : CullMode
    isVisible   : bool
    quality     : NumericInput
}


[<DomainType>]
type ViewConfigModel = {
    nearPlane               : NumericInput
    farPlane                : NumericInput
    navigationSensitivity   : NumericInput
    arrowLength             : NumericInput
    arrowThickness          : NumericInput
    dnsPlaneSize            : NumericInput
    minDipAngle             : NumericInput
    maxDipAngle             : NumericInput
    lodColoring             : bool
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
        value   = 5000.0
        min     = 1.0
        max     = 10000000.0
        step    = 10.0
        format  = "{0:0.0}"
    }
    let initNavSens = {
        value   = 0.0
        min     = -8.0
        max     = +8.0
        step    = 0.25
        format  = "{0:0.00}"
    }
    let initArrowLength = {
        value   = 0.5
        min     = 0.00
        max     = 10.0
        step    = 0.01
        format  = "{0:0.00}"
    }
    let initArrowThickness = {
        value   = 1.0
        min     = 0.0
        max     = 10.0
        step    = 0.1
        format  = "{0:0.00}"
    }
    let initPlaneSize = {
        value   = 0.2
        min     = 0.0
        max     = 10.0
        step    = 0.1
        format  = "{0:0.00}"
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
    let initial = {
        nearPlane = initNearPlane
        farPlane = initFarPlane
        navigationSensitivity = initNavSens
        arrowLength = initArrowLength
        arrowThickness = initArrowThickness
        dnsPlaneSize = initPlaneSize
        minDipAngle = initMinAngle
        maxDipAngle = initMaxAngle
        lodColoring = false
    }
      
[<DomainType>]
type ReferenceSystem = {    

    origin      : V3d
    north       : V3d
    up          : V3dInput
    isVisible   : bool
    config      : ViewConfigModel
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReferenceSystem =
    let initNum = {
        value   = 0.0
        min     = -1.0
        max     = +1.0
        step    = 0.1
        format  = "{0:0.000}"
    }

    let setV3d (v : V3d) = {
        x = { initNum with value = v.X }
        y = { initNum with value = v.Y }
        z = { initNum with value = v.Z }
        value = v    
    }

    let initArrowSize = {
        value   = 1.0
        min     = 0.1
        max     = 10.0
        step    = 0.1
        format  = "{0:0.0}"
    }

    let initial = {
        origin = V3d.OOO
        north = V3d.IOO
        up = setV3d V3d.OOI
        isVisible = true
        config = ViewConfigModel.initial
        }   
        
[<DomainType>]
type NavigationModel = {
    camera : CameraControllerState
    navigationMode : NavigationMode  
    coordinates : ReferenceSystem
    exploreCenter : V3d
    config : ViewConfigModel
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NavigationModel =
    let initial = {
        camera = CameraController.initial  
        navigationMode =  NavigationMode.FreeFly
        coordinates = ReferenceSystem.initial
        exploreCenter = V3d.Zero // make option
        config = ViewConfigModel.initial
        }


