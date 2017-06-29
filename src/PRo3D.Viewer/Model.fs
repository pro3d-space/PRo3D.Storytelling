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
type Segment = {
    startPoint : V3d
    endPoint : V3d
    
    points : plist<V3d> 
}

[<DomainType>]
type Annotation = {
    key : Guid

    modelTrafo : Trafo3d

    geometry : Geometry
    projection : Projection
    semantic : Semantic

    points : plist<V3d>
    segments : plist<Segment> 
    color : C4b
    thickness : NumericInput
    dnsResults : Option<DipAndStrikeResults>    

    visible : bool
    text : string
}

type Style = {
    color : C4b
    thickness : NumericInput
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
    let thickness = [1.0; 2.0; 3.0; 4.0; 5.0; 1.0; 1.0]
    let color = [new C4b(241,238,246); new C4b(189,201,225); new C4b(116,169,207); new C4b(43,140,190); new C4b(4,90,141); new C4b(241,163,64); new C4b(153,142,195) ]

    let thickn = {
        value   = 3.0
        min     = 1.0
        max     = 8.0
        step    = 1.0
        format  = "{0:0}"
    }

    let make (projection) (geometry) (semantic) : Annotation  = 
        let thickness = thickness.[int semantic]
        let color = color.[int semantic]
        {
            key = Guid.NewGuid()
            geometry = geometry
            semantic = semantic
            points = plist.Empty
            segments = plist.Empty //[]
            color = color
            thickness = { thickn with value = thickness}
            dnsResults = None            
            projection = projection
            visible = true
            text = ""
            modelTrafo = Trafo3d.Identity
        }

    let initial =
        make Projection.Linear Geometry.Polyline Semantic.Horizon0



            

[<DomainType>]
type MeasurementsImporterModel = {
    annotations : plist<Annotation>
}

[<DomainType>]
type Surface = {    
    
    [<PrimaryKey>]
    guid : System.Guid
    
    name : string
    importPath : string
    opcPaths : list<string>
    
    fillMode  : FillMode
    cullMode  : CullMode
    isVisible : bool
}

[<DomainType>]
type OrientationDisplayModel = {    

    center : Option<V3d>
    northVector : Option<V3d>
    upVector : Option<V3d>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrientationDisplayModel =
    let initial = {
        center = Some V3d.OOO
        northVector = Some V3d.IOO
        upVector = Some V3d.OOI
        }

        
[<DomainType>]
type NavigationModel = {
    camera : CameraControllerState
    navigationMode : NavigationMode  
    coordinates : OrientationDisplayModel
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NavigationModel =
    let initial = {
        camera = CameraController.initial  
        navigationMode =  NavigationMode.FreeFly
        coordinates = OrientationDisplayModel.initial
        }