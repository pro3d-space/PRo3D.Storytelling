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

type NavigationMode = FreeFly = 0 | ArcBall = 1
type InteractionMode = PickOrbitCenter = 0 | Draw = 1 | PlaceObject = 2

[<DomainType>]
type Surface = {    
    
    [<PrimaryKey>]
    guid : System.Guid
    
    name : string
    importPath : string
    opcPaths : list<string>

    globalBB : Box3d   
    sceneGraph : ISg
}

module Surface =     

    let mk name opcs = {
        guid = Guid.NewGuid()
        name = name
        importPath = String.Empty
        opcPaths = opcs

        globalBB = Box3d.Invalid
        sceneGraph = Sg.group []
    }

    let loaded s sg bb = {
        s with sceneGraph = sg; globalBB = bb
    }

[<DomainType>] 
type Scene = {
    cameraModel : CameraControllerState    
    interaction : InteractionMode
    surfaces : list<Surface>
}

[<DomainType>]
type Model = { 
   
    scene : Scene
    surfaces   : hset<Surface>        
}

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