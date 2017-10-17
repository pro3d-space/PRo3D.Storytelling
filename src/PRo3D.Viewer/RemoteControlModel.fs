namespace RemoteControlModel

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open PRo3D.Viewer
open System.Drawing.Printing
open System.Runtime.Serialization
open PRo3d
open PRo3D



//PTU values sequence
//isntrument model
//instrument pose

[<DataContract>]
type Xyz = {
    [<field: DataMember(Name = "x")>]
    x : float
    [<field: DataMember(Name = "y")>]
    y : float
    [<field: DataMember(Name = "z")>]
    z : float
}


module Xyz =
    let fromV3d (v : V3d) =
        { x = v.X; y = v.Y; z = v.Z }

    let toV3d (v : Xyz) : V3d =
        V3d(v.x, v.y, v.z)
        
[<DataContract>]
type Shot = {    
    [<field: DataMember(Name = "pos")>]
    pos     : Xyz
    [<field: DataMember(Name = "lookAt")>]
    lookAt  : Xyz
    [<field: DataMember(Name = "up")>]
    up      : Xyz
    [<field: DataMember(Name = "col")>]    
    col     : int
    [<field: DataMember(Name = "row")>]
    row     : int
    [<field: DataMember(Name = "pph")>]
    pph     : int
    [<field: DataMember(Name = "ppv")>]
    ppv     : int
    [<field: DataMember(Name = "id")>]
    id      : string
    [<field: DataMember(Name = "folder")>]
    folder  : string
}

[<DataContract>]
type InstrumentPlatform = {    
    [<field: DataMember(Name = "rover")>]
    rover      : string
    [<field: DataMember(Name = "instrument")>]
    instrument : string
    [<field: DataMember(Name = "focal")>]
    focal      : double
    [<field: DataMember(Name = "pan")>] //"Pan Axis"
    pan        : double
    [<field: DataMember(Name = "tilt")>] //"Tilt Axis"
    tilt       : double
}

//instrument shot 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Shot =
    open System

    let init = 
        {
            pos     = V3d.III |> Xyz.fromV3d
            lookAt  = V3d.OOO |> Xyz.fromV3d
            up      = V3d.OOI |> Xyz.fromV3d
            col     = 1024
            row     = 768
            pph     = 0
            ppv     = 0
            id      = Guid.NewGuid().ToString()
            folder  = "./shots"
        }

    let getCamera (s : Shot) : CameraView =
        CameraView.LookAt(s.pos |> Xyz.toV3d, s.lookAt |> Xyz.toV3d, s.up |> Xyz.toV3d)

    

    let fromCamera (cv : CameraView) : Shot =
        { init with 
                pos     = cv.Location |> Xyz.fromV3d
                up      = cv.Up |> Xyz.fromV3d
                lookAt  = (cv.Location + cv.Forward) |> Xyz.fromV3d  }

    let fromWp (wp : WayPoint) : Shot =        
        { fromCamera wp.cv with id = wp.name }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InstrumentPlatform =

    let init = 
        {
            rover      = "ExoMarsRover_VariousCameras"
            instrument = "MastcamZL"
            focal      = 0.0
            pan        = 0.0
            tilt       = 0.0
        }

    let fromRoverModel (rm : RoverModel) : option<InstrumentPlatform> =

        let result = init

        let result =
            rm.selectedRover 
                |> Option.bind (fun r -> 
                    let result' = { result with rover = r.id }
                    let result' = 
                        rm.selectedInstrument |> Option.map(fun x -> { result' with instrument = x.id; focal = x.focal.value })


                    result')
        
        result





[<DomainType>]
type RemoteModel =
    {
        //viewPoints : plist<WayPoint>
        shots         : plist<Shot>
        Rover         : RoverModel
    }

type RemoteAction = 
    | SetViewpoint of CameraView
    

type Action = 
    | MakeScreenshot of Shot
    | UpdateCamera   of Shot
    | Play
    | Load
    | OpenFolder     of string
    | RoverMessage   of RoverApp.Action