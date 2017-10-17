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

//instrument shot 

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