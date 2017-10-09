namespace RemoteControlModel

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open PRo3D.Viewer
open System.Drawing.Printing

type RemoteAction = 
    | SetViewpoint of CameraView

type Action = 
    | MakeScreenshot of CameraView
    | UpdateCamera   of CameraView
    | Play

//PTU values sequence
//isntrument model
//instrument pose

type Xyz = {
    x : float
    y : float
    z : float
}

module Xyz =
    let fromV3d (v : V3d) =
        { x = v.X; y = v.Y; z = v.Z }

    let toV3d (v : Xyz) : V3d =
        V3d(v.x, v.y, v.z)


type Shot = {    
    pos     : Xyz
    lookAt  : Xyz
    up      : Xyz
    col     : int
    row     : int
    pph     : int
    ppv     : int
    id      : string
    folder  : string
}

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
            folder  = @".\shots"
        }

    let getCamera (s : Shot) : CameraView =
        CameraView.LookAt(s.pos |> Xyz.toV3d, s.lookAt |> Xyz.toV3d, s.up |> Xyz.toV3d)

[<DomainType>]
type Model =
    {
        viewPoints : plist<WayPoint>
        shots      : plist<Shot>
    }