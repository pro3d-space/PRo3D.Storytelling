namespace Provenance.Reduced

open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D.Viewer

open Model

type OCameraView = CameraView

// TODO: We need to redefine this because we want structural equality but CameraView is a class :/
[<DomainType>]
type CameraView = {
    sky : V3d
    location : V3d
    forward : V3d
    up : V3d
    right : V3d
}

type OMessage = AppAction

[<DomainType>]
type Message =
    | Unknown 

    override x.ToString () =
        match x with
            | Unknown -> ""

type OState = AppModel

[<DomainType>]
type State = {
    nil : int
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CameraView =
    
    let create (v : OCameraView) =
        { sky = v.Sky
          location = v.Location
          forward = v.Forward
          up = v.Up
          right = v.Right }

    let restore (v : CameraView) =
        OCameraView (v.sky, v.location, v.forward, v.up, v.right)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create (current : State) (next : State) (msg : OMessage) =
        Unknown

    let isCamera = function
        | SetCamera _
        | SetCameraAndFrustum _
        | SetCameraAndFrustum2 _
        | NavigationMessage _ -> true
        | _ -> false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : OState) =
        { nil = 0 }

    let restore (current : OState) (s : State) =
        current