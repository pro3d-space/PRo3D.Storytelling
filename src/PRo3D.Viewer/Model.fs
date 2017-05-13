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


[<DomainType>]
type Model = { 

    cameraModel : CameraControllerState

    importOpcs : list<string>

    surfaces   : hset<ISg>
}

