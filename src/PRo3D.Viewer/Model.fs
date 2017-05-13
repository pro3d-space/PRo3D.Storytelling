namespace KitchenSink

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

[<DomainType>]
type Model = { 
    value : float 
    cameraModel : CameraControllerState
}

