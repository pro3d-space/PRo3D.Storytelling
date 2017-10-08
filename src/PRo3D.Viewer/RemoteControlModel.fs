namespace RemoteControlModel

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

type RemoteAction = 
    | SetViewpoint of CameraView

type Action = 
    | MakeScreenshot of CameraView
    | UpdateCamera of CameraView

[<DomainType>]
type Model =
    {
        viewPoints : plist<CameraView>
    }