namespace RemoteControlModel

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

type RemoteAction = 
    | SetViewpoint of CameraView

type Action = 
    | MakeScreenshot

[<DomainType>]
type Model =
    {
        viewPoints : list<CameraView>
    }