namespace View

open Aardvark.Base.Incremental

open Abstraction.Camera
open Abstraction.Presentation
open Animation

[<DomainType>]
type ViewParams = {
    camera : CameraView
    presentation : PresentationParams
}

[<DomainType>]
type View = {
    state : ViewParams
    animation : Animation
}

type ViewAction =
    | Set
    | Move
    | Preview
    | Animation     of AnimationAction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module View =

    let camera (view : View) =
        Animation.camera view.animation