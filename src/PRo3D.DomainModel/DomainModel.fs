namespace Aardvark.PRo3d

open System
open Aardvark.Base
open Aardvark.Base.Incremental

[<DomainType>]
type Urdar = { urdar : int }

[<DomainType>]
type Model =
    {
        boxHovered      : bool
        dragging        : bool
        lastName        : Option<string>
        elements        : plist<string>
        hasD3Hate       : bool
        boxScale        : float
        objects         : hmap<string,Urdar>
    }

[<DomainType>]
type CameraControllerState =
    {
        view : CameraView

        dragStart : V2i
        look : bool
        zoom : bool
        pan : bool

        forward : bool
        backward : bool
        left : bool
        right : bool
        moveVec : V3i


        lastTime : Option<float>

        [<TreatAsValue>]
        stash : Option<CameraControllerState> 

    }
