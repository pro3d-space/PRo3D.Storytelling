namespace Aardvark.PRo3d

open System
open Aardvark.Base
open Aardvark.Base.Incremental

module Test =
    let test () =
        let initial =
            {
                lastName = None
                elements = PList.ofList ["A"; "B"; "C"]
                hasD3Hate = true
                boxHovered = false
                boxScale = 1.0
                dragging = false
                objects = HMap.empty
            }
        let mModel = MModel.Create initial
        Model.Lens.boxHovered |> ignore

        0