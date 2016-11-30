namespace PRo3D.Gui

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Core.JsInterop


open Helpers


module Startup =
    let contentNode = "#app"

    #if DEV_HMR

    type IModule =
        abstract hot: obj with get, set

    let [<Global>] [<Emit("module")>] Module : IModule = failwith "JS only"

    let node = document.querySelector contentNode

    if isNotNull Module.hot then
        Module.hot?accept() |> ignore

        Module.hot?dispose(fun _ ->
        node.removeChild(node.firstChild) |> ignore
        ) |> ignore
    #endif

    Main.start contentNode () |> ignore
