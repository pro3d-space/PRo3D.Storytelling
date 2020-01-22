namespace Aardvark.UI

open Aardvark.Base.Incremental
open Aardvark.UI

[<AutoOpen>]
module SvgExtensions =

    [<AutoOpen>]
    module Static =

        module Svg =

            let inline g x = elemNS "g" Svg.svgNS x

            let inline marker x = elemNS "marker" Svg.svgNS x

            let inline foreignObject x = elemNS "foreignObject" Svg.svgNS x

    module Incremental =

        module Svg =

            let inline g x = Incremental.elemNS "g" Svg.svgNS x

[<AutoOpen>]
module HigherOrderTagsExtensions =

    let onBootInitial (name : string) (input : IMod<'a>) (code : string) (node : DomNode<'msg>) =
        let init = code.Replace ("__DATA__", input |> Mod.force |> Pickler.jsonToString)
        let update = sprintf "%s.onmessage = function (data) { %s }" name (code.Replace ("__DATA__", "data"))

        onBoot init (
            onBoot' [name, input |> Mod.channel] update node
        )

    // We don't want buttons to trigger other click events
    let disableClickPropagation (node : DomNode<'msg>) =
        onBoot "$('#__ID__').on('click', function (event) { event.stopPropagation(); });" node

    let disableBlurAndClickPropagation (node : DomNode<'msg>) =
        let blur = "$('#__ID__').on('mousedown', function (e) { e.preventDefault(); });"
        let propagation = "$('#__ID__').on('click', function(e) { e.stopPropagation(); } );"

        onBoot (blur + propagation) node