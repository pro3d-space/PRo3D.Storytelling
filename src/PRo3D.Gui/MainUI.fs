namespace PRo3D.Gui

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.App
open Fable.Helpers.Virtualdom.Html

open System
open Helpers

type RoseTree<'a,'b> =
    | Node of 'b * list<RoseTree<'a,'b>>
    | Leaf of 'a


module RoseTreeOps =
    let rec map f t =
        match t with
            | Leaf l -> Leaf (f l)
            | Node(l,xs) -> Node(f l,xs |> List.map (map f))

    let rec mkTree f n =
        if n <= 0 then Leaf (f n)
        else Node(f n, [mkTree f (n-1); mkTree f (n-1)])

module TreeViewOps =
    
    let fromTree t =
        let rec fromTree t =
            match t with
                | Leaf(label) -> li [] [span [attribute "class" "leaf"] [label]]
                | Node(label,children) ->
                    li [attribute "class" "node"]
                       [
                         span [attribute "class" "leaf"] [label]
                         span [attribute "class" "node-toggle"] []
                         ul [] (children |> List.map fromTree)
                       ]
        div [ attribute "class" "treeview"; attribute "data-role" "treeview" ] 
            [ ul [] [ fromTree t ] ]

                

module Main =

  type Model = int

  let initial = 2

//  let viewsurface
//  let viewSurfaces (surfaces list) =
//    surfaces |> List.map viewSurfance
//  let viewScene (scene : Scene) = div [ ... viewSurfaces scene.surfaces ]

  // Actions supported by the application
  type Actions =
    | Inc 
    | OpenSettings 

  let update (model: Model) action =
    match action with
        | Inc -> model+1, []
        | _ -> model, []

//    let model', action' =
//      match action with
//      | ChangeInput s ->
//        { model with Input = s } , []
//
//    #if DEV_HMR
//    // Update the model in storage
//    window?storage <- model'
//    #endif
//
//    model', action'

  /// Custom binding for inInput. This directly give the value
  let inline onInput x = onEvent "oninput" (fun e -> x (unbox e?target?value))
  /// View
//  let view model =
//    div
//      []
//      [
//        label
//          []
//          [text "Enter aaaaaasdfasdf aaaurgeil abc: "]
//        input
//          [ onInput ChangeInput ]
//        br []
//        span
//          []
//          [text (sprintf "Hello %s" model.Input)]
//      ]
  let view model =
    div [] [
        button [ onMouseClick (fun _ -> Inc) ] [ text "abc" ]
        model 
            |> RoseTreeOps.mkTree string 
            |> RoseTreeOps.map text 
            |> TreeViewOps.fromTree
    ]

  let start node () =
    createApp initial view update
    |> withStartNodeSelector node
    |> start renderer
