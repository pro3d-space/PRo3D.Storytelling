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
  open TestModel 

  // Actions supported by the application
  type Actions = int    

  module Tree =
    let treeView attribs children = 
        let attribs = attribs @ [ attribute "class" "treeview"; attribute "data-role" "treeview" ]
        div attribs  [ ul [] children ]

    let leaf attribs children = li attribs [span [attribute "class" "leaf"] children]
    let node attribs label (collapsed : bool) children  =
        let nodeClass = if collapsed then "node collapsed" else "node"
        let attribs = attribs @ [attribute "class" "leaf"]
        li [attribute "class" nodeClass]
                [
                    span attribs [label]
                    span [attribute "class" "node-toggle"] []
                    ul [] children
                ]

  let update (model: Model) (action : Actions) = model, []

  let viewOpc (opc: Opc) = 
    Tree.leaf [] [text opc.Name]

  let viewOpcs opcs = 
    opcs |> List.map viewOpc

  let viewSurface (surf: Surface) = 
    Tree.node [] (text surf.Name) true (viewOpcs surf.OPCs)

  let viewSurfaces surfaces =
    surfaces |> List.map viewSurface |> Tree.node [] (text "Surfaces") true

  let viewMeasurement (measurement : Measurement) = 
    Tree.leaf [] [text measurement.Base.Id]

  let viewMeasurements (rootGroup : MeasurementGroup) =
    rootGroup.Measurements |> List.map viewMeasurement |> Tree.node [] (text rootGroup.Name) true

  let view (model : Model) =
    div [] [
        Tree.treeView [attribute "data-on-click" "tree_add_leaf_example_click"] [
            Tree.node [] (text model.Scene.Name) false [
                    viewSurfaces model.Scene.Surfaces
                    //viewMeasurements model.Scene.Measurements
                ]             
        ]
    ]

  let start node () =
    createApp Model.initial view update
    |> withStartNodeSelector node
    |> start renderer
