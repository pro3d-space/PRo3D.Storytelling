namespace KitchenSink

open Aardvark.Service

open System

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Application.WinForms
open System.Collections.Generic
open System.Collections.Concurrent
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text

module KitchenSinkApp =

    type Message =
        | CameraMessage of CameraController.Message

    let initial =
        {
            cameraModel = CameraController.initial
            value = 1.0
        }

    let update (m : Model) (msg : Message) =
        match msg with
            | CameraMessage msg -> { m with cameraModel = CameraController.update m.cameraModel msg }

    let view (m : MModel) =
        let view = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI
        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0
        let cam = Mod.constant (Camera.create view frustum)


        body [] [
            div [] [
                Incremental.renderControl
                    cam
                    ( AttributeMap.ofListCond [ always (style "display: flex; width: 100%; height: 100%")] )
                    ( Sg.ofSeq [] )
            ]
        ]


    let pool = ThreadPool.empty

    let start () =
        App.start {
            unpersist = Unpersist.instance
            threads = fun _ -> pool
            view = view
            update = update
            initial = initial
        }