namespace PRo3D

module Navigation =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open Aardvark.UI.Primitives

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering

    type Action =
        | ArcBallAction         of ArcBallController.Message
        | FreeFlyAction         of CameraController.Message
        | SetNavigationMode     of NavigationMode

    let update (model : NavigationModel) (act : Action) =
        match act with            
            | ArcBallAction a -> 
                let model = 
                    match a with 
                        | ArcBallController.Message.Pick a ->
                            { model with navigationMode =  NavigationMode.ArcBall; exploreCenter = a }
                        | _ -> model
                        
                { model with camera = ArcBallController.update model.camera a }
            | FreeFlyAction a ->
                { model with camera = CameraController.update model.camera a }
            | SetNavigationMode mode ->
                { model with navigationMode = mode }

    module UI =
        open Aardvark.Base.Camera

        let frustum (model : MNavigationModel) =
            adaptive {
                let! near = model.config.nearPlane.value
                let! far = model.config.farPlane.value
                return (Frustum.perspective 60.0 near far 1.0)
                }

        let renderControlAttributes (model : MNavigationModel) =
            amap {
                let! state = model.navigationMode 
                match state with
                    | NavigationMode.FreeFly -> yield! CameraController.extractAttributes model.camera FreeFlyAction (frustum model)
                    | NavigationMode.ArcBall -> yield! ArcBallController.extractAttributes model.camera ArcBallAction (frustum model)
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap

        let viewNavigationModes  (model : MNavigationModel) =
            Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large location arrow icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.navigationMode SetNavigationMode ]                
            ]

    module Sg =
        let view (model:MNavigationModel) =
            let point =
                aset{
                let! pos = model.exploreCenter
                yield DrawingApp.Sg.dots (alist{yield pos}) (Mod.constant C4b.Magenta) model.camera.view
                } |> Sg.set
            Sg.ofList [point]
           