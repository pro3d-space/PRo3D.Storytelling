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
        | ArcBallAction     of ArcBallController.Message
        | FreeFlyAction     of CameraController.Message
        | SetNavigationMode of NavigationMode

    let update (model : NavigationModel) (act : Action) =
        match act with            
            | ArcBallAction a -> 
                let model = 
                    match a with 
                        | ArcBallController.Message.Pick a ->
                            { model with navigationMode =  NavigationMode.ArcBall }
                        | _ -> model
                        
                { model with camera = ArcBallController.update model.camera a }
            | FreeFlyAction a ->
                { model with camera = CameraController.update model.camera a }
            | SetNavigationMode mode ->
                { model with navigationMode = mode }

    module UI =

        let frustum =
                Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

        let renderControlAttributes (model : MNavigationModel) =
            amap {
                let! state = model.navigationMode 
                match state with
                    | NavigationMode.FreeFly -> yield! CameraController.extractAttributes model.camera FreeFlyAction frustum
                    | NavigationMode.ArcBall -> yield! ArcBallController.extractAttributes model.camera ArcBallAction frustum
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap

        let viewNavigationModes  (model : MNavigationModel) =
             Html.Layout.boxH [ Html.SemUi.dropDown model.navigationMode SetNavigationMode ]
            // Html.table [                            
            //   Html.row "Mode:" [Html.SemUi.dropDown model.navigationMode SetNavigationMode]
            //]