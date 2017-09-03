namespace PRo3D

module Navigation =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open Aardvark.UI.Primitives

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.Base.Camera
    open PRo3D.Navigation2
   // open Navigation.Sg

    type Action =
        | ArcBallAction         of ArcBallController.Message
        | FreeFlyAction         of CameraController.Message
        | SetNavigationMode     of NavigationMode

    type smallConfig<'a,'b> = 
        {
            navigationSensitivity : Lens<'a, float>
            up                    : Lens<'b, V3d>
        }

    let update<'a,'b> (bigConfigA : 'a) (bigConfigB : 'b) (smallConfig : smallConfig<'a,'b>) (model : NavigationModel) (act : Action) =
        match act with            
            | ArcBallAction a -> 
                let model = 
                    match a with 
                        | ArcBallController.Message.Pick a ->
                            { model with navigationMode =  NavigationMode.ArcBall; exploreCenter = a }
                        | _ -> model
                
                let cam = ArcBallController.update model.camera a
                let cam = { cam with sensitivity = smallConfig.navigationSensitivity.Get(bigConfigA) } 
                match cam.orbitCenter with
                    | Some oc -> { model with camera = cam; exploreCenter = oc}
                    | None -> { model with camera = cam }
                    
            | FreeFlyAction a ->
                let cam' = CameraController.update model.camera a                
                { model with camera = { cam' with sensitivity = smallConfig.navigationSensitivity.Get(bigConfigA) } }
            | SetNavigationMode mode ->
                match mode with
                    | NavigationMode.FreeFly ->
                        let center = 
                            match model.camera.orbitCenter with
                                | Some x ->  x
                                | None   -> V3d.OOO

                        let view' =
                            CameraView.lookAt model.camera.view.Location center (smallConfig.up.Get(bigConfigB))
                        
                        { model with camera = { model.camera with view = view'}; navigationMode = mode} 
                    | _ ->  { model with navigationMode = mode }
               
    module UI =
        open Aardvark.Base.Camera

        type smallConfig<'ma> = 
            {
                getNearPlane : 'ma -> IMod<float>
                getFarPlane  : 'ma -> IMod<float>
            }

        let frustum near far =
            adaptive {
                let! near = near
                let! far = far
                return (Frustum.perspective 60.0 near far 1.0)
                }

        let renderControlAttributes (model : MNavigationModel) near far =
            amap {
                let! state = model.navigationMode 
                match state with
                    | NavigationMode.FreeFly -> yield! CameraController.extractAttributes model.camera FreeFlyAction (frustum near far)
                    | NavigationMode.ArcBall -> yield! ArcBallController.extractAttributes model.camera ArcBallAction (frustum near far)
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
                yield PRo3D.Sg.dots (AList.single pos) (Mod.constant C4b.Magenta) model.camera.view
                } |> Sg.set
            Sg.ofList [point]
           