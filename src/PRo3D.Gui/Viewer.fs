namespace PRo3DGui

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open System.Windows.Forms

open Scratch
open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module Viewer =

    open Scratch.DomainTypes
    open SharedModel
    open Aardvark.ImmutableSceneGraph
    open Aardvark.Elmish
    open Aardvark.Base.Rendering

    
    //[Style ["width", "100%"; "height", "100%"; "background-color", "transparent"]; attribute "id" "renderControl"] 

    type Action = Gui of FullLayoutApp.Action | ThreeD of Scratch.ComposedTestApp.Action

    let oldView (cam : IMod<Camera>) (nativeSg : ISg) m= 
        Aardvark.ImmutableSceneGraph.Conv<_>((fun _ -> nativeSg),Scene.group[]) :> ISg<_>
        //m.mscene  |> TranslateController.viewScene cam |> Scene.map Translate

    let view3D (m : DomainModel.MFullModel) =
        Scratch.ComposedTestApp.view m.composedTest 
        |> Scene.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.vertexColor |> toEffect; DefaultSurfaces.simpleLighting |> toEffect ]
        |> Scene.map ThreeD

    let update (f : Form) e (m : DomainModel.FullModel) (msg : Action) =
        match msg with
            | Gui msg -> FullLayoutApp.update f (Env.map Gui e) m msg
            | ThreeD a -> { m with composedTest = Scratch.ComposedTestApp.update (Env.map ThreeD) m.composedTest a }

    let createApp form runtime sizes mouse keyboard signature time viewport camera =

        let app = FullLayoutApp.app ignore form
        let initial = app.initial
        let composed = ComposedApp.ofUpdate initial (update form)

//        let loader = 
//            async {
//                return Aardvark.Opc.Viewer.createSceneGraph runtime sizes mouse keyboard signature time (Aardvark.Opc.Configurations.Network.mola())
//            } |> Mod.async
//
//        let sg = 
//            adaptive {
//                let! loadedScene = loader
//                match loadedScene with
//                    | Some l -> 
//                        return l
//                    | None ->   
//                        return Sg.group' []
//            } |> Sg.dynamic


        let three3dApp : App<DomainModel.FullModel,DomainModel.MFullModel,Action,ISg<Action>>  = {
            initial = initial
            update = update form
            view = view3D
            ofPickMsg = fun _ _ -> []
            subscriptions = Subscriptions.none
        }



        let viewApp  = 
            {
                initial = initial 
                update = update form
                view = app.view >> Html.map Gui
                subscriptions = Fablish.CommonTypes.Subscriptions.none
                onRendered = OnRendered.ignore
            }

        let three3dInstance = ComposedApp.add3d composed keyboard mouse viewport camera three3dApp (fun m app -> m) id id
        let fablishInstance = ComposedApp.addUi composed Net.IPAddress.Loopback "8083" viewApp (fun m app -> m) id id

        three3dInstance, fablishInstance