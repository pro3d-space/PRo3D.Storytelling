namespace PRo3DGui

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

open Scratch
open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

module Viewer =

    open Scratch.DomainTypes
    open SharedModel
    open Aardvark.ImmutableSceneGraph
    open Aardvark.Elmish

    
    //[Style ["width", "100%"; "height", "100%"; "background-color", "transparent"]; attribute "id" "renderControl"] 

    let view3D (cam : IMod<Camera>) (nativeSg : ISg) m= 
        Aardvark.ImmutableSceneGraph.Conv<_>((fun _ -> nativeSg),Scene.group[]) :> ISg<_>
        //m.mscene  |> TranslateController.viewScene cam |> Scene.map Translate

    let createApp form runtime sizes mouse keyboard signature time viewport camera =

        let app = FullLayoutApp.app ignore form
        let initial = app.initial
        let composed = ComposedApp.ofUpdate initial app.update 

        let loader = 
            async {
                return Aardvark.Opc.Viewer.createSceneGraph runtime sizes mouse keyboard signature time (Aardvark.Opc.Configurations.Network.mola())
            } |> Mod.async

        let sg = 
            adaptive {
                let! loadedScene = loader
                match loadedScene with
                    | Some l -> 
                        return l
                    | None ->   
                        return Sg.group' []
            } |> Sg.dynamic

        let three3dApp : App<DomainModel.FullModel,DomainModel.MFullModel,_,ISg<FullLayoutApp.Action>>  = {
            initial = initial
            update = app.update
            view = view3D camera sg
            ofPickMsg = fun _ _ -> []
            subscriptions = Subscriptions.none
        }

        let viewApp  = 
            {
                initial = initial 
                update = app.update
                view = app.view
                subscriptions = Fablish.CommonTypes.Subscriptions.none
                onRendered = OnRendered.ignore
            }

        let three3dInstance = ComposedApp.add3d composed keyboard mouse viewport camera three3dApp (fun m app -> m) id id
        let fablishInstance = ComposedApp.addUi composed Net.IPAddress.Loopback "8083" viewApp (fun m app -> m) id id

        three3dInstance, fablishInstance