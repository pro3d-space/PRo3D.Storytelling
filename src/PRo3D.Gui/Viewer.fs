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

    

//        let sg = 
//            adaptive {
//                let loader = 
//                    async {
//                        return Aardvark.Opc.Viewer.createSceneGraph runtime sizes mouse keyboard signature time (Aardvark.Opc.Configurations.Network.mola())
//                    } |> Mod.async
//                let! loadedScene = loader
//                match loadedScene with
//                    | Some l -> 
//                        return l
//                    | None ->   
//                        return Sg.group' []
//            } |> Sg.dynamic

    
//
//    let nativeSceneGraph (runtime : IRuntime) (signature : IFramebufferSignature) (surfaces : IMod<list<DomainModel.Surface>>) =
//        aset {
//            let! ss = surfaces
//            let config = Aardvark.Opc.Configurations.Network.mars()
//            for s in ss do
//                printfn "loading: %A" s
//                let a = 
//                    async {
//                        return Aardvark.Opc.Viewer.createPlainSceneGraph runtime signature {config with patchHierarchies = s.OPCs |> List.map (fun a -> a.name)}
//                    } |> Mod.async
//                let! b = a
//                match b with
//                    | None -> ()
//                    | Some b -> yield b
//        } |> Sg.set

             

    type Action = Gui of FullLayoutApp.Action | ThreeD of Scratch.FreeFlyCameraApp.Action

    let oldView (cam : IMod<Camera>) (nativeSg : ISg) m= 
        Aardvark.ImmutableSceneGraph.Conv<_>((fun _ -> nativeSg),Scene.group[]) :> ISg<_>
        //m.mscene  |> TranslateController.viewScene cam |> Scene.map Translate

    let view3D (runtime : IRuntime) (signature : IFramebufferSignature) (frustum : IMod<Frustum>) (m : DomainModel.MFullModel) =
        Scene.group [
            //Scratch.ComposedTestApp.view frustum m.composedTest 
            //Scene.ofSg (nativeSceneGraph runtime signature m.surfaces)
            Scene.ofSg (Sg.set m.renderSurfaces)

            // renderMeasurements (gets kdtrees and enabled states, priority)
        ]
        |> Scene.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.vertexColor |> toEffect; DefaultSurfaces.simpleLighting |> toEffect ]
        |> Scene.camera (Mod.map2 Camera.create m.camera.mcamera frustum) 
        |> Scene.map ThreeD
        

    let update runtime signature (f : Form) e (m : DomainModel.FullModel) (msg : Action) =
        match msg with
            | Gui msg -> FullLayoutApp.update runtime signature f (Env.map Gui e) m msg
            //| ThreeD a -> { m with composedTest = Scratch.ComposedTestApp.update (Env.map ThreeD) m.composedTest a }
            | ThreeD a -> { m with camera = Scratch.FreeFlyCameraApp.update e m.camera a }

    let createApp form runtime sizes mouse keyboard signature time viewport camera =

        let app = FullLayoutApp.app runtime signature ignore form
        let initial = app.initial
        let composed = ComposedApp.ofUpdate initial (update runtime signature form)

        composed.Update(Gui(FullLayoutApp.LoadSurfaces initial.pendingSurfaces)) |> ignore
        

        let frustum = sizes |> Mod.map (fun (s : V2i) -> Frustum.perspective 60.0 0.1 10.0 (float s.X / float s.Y))


        let three3dApp : App<DomainModel.FullModel,DomainModel.MFullModel,Action,ISg<Action>>  = {
            initial = initial
            update = update runtime signature form
            view = view3D runtime signature frustum
            ofPickMsg = fun _ _ -> []
            subscriptions = fun m -> Sub.map ThreeD (Scratch.FreeFlyCameraApp.subscriptions time m.camera)
        }



        let viewApp  = 
            {
                initial = initial 
                update = update runtime signature form
                view = app.view >> Html.map Gui
                subscriptions = Fablish.CommonTypes.Subscriptions.none
                onRendered = OnRendered.ignore
            }

        let three3dInstance = ComposedApp.add3d composed keyboard mouse viewport camera three3dApp (fun m app -> m) id id
        let fablishInstance = ComposedApp.addUi composed Net.IPAddress.Loopback "8083" viewApp (fun m app -> m) id id

        three3dInstance, fablishInstance