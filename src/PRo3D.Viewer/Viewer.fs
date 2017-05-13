namespace PRo3D

open Aardvark.Service

open System
open System.Windows.Forms

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

open PRo3D

module DialogUtils =
    open System.Windows.Forms
    open Microsoft.WindowsAPICodePack.Dialogs

    let openFolder (w : Form) =
        let mutable r = []
        w.Invoke(Action(fun _ -> 
            let dialog = new CommonOpenFileDialog()
            dialog.Multiselect <- true
          //  dialog.InitialDirectory <- "C:\\Users"
            dialog.IsFolderPicker <- true
            if dialog.ShowDialog() = CommonFileDialogResult.Ok then
                r <- dialog.FileNames |> Seq.toList
            else r <- []
        )) |> ignore
        r

module Viewer =

    type Message =
        | CameraMessage of CameraController.Message
        | OpenSurface 
        | AddSg of ISg

    let initial =
        {
            cameraModel = CameraController.initial
            importOpcs = []
            surfaces = HSet.empty
        }

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) (msg : Message) =
        match msg with
            | CameraMessage msg -> { m with cameraModel = CameraController.update m.cameraModel msg }
            | OpenSurface -> 
                match  DialogUtils.openFolder w with
                    | [] -> m
                    | paths ->
                        Log.line "Patch hierarchies: %A" paths
                        let scene = { Aardvark.Opc.Configurations.Network.mars() with patchHierarchies = paths }  
                        Log.start "importing: %A" scene
                        let sg,_ = Aardvark.Opc.Viewer.createPlainSceneGraph runtime signature scene
                        Log.stop "did it"
                        let bb = scene.boundingBox
                        let view = CameraView.lookAt (bb.Max - bb.Center)  V3d.Zero bb.Center.Normalized
                        { m with surfaces = HSet.add sg m.surfaces; cameraModel = { CameraController.initial with view = view  } }
            | AddSg sg -> { m with surfaces = HSet.add sg m.surfaces }

    let scene (m : MModel) =
        let opcs =
            aset {
                for surface in m.surfaces do
                    yield surface |> Sg.noEvents
            }
        opcs |> Sg.set

    let view (m : MModel) =

        let renderControl = 
                CameraController.controlledControl m.cameraModel CameraMessage (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ attribute "style" "width:100%; height: 100%"; ]) (scene m)

        let openSurface =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> OpenSurface)
             ] 

        body [] [
            require Html.semui (
                Html.SemUi.adornerMenu [
                    "Open Surface", [openSurface]
                ] renderControl
            )
        ]


    let pool (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) = 
        
        //let imports =
        //    proclist {
        //        for opcPath in m.importOpcs do
        //            let h = System.IO.Directory.EnumerateDirectories opcPath
        //            let scene = { Aardvark.Opc.Configurations.Network.mars() with patchHierarchies = h }  
        //            let sg = Aardvark.Opc.Viewer.createPlainSceneGraph runtime signature scene
        //            yield AddSg sg
        //    }

        ThreadPool.empty

    let start (w : Form) (runtime: IRuntime) (signature: IFramebufferSignature) =
        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view
            update = update w runtime signature
            initial = initial
        }