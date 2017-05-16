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
    open Aardvark.Opc
    open Aardvark.Opc    
    open Aardvark.SceneGraph.SgPrimitives.Sg

    type Message =
        | CameraMessage of CameraController.Message
        | OpenSurface 
        | QuickLoad
        | AddSg of ISg
        | PickSurface of SceneHit

    let initial =
        {
            cameraModel = CameraController.initial
            importOpcs = []
            surfaces = HSet.empty           
        }

    let loadHierarchies scene (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) =
        Log.start "importing: %A" scene
        let sg,_ = Aardvark.Opc.Viewer.createPlainSceneGraph runtime signature scene
        Log.stop "did it"

        //let hierarchies = scene.patchHierarchies |> Seq.map PatchHierarchy.load |> Seq.map (fun p -> p.tree |> QTree.height)

        let bb = scene.boundingBox
        let view = CameraView.lookAt (bb.Max - bb.Center)  V3d.Zero bb.Center.Normalized
        { m with surfaces = HSet.add (Surface.mk sg bb) m.surfaces; cameraModel = { CameraController.initial with view = view  } }

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) (msg : Message) =
        match msg with
            | CameraMessage msg -> { m with cameraModel = CameraController.update m.cameraModel msg }
            | OpenSurface -> 
                match  DialogUtils.openFolder w with
                    | [] -> m
                    | paths ->
                        Log.line "Patch hierarchies: %A" paths
                        let scene = { Aardvark.Opc.Configurations.Empty.mars() with patchHierarchies = paths }
                        loadHierarchies scene runtime signature m                        
            | AddSg sg -> { m with surfaces = HSet.add  (Surface.mk sg Box3d.Invalid) m.surfaces }
            | QuickLoad -> loadHierarchies (Aardvark.Opc.Configurations.Empty.mars()) runtime signature m
            | PickSurface p -> 
                Log.error "picked bb at %A" (p.globalRay.Ray.Ray.Direction, p.globalRay.Ray.Ray.Origin)
                m
                
    let scene (m : MModel) =
        let opcs =
            aset {
                for surface in m.surfaces do

                    let createSg (bb : Box3d) sg =    
                        let box = bb.Translated(bb.Center.Negated)
                        sg |> Sg.noEvents |> Sg.andAlso (wireBox' C4b.VRVisGreen box 
                                                           // |> Sg.noEvents 
                                                            |> Sg.shader {
                                                                    do! DefaultSurfaces.trafo
                                                                    do! DefaultSurfaces.vertexColor 
                                                               } |> Sg.noEvents
                                                        ) 
                        |> Sg.pickable (PickShape.Box box) 

                    let sg = 
                        Mod.map2 createSg surface.globalBB surface.sceneGraph
                        |> Sg.dynamic
                        |> Sg.withEvents [
                                Sg.onMouseDownEvt (fun sceneHit -> PickSurface sceneHit)
                            ]

                    yield sg
            }
        opcs |> Sg.set

    let view (m : MModel) =

        let renderControl = 
                CameraController.controlledControl m.cameraModel CameraMessage (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ attribute "style" "width:70%; height: 100%; float:left"; ]) (scene m)

        let openSurface =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> OpenSurface)
             ] 

        body [style "background: #1B1C1E"] [
            require Html.semui (
                Html.SemUi.adornerMenu [
                    "Open Surface", [openSurface]
                ](
                    div [][
                        renderControl
                        div [style "width:29%; height: 100%; float:right;"] [
                            div [clazz "ui buttons inverted"] [
                                 button [clazz "ui icon button"; onMouseClick (fun _ -> QuickLoad)] [
                                    i [clazz "folder outline icon"] [] ]
                            ]
                        ]
                    ]
                )
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