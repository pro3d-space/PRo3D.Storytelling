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

open MBrace.FsPickler
open System.IO

open PRo3D

module Serialization =
    
    let binarySerializer = FsPickler.CreateBinarySerializer()

    let save path (d : 'a) =    
        let arr = binarySerializer.Pickle d     
        File.WriteAllBytes(path, arr);
        d

    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr
        

module DialogUtils =
    open System.Windows.Forms
    open Microsoft.WindowsAPICodePack.Dialogs    

    let saveDialog  (w : Form)  =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new SaveFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r
    
    let openDialog (w : Form) =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new OpenFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r

    let openDialog2 (w : Form)(isFolder : bool)(isMulti : bool) =
        let mutable r = []
        w.Invoke(Action(fun _ -> 
            let dialog = new CommonOpenFileDialog()
            dialog.Multiselect <- isMulti
          //  dialog.InitialDirectory <- "C:\\Users"
            dialog.IsFolderPicker <- isFolder
            if dialog.ShowDialog() = CommonFileDialogResult.Ok then
                r <- dialog.FileNames |> Seq.toList
            else r <- []
        )) |> ignore
        r
      
    let openFolders (w : Form) =
        openDialog2 w true true

module Box3d =
    let extendBy (box:Box3d) (b:Box3d) =
        box.ExtendBy(b)
        box

    let ofSeq (bs:seq<Box3d>) =
        let box = 
            match bs |> Seq.tryHead with
                | Some b -> b
                | None -> Box3d.Invalid
                    
        for b in bs do
            box.ExtendBy(b)
        box

module Viewer =
    open Aardvark.Opc    
    open Aardvark.SceneGraph.SgPrimitives.Sg

    type Message =
        | CameraMessage of CameraController.Message
        | OpenSurface 
        | QuickLoad
        | AddSg of ISg
        | PickSurface of SceneHit
        | SaveScene
        | OpenScene
        | NewScene

    let initial =
        {           
            surfaces = HSet.empty                       
            scene = {
                    importOpcs = []
                    cameraModel = CameraController.initial            
                    interaction = InteractionMode.PickOrbitCenter
            }
        }
    
    

    let loadHierarchies (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) (computeView : bool) =
        let scene = { Aardvark.Opc.Configurations.Empty.mars() with patchHierarchies = m.scene.importOpcs }
        Log.start "importing: %A" scene
        let sg,_,hierarchies = Aardvark.Opc.Viewer.createPlainSceneGraph runtime signature scene
        Log.stop "did it"
        //let hierarchies = scene.patchHierarchies |> Seq.map PatchHierarchy.load |> Seq.map (fun p -> p.tree |> QTree.height)
        
        //let bb = 
        //    hierarchies
        //        |> List.map(
        //            fun d -> match d.tree with
        //                        | Node (a,_) -> a.info.GlobalBoundingBox
        //                        | Leaf a -> a.info.GlobalBoundingBox) 
        //        |> Box3d.ofSeq

        let bb = 
            hierarchies
                |> List.map(fun d -> d.tree |> QTree.getLeaves)                  
                |> Seq.map (fun p -> p |> Seq.map(fun d -> d.info.GlobalBoundingBox)) 
                |> Seq.concat
                |> Box3d.ofSeq
        
        let cam = 
            if computeView then
                let view = CameraView.lookAt bb.Max bb.Center bb.Center.Normalized
                { CameraController.initial with view = view }
            else 
                m.scene.cameraModel
               
        { m with surfaces = HSet.add (Surface.mk sg bb) m.surfaces; scene = { m.scene with cameraModel = cam }}

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) (msg : Message) =        
        match msg with
            | CameraMessage msg -> { m with scene = { m.scene with cameraModel = CameraController.update m.scene.cameraModel msg }}
            | OpenSurface -> 
                match DialogUtils.openFolders w with
                    | [] -> m
                    | paths ->
                        Log.line "Patch hierarchies: %A" paths
                        let m = { m with scene = { m.scene with importOpcs = paths }}
                        loadHierarchies runtime signature m true                      
            | AddSg sg -> { m with surfaces = HSet.add  (Surface.mk sg Box3d.Invalid) m.surfaces }
            | QuickLoad -> loadHierarchies runtime signature { m with scene = { m.scene with importOpcs = [@"D:\_WORK\Aardwork\Exomars\_Scenes\Dingo_Gap_2\Surface\MSL_Mastcam_Sol_529_id_47596\OPC_000_000"]}} true
            | PickSurface p -> 
                Log.error "picked bb at %A" (p.globalRay.Ray.Ray.Direction, p.globalRay.Ray.Ray.Origin)
                m
            | SaveScene -> 
                match DialogUtils.saveDialog w with
                    | None -> m
                    | Some p -> {m with scene = Serialization.save p m.scene}
            | OpenScene ->
                match DialogUtils.openDialog w with
                    | None -> m
                    | Some path -> 
                        let s = Serialization.loadAs<Scene> path
                        loadHierarchies runtime signature { m with scene = s } false
            | NewScene -> initial
            | _ -> m
                
    let scene (m : MModel) =
        let opcs =
            aset {
                for surface in m.surfaces do

                    let createSg (bb : Box3d) sg =    
                   //     let box = bb.Translated(bb.Center.Negated)
                        sg |> Sg.noEvents |> Sg.andAlso (wireBox' C4b.VRVisGreen bb 
                                                           // |> Sg.noEvents 
                                                            |> Sg.shader {
                                                                    do! Shader.stableTrafo
                                                                    do! DefaultSurfaces.vertexColor 
                                                               } |> Sg.noEvents
                                                        ) 
                        |> Sg.pickable (PickShape.Box bb) 

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
                CameraController.controlledControl m.scene.cameraModel CameraMessage (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ attribute "style" "width:70%; height: 100%; float:left"; ]) (scene m)

        let interactTools =
            div [clazz "ui buttons inverted"] [
                button [clazz "ui icon button"; onMouseClick (fun _ -> QuickLoad)] [
                            i [clazz "large rocket icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> QuickLoad)] [
                            i [clazz "large rocket icon"] [] ]
            ]

        let sceneTools =
            div [clazz "ui buttons inverted"] [
                button [clazz "ui icon button"; onMouseClick (fun _ -> SaveScene)] [
                            i [clazz "save icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> OpenScene)] [
                            i [clazz "folder outline icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> NewScene)] [
                            i [clazz "file outline icon"] [] ]
            ]
            

        let openSurface =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> OpenSurface)
             ] 

        body [style "background: #1B1C1E"] [
            require Html.semui (
                Html.SemUi.adornerMenu [
                    "Interactions", [interactTools]
                    "Scene", [sceneTools]
                    "Add Surface", [openSurface]
                    
                ](
                    div [style "background: #1B1C1E"][
                        renderControl
                        div [style "width:29%; height: 100%; float:right; background: #1B1C1E"] [
                            text "STufF here"
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