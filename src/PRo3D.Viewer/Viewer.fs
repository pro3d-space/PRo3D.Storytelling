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

    let openFolder (w : Form) =
        openDialog2 w true false

module Box3d =
    let extendBy (box:Box3d) (b:Box3d) =
        box.ExtendBy(b)
        box

    let ofSeq (bs:seq<Box3d>) =
        let box = 
            match bs |> Seq.tryHead with
                | Some b -> b
                | None -> failwith "box sequence must not be empty"
                    
        for b in bs do
            box.ExtendBy(b)
        box

module Viewer =
    open Aardvark.Opc    
    open Aardvark.SceneGraph.SgPrimitives.Sg

    type Message =
        | CameraMessage of CameraController.Message
        | ImportSurface 
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
                    cameraModel = CameraController.initial            
                    interaction = InteractionMode.PickOrbitCenter
                    surfaces = []
            }
        }
    
    let checkBB (bb:Box3d) = 
        if(bb.IsInvalid) then 
            failwith (sprintf "invalid bounding box %s" (bb.ToString()))
        else bb

    let computeBB (hierarchies : list<PatchHierarchy>) =
         hierarchies
                |> List.map(fun d -> d.tree |> QTree.getLeaves)                  
                |> Seq.map (fun p -> p |> Seq.map(fun d -> checkBB d.info.GlobalBoundingBox)) 
                |> Seq.concat
                |> Box3d.ofSeq

    let computeBBfromRoots (hierarchies : list<PatchHierarchy>) =
         hierarchies
                |> List.map(fun d -> d.tree |> QTree.getRoot)                  
                |> Seq.map (fun p -> checkBB p.info.GlobalBoundingBox)                                
                |> Box3d.ofSeq    

    let lookAtData m = 
        let bb = m.surfaces |> Seq.map(fun x -> x.globalBB) |> Box3d.ofSeq
        let cam = 
            let view = CameraView.lookAt bb.Max bb.Center bb.Center.Normalized
            { CameraController.initial with view = view }            

        { m with scene = { m.scene with cameraModel = cam }}

    let importSurface (runtime : IRuntime) (signature: IFramebufferSignature) (surface : Surface) (m : Model) =
        
        Log.start "importing: %A" surface.importPath        
        let import = { Configurations.Empty.mars() with patchHierarchies = surface.opcPaths }                           
        let sg,_,hierarchies = Viewer.createPlainSceneGraph runtime signature import
        Log.stop "did it"
              
        let surfaceImp = Surface.mkImported surface sg (hierarchies |> computeBB)
                
        { m with 
                scene       = { m.scene with surfaces = surface::m.scene.surfaces }
                surfaces    = HSet.add surfaceImp m.surfaces }

    let loadSurfaces (runtime : IRuntime) (signature: IFramebufferSignature)(s : Scene) (m : Model) =
        
        let surfs = 
            s.surfaces 
                |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths })
                |> List.map (fun d -> Viewer.createPlainSceneGraph runtime signature d)
                |> List.zip s.surfaces
                |> List.map (fun (surf, (sg,_,hierarchies)) -> Surface.mkImported surf sg (hierarchies |> computeBB))
                |> HSet.ofList

        { m with scene = s; surfaces = surfs }
    
    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) (msg : Message) =        
        match msg with
            | CameraMessage msg -> { m with scene = { m.scene with cameraModel = CameraController.update m.scene.cameraModel msg }}
            | ImportSurface -> 
                match DialogUtils.openFolder w with
                    | [] -> m
                    | paths ->
                        let path = paths.Head
                        if (path |> OpcUtils.isSurfaceFolder) then
                            let s = Surface.mk path
                            importSurface runtime signature s m |> lookAtData

                        else
                            Log.error "path %s does not point to surface folder" path
                            m            
            | QuickLoad -> 
                let surface = Surface.mk (@"E:\Aardwork\Exomars\_SCENES\cd - Copy\Surface\Cape_Desire_RGB")
                let m = importSurface runtime signature surface m 
                m |> lookAtData
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
                        let m = { m with scene = s }

                        loadSurfaces runtime signature s m
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
                onClick (fun _ -> ImportSurface)
             ] 
      
        require Html.semui (
            body [] [                               
                    Html.SemUi.adornerMenu [
                        "Interactions", [interactTools]
                        "Scene", [sceneTools]
                        "Add Surface", [openSurface]
                    ](
                        div [clazz "pusher"] [                            
                            renderControl
                            div [style "width: 30%; height: 100%; float:right; background: #1B1C1E"] [
                                Incremental.div 
                                    (AttributeMap.ofList [clazz "ui divided list inverted segment"]) (
                                        alist {
                                            for s in (m.surfaces |> AList.ofASet) do
                                                    yield div [clazz "item"] [
                                                        i [clazz "medium File Outline middle aligned icon"][]                                             
                                                        Incremental.text s.name
                                                    ]
                                        }
                                )      
                            ]
                        ]
                    )
                 //   ]
            
            

                //require Html.semui (
                //    Html.SemUi.adornerMenu [
                //        "Interactions", [interactTools]
                //        "Scene", [sceneTools]
                //        "Add Surface", [openSurface]
                    
                //    ](
                //        div [][
                //            renderControl
                //            div [style "width: 29%; height: 100%; float:right; background: #1B1C1E"] [                           
                //                Incremental.div 
                //                    (AttributeMap.ofList [clazz "ui divided list inverted"]) (
                //                        alist {
                //                            for s in (m.surfaces |> AList.ofASet) do
                //                                    yield div [clazz "item"] [
                //                                        i [clazz "medium File Outline middle aligned icon"][]                                             
                //                                        text (s.guid.ToString())
                //                                    ]   
                //                        }
                //                )                            
                //            ]
                //        ]
                //    )
                //)
            ]
        )


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