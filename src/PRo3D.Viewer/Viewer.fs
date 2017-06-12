namespace PRo3D

open Aardvark.Service

open System
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text

open Aardvark.Opc    
open Aardvark.SceneGraph.SgPrimitives.Sg
open Aardvark.VRVis

open MBrace.FsPickler
open System.IO
open PRo3D

module PList =
    let append (a : plist<_>) (b : plist<_>) =
        let rec doIt xs =
            match xs with
                | x::xs -> PList.prepend x (doIt xs)
                | [] -> b
        doIt (PList.toList a)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]        
module Surface =
    let mk path = {        
        guid = Guid.NewGuid()
        name = IO.Path.GetFileName path
        importPath = path
        opcPaths = OpcUtils.getOPCPaths path      
    }

    let toSg (s:Surface) sg bb kd = {
        surface = s
       // guid = Guid.NewGuid()
        sceneGraph = sg
        globalBB = bb
        kdTree = kd
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

    let import (runtime : IRuntime) (signature: IFramebufferSignature) (surface : Surface) (m : Model) =
        
        Log.start "importing: %A" surface.importPath        
        let import = { Configurations.Empty.mars() with patchHierarchies = surface.opcPaths }                           
        let sg, hierarchies, kdtree = Viewer.createPlainSceneGraph runtime signature import true
        Log.stop "did it"
              
        let surfaceImp = toSg surface sg (hierarchies |> computeBB) kdtree
                
        { m with 
                scene       = { m.scene with surfaces = surface::m.scene.surfaces }
                surfaces    = HSet.add surfaceImp m.surfaces }

    let load (runtime : IRuntime) (signature: IFramebufferSignature)(s : Scene) (m : Model) =
        
        let surfs = 
            s.surfaces 
                |> List.map (fun d -> { d with guid = Guid.NewGuid() })
                |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths })
                |> List.map (fun d -> Viewer.createPlainSceneGraph runtime signature d true)
                |> List.zip s.surfaces
                |> List.map (fun (surf, (sg, hierarchies, kdtree)) -> toSg surf sg (hierarchies |> computeBB) kdtree)
                |> HSet.ofList

        { m with scene = s; surfaces = surfs }

    let fileExists path =
        if System.IO.File.Exists(path) then
            Some path
        else
            None

    let loadLastScene m runtime signature =
        match fileExists "./last" with
            | Some path  ->
                try 
                    let s = Serialization.loadAs<Scene> path
                    let m = { m with scene = s }
                    load runtime signature s m

                with e ->
                    Aardvark.Base.Log.warn "could not restore %A. using empty scene" p
                    System.IO.File.Delete path
                    m
            | _ -> m



    
module Viewer =
    open Surface
   
    type Message =
        | CameraMessage of CameraController.Message
        | DrawingMessage of Drawing.Action
        | ImportSurface 
        | ImportMeasurements 
        | QuickLoad
        | AddSg of ISg
        | PickSurface of SceneHit
        | SaveScene
        | OpenScene
        | NewScene
        | KeyDown of key : Aardvark.Application.Keys
        | KeyUp of key : Aardvark.Application.Keys      
                
    let lookAtData m = 
        let bb = m.surfaces |> Seq.map(fun x -> x.globalBB) |> Box3d.ofSeq
        let cam = 
            let view = CameraView.lookAt bb.Max bb.Center bb.Center.Normalized
            { CameraController.initial with view = view }            

        { m with scene = { m.scene with cameraModel = cam }}
    
    let loadScene path m runtime signature =
        let s = Serialization.loadAs<Scene> path
        let m = { m with scene = s }

        Surface.load runtime signature s m
        
    let doKdTreeIntersection (m : Model) (t : Trafo3d) (r : FastRay3d) : Option<double> = 
        match m.surfaces |> HSet.map (fun d -> d.kdTree) |> HSet.toSeq |> Seq.tryHead with 
            | Some kd ->                    
                let kdi = kd |> KdIntersectionTree
                let mutable hit = ObjectRayHit.MaxRange
                let objFilter  a b = true
                let hitFilter a b c d = false
                if kdi.Intersect(r, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then 
                    Some hit.RayHit.T
                else
                    None
            | None -> None

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) (msg : Message) =        
        match msg, m.drawing.draw with
            | CameraMessage msg, false -> { m with scene = { m.scene with cameraModel = CameraController.update m.scene.cameraModel msg }}
            | DrawingMessage msg, _ -> { m with drawing = Drawing.update m.drawing msg }
            | ImportSurface, _ -> 
                match Dialogs.openFolder w with
                    | [] -> m
                    | paths ->
                        let path = paths.Head
                        if (path |> OpcUtils.isSurfaceFolder) then
                            let s = Surface.mk path
                            Surface.import runtime signature s m |> lookAtData
                        else
                            Log.error "path %s does not point to surface folder" path
                            m   
            | ImportMeasurements, _ ->
                match Dialogs.openDialog w with
                    | x -> { m with drawing = {m.drawing with annotations = m.drawing.annotations |> PList.append (MeasurementsImporter.startImporter x.Value)}}
                    | _ -> m   
            | QuickLoad, _ -> 
                //let surface = Surface.mk (@"E:\Aardwork\Exomars\_SCENES\cd - Copy\Surface\Cape_Desire_RGB")
                //let m = Surface.import runtime signature surface m 
                //m |> lookAtData
                loadScene "D:\_WORK\Aardwork\Exomars\Dingo\MSL_Mastcam_Sol_540_id_47621_X\dingo" m runtime signature
            | PickSurface p, true ->                 
                let fray = p.globalRay.Ray
                let r = fray.Ray
                
                match doKdTreeIntersection m Trafo3d.Identity fray with
                    | Some t -> 
                        let hit = r.GetPointOnRay(t)
                        Log.warn "hitihtihtihtithi %A" hit
                        let drawing = Drawing.update m.drawing (Drawing.Action.AddPoint hit)
                        { m with drawing = drawing}
                    | None -> 
                        Log.error ("nooo hit")
                        m

            | SaveScene, _ -> 
                match Dialogs.saveDialog w with
                    | None -> m
                    | Some p -> 
                        Serialization.save "./last" m.scene |> ignore
                        {m with scene = Serialization.save p m.scene}
            | OpenScene, _ ->
                match Dialogs.openDialog w with
                    | None -> m
                    | Some path -> loadScene path m runtime signature
            | NewScene, _ -> { m with surfaces = hset.Empty ; scene = { m.scene with surfaces = [] }}
            | KeyDown k, _ ->
                    let d = Drawing.update m.drawing (Drawing.Action.KeyDown k)
                    { m with drawing = d }
            | KeyUp k, _ ->
                    let d = Drawing.update m.drawing (Drawing.Action.KeyUp k)
                    { m with drawing = d }
            | _ -> m
                
    let scene (m : MModel) =
        let opcs =
            aset {                
                for surface in m.surfaces do

                    let createSg (bb : Box3d) sg =                       
                        sg |> Sg.noEvents |> Sg.andAlso (wireBox' C4b.VRVisGreen bb                                                            
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

    let mkBrushISg color size trafo : ISg<Message> =         
        Sg.sphere 5 color size 
                |> Sg.shader {
                    do! Shader.stableTrafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.noEvents
                |> Sg.trafo(trafo)

    

    let view (m : MModel) =

        let renderControl = 
                CameraController.controlledControl m.scene.cameraModel CameraMessage (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
                    (AttributeMap.ofList [ 
                            onKeyDown (KeyDown)
                            onKeyUp (KeyUp)
                            attribute "style" "width:70%; height: 95%; float:left"; ]) 
                    (                    
                        let annotations =
                            Drawing.Sg.view m.drawing m.scene.cameraModel.view
                                |> Sg.map DrawingMessage                                
                                |> Sg.fillMode (Mod.constant FillMode.Fill)//m.rendering.fillMode
                                |> Sg.cullMode (Mod.constant CullMode.None)//m.rendering.cullMode  

                        [(scene m); annotations]
                            |> Sg.ofList
                         //   |> Sg.fillMode (Mod.constant FillMode.Fill)
                         //   |> Sg.cullMode (Mod.constant CullMode.None)      
                    )

        //right side fixed tabbed menu
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

        let importMeasurements =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> ImportMeasurements)
             ] 

        let surfaceList = 
            Incremental.div 
                (AttributeMap.ofList [clazz "ui divided list inverted segment"]) (
                    alist {
                        for s in (m.surfaces |> AList.ofASet) do
                                yield div [clazz "item"] [
                                    i [clazz "large cubes middle aligned icon"][]                                             
                                    div [clazz "content"] [
                                        div [clazz "header"] [Incremental.text s.surface.name]
                                        div [clazz "description"] [
                                            Incremental.text (
                                                s.globalBB 
                                                    |> Mod.map (fun d -> sprintf "Size: %.3f m" (d.Size.Length)))
                                        ]
                                    ]
                                    
                                ]
                    }
            )   

        let annotationUI = 
            div [][
                Drawing.UI.viewAnnotations m.drawing
            ]
            

        let tabs = Html.SemUi.tabbed [clazz "ui inverted segment"] [
                            ("Surfaces", surfaceList)
                            ("Annotations", annotationUI)
                            ("Config", text "Config")
                        ] "Surfaces"
      
        let topMenu = [Drawing.UI.viewAnnotationToolsHorizontal m.drawing |> UI.map DrawingMessage]
            

        let rest = 
            [
             //   div[clazz "ui bottom attached segment"][
                renderControl
                div [clazz "ui"; style "width: 30%; float:right; background: #1B1C1E"] [
                    div [clazz "ui"; style "height: 40%"][
                        tabs                                                        
                    ]
                ]
            //    div[clazz "ui inverted bottom attached menu"; style "width: 100%; height: 6%"] [i [clazz "wrench icon inverted"][]]
             //   ]
            ]
        
        let usingContext =             
            "$('.context.example .ui.sidebar')
              .sidebar({
                context: $('.context.example .bottom.segment')
              })
              .sidebar('attach events', '.context.example .menu.blarg .item.blarg')
            ;"

        //Sidebar adorner
        let myCss = { kind = Stylesheet; name = "semui-overrides"; url = "./semui-overrides.css" }
        require (Html.semui @ [myCss]) (
            let sections = [
                "Interactions", [interactTools]             
                "Scene", [sceneTools]
                "Add Surface", [openSurface]
                "Import Measurements", [importMeasurements]]                              
            onBoot usingContext (                            
                body [clazz "context example"; style "overflow:hidden"] (GuiEx.adornerMenu sections topMenu rest)
               
                    //GuiEx.menu "ui sidebar inverted vertical menu" sections
                    //div[clazz "pusher"] [
                    //    yield div [
                    //        clazz "ui black big launch right attached fixed button menubutton"
                    //        js "onclick" "$('.sidebar').sidebar('toggle');"
                    //    ] [
                    //        i [clazz "content icon"] [] 
                    //        span [clazz "text"] [text "Menu"]
                    //    ]

                    //    yield! rest
                    //]

                
            
                        //(GuiEx.adornerMenu [
                        //    "Interactions", [interactTools]
                        //    "Scene", [sceneTools]
                        //    "Add Surface", [openSurface]]
                        //rest)
        ))
                                                              

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
        let m = Model.initial        
        let m = Surface.loadLastScene m runtime signature

        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view
            update = update w runtime signature
            initial = m
        }