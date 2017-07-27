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

open PRo3D.Viewer
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
    let qualityInit = 
        {
            value = 1.0
            min = 0.5
            max = 100.0
            step = 0.5
            format = "{0:0.0}"
        }

    let mk path = {
        guid = Guid.NewGuid()
        name = IO.Path.GetFileName path
        importPath = path
        opcPaths = OpcUtils.getOPCPaths path      
        fillMode = FillMode.Fill
        cullMode = CullMode.None
        isVisible = true
        quality = qualityInit
    }

    let toSg (s:Surface) sg bb kd = {
        surface = s
        guid = Guid.NewGuid()
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
    
    let createSgSurfaces runtime signature surfaces =
        surfaces
            //|> List.map (fun (d : Surface) -> { d with guid = Guid.NewGuid() })
            |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths })
            |> List.map (fun d -> Viewer.createPlainSceneGraph runtime signature d true)
            |> List.zip surfaces
            |> List.map (fun (surf, (sg, hierarchies, kdtree)) -> toSg surf sg (hierarchies |> computeBB) kdtree)
            |> HSet.ofList

    let import' (runtime : IRuntime) (signature: IFramebufferSignature)(surfaces : list<Surface>) (m : Model) =
                        
        let surfSet = createSgSurfaces runtime signature surfaces
                                                
        { m with 
            scene       =  { m.scene with surfaces = surfaces @ m.scene.surfaces }
            surfaces    =  HSet.union surfSet m.surfaces }

    let load (runtime : IRuntime) (signature: IFramebufferSignature)(s : Scene) (m : Model) =
        
        let surfs = createSgSurfaces runtime signature s.surfaces
        { m with scene = s; surfaces = surfs }

    let fileExists path =
        if System.IO.File.Exists(path) then
            Some path
        else
            None

    let loadScene path m runtime signature =
        let s = Serialization.loadAs<Scene> path
        let m = { m with scene = s }

        load runtime signature s m

    let loadLastScene m runtime signature =
        match fileExists "./last" with
            | Some path  ->
                try 
                   loadScene path m runtime signature

                with e ->
                    Aardvark.Base.Log.warn "could not restore %A. using empty scene" p
                    System.IO.File.Delete path
                    m
            | _ -> m
    
module ViewerApp =
    open Surface
    open Aardvark.Base.Geometry.RayHit
    open Aardvark.SceneGraph.SgFSharp.Sg
    open System.Collections.Concurrent
    open FShade.GLSL.AssemblerState
        
    type PropertyActions =
        | DrawingMessage    of Drawing.Action
        | AnnotationMessage of AnnotationProperties.Action

    type Action =                
        | DrawingMessage            of Drawing.Action
        | NavigationMessage         of Navigation.Action
        | ReferenceSystemMessage    of ReferenceSystemApp.Action
        | AnnotationMessage         of AnnotationProperties.Action
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | ImportSurface 
        | ImportMeasurements 
        | DnSProperties             of PropertyActions
        | ConfigPropertiesMessage   of ConfigProperties.Action
        | QuickLoad1
        | QuickLoad2
        | DeleteLast
        | AddSg                     of ISg
        | PickSurface               of SceneHit*string
        | SaveScene
        | SaveAs                    
        | OpenScene
        | NewScene
        | KeyDown                   of key : Aardvark.Application.Keys
        | KeyUp                     of key : Aardvark.Application.Keys      
        | SetInteraction            of Interactions
        | SelectSurface             of Guid
        | FlyToSurface              of Guid      
                
    let lookAtData m = 
        let bb = m.surfaces |> Seq.map(fun x -> x.globalBB) |> Box3d.ofSeq
        let cam = 
            let view = CameraView.lookAt bb.Max bb.Center bb.Center.Normalized
            { CameraController.initial with view = view }            

        { m with scene = { m.scene with navigation = {m.scene.navigation with camera = cam} }}
    
    let lookAtSurface m id =
        let surf = m.surfaces |> Seq.tryFind(fun x -> x.surface.guid = id)
        match surf with
            | Some s ->
                let bb = s.globalBB
                let cam = 
                    let view = CameraView.lookAt bb.Max bb.Center bb.Center.Normalized
                    { CameraController.initial with view = view }            

                { m with scene = { m.scene with navigation = {m.scene.navigation with camera = cam} }}
            | None -> m
            
    let doKdTreeIntersection (m : Model) (t : Trafo3d) (r : FastRay3d) = 
        m.surfaces 
            |> HSet.map (fun d -> d.kdTree, d.surface.name) 
            |> HSet.map (
                fun (kd,n) ->            
                    Log.warn "do intersection for %s" n
                    let kdi = kd |> KdIntersectionTree
                    let mutable hit = ObjectRayHit.MaxRange
                    let objFilter  a b = true
                    let hitFilter a b c d = false
                    if kdi.Intersect(r, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then 
                        Log.warn "hit for %s" n
                        Some (hit.RayHit.T, n)
                    else
                        Log.error "no hit for %s" n
                        None //Double.MaxValue, n
                )
            |> HSet.toSeq
            |> Seq.filter(fun d -> d.IsSome)
            |> Seq.map(fun d -> d.Value)
            |> Seq.sortByDescending(fun (t,n)-> t)
            |> Seq.tryHead

    let stash (model : Model) =
        { model with past = Some model.drawing; future = None }
    
    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (bc : BlockingCollection<string>) (m : Model) (msg : Action) =
        
        match msg, m.interaction, m.picking with
            | NavigationMessage  msg,_,false -> 
                { m with scene = { m.scene with navigation = Navigation.update m.scene.navigation msg }}                            
            | DrawingMessage msg, Interactions.DrawAnnotation,_-> 
                //let m = 
                //    match msg with
                //        | Drawing.Action.Select _ ->
                //            { m with properties = 
                //                        match m.drawing.selectedAnnotation with 
                //                            | Some a -> Properties.AnnotationProperties a
                //                            | None -> Properties.NoProperties } 
                //        | _ -> m
                
                { m with drawing = DrawingApp.update w bc m.drawing msg } |> stash
            | SelectSurface id,_,_ ->
                let surf = m.scene.surfaces |> Seq.tryFind(fun x -> x.guid = id)
                let surf =
                    match surf, m.selectedSurface with
                        | Some a, Some b -> if a.guid = b.guid then None else Some a
                        | Some a, None   -> Some a
                        | None, _        -> None

                { m with selectedSurface = surf }
            | FlyToSurface id,_,_ ->
                lookAtSurface m id
            | SurfacePropertiesMessage msg,_,_ ->
                match m.selectedSurface with 
                    | Some s -> 
                        let s' = SurfaceProperties.update s msg

                        let surfaces' = 
                            m.scene.surfaces |> List.updateIf (fun x -> x.guid = s'.guid) (fun x -> s')

                        let sgSurfaces' =
                            m.surfaces 
                                |> HSet.toList 
                                |> List.updateIf (fun x -> x.surface.guid = s'.guid) (fun x -> { x with surface = s'})
                                |> HSet.ofList

                        {m with surfaces = sgSurfaces'; selectedSurface = Some s'; scene = {m.scene with surfaces = surfaces'}}
                    | None -> m
            | AnnotationMessage msg,_,_ ->                
                    match m.drawing.selectedAnnotation with
                        | Some a -> 
                            let a' = AnnotationProperties.update a msg
                            let annotations' = 
                                m.drawing.annotations.AsList |> List.updateIf (fun x -> x.key = a'.key) (fun x -> a')                                     
                            { m with drawing = { m.drawing with selectedAnnotation = Some a'; annotations = annotations' |> PList.ofList }}

                        | None -> m                                                
            | ImportSurface,_,_ -> 
                match Dialogs.openFolder w with
                    | [] -> m
                    | paths ->
                        let path = paths.Head
                        if (path |> OpcUtils.isSurfaceFolder) then                            
                            Surface.import' runtime signature [Surface.mk path] m |> lookAtData
                        else
                            Log.error "path %s does not point to surface folder" path
                            m   
            | ImportMeasurements,_,_ ->
                match Dialogs.openDialog w with
                    | Some x -> { m with drawing = {m.drawing with annotations = m.drawing.annotations |> PList.append (MeasurementsImporter.startImporter x)}}
                    | None -> m   
            | QuickLoad1,_,_ ->
                //loadScene "D:\_WORK\Aardwork\Exomars\Dingo\MSL_Mastcam_Sol_540_id_47621_X\dingo" m runtime signature
                let path = @"D:\Aardwork\Exomars\cd - Copy\Surface\Cape_Desire_RGB"
                Surface.import' runtime signature [Surface.mk path] m |> lookAtData
            | QuickLoad2,_,_ ->                
                //loadScene "D:\_WORK\Aardwork\Exomars\Dingo\MSL_Mastcam_Sol_540_id_47621_X\dingo" m runtime signature
                (MeasurementsImporter.startImporter "D:\_WORK\Aardwork\Exomars\Cape Desire\cd.xml") |> ignore
                //let path = @"D:\Aardwork\Exomars\cd - Copy\Surface\cape.scn"
                //loadScene path m runtime signature
                m
            | DeleteLast,_,_ -> 
                File.Delete("./last") |> ignore
                m
            | PickSurface (p,name), _ ,true ->                
                Log.line "hit surface %s" name
                let fray = p.globalRay.Ray
                let r = fray.Ray

                let cameraLocation = m.scene.navigation.camera.view.Location 

                let hitF (p : V3d) = 
                    let ray = FastRay3d(cameraLocation,(p-cameraLocation).Normalized)
                    //let mutable a = 0
                    //for i in 0 .. 60000000 do a <- a + 1
                    //printfn "%A" 0
                    match doKdTreeIntersection m Trafo3d.Identity ray with
                        | Some (t,n) -> ray.Ray.GetPointOnRay t |> Some
                        | None -> None
                                    
                match doKdTreeIntersection m Trafo3d.Identity fray with
                    | Some (t,n) -> 
                        let hit = r.GetPointOnRay(t)
                        Log.line "[PickSurface] %s %A" n hit
                        
                        match m.interaction with
                            | Interactions.DrawAnnotation ->
                                let drawing = DrawingApp.update w bc m.drawing (Drawing.Action.AddPointAdv(hit,hitF,n))
                                { m with drawing = drawing } |> stash
                            | Interactions.PlaceCoordinateSystem -> 
                                let refSystem = 
                                    ReferenceSystemApp.update m.scene.config.coordinates (ReferenceSystemApp.Action.InferCoordSystem(hit))
                                //(m, MScene. *. Config.Coordinates) <=== refSystem
                                { m with scene = { m.scene with config = { m.scene.config with coordinates = refSystem }}}
                            | Interactions.PickExploreCenter ->
                                let navigation = 
                                    Navigation.update m.scene.navigation (Navigation.Action.ArcBallAction(ArcBallController.Message.Pick hit))
                                { m with scene = {m.scene with navigation = navigation; } }
                            | _ -> m
                    | None -> 
                        Log.error ("no hit")
                        m
            | SaveScene, _,_ -> 
                match m.scene.scenePath with
                    | None ->                        
                        match Dialogs.saveDialog w with
                            | None -> m
                            | Some p -> 
                                Serialization.save "./last" m.scene |> ignore
                                { m with scene = Serialization.save p { m.scene with scenePath = Some p} }
                    | Some path ->
                        Serialization.save "./last" m.scene |> ignore
                        { m with scene = Serialization.save path m.scene }
            | SaveAs,_,_ ->
                match Dialogs.saveDialog w with
                    | None -> m
                    | Some p -> 
                        Serialization.save "./last" m.scene |> ignore
                        { m with scene = Serialization.save p { m.scene with scenePath = Some p} }
            | OpenScene,_,_ ->
                match Dialogs.openDialog w with
                    | None -> m
                    | Some path -> loadScene path m runtime signature
            | NewScene,_,_ ->
                PRo3D.Viewer.Viewer.initial     //   { m with surfaces = hset.Empty ; scene = { m.scene with surfaces = [] }}
            | KeyDown k, _, _ ->
                    let a =
                        match k with
                            | Aardvark.Application.Keys.Enter       -> Drawing.Action.Finish
                            | Aardvark.Application.Keys.LeftCtrl    -> Drawing.Action.StartDrawing
                            | Aardvark.Application.Keys.D0          -> Drawing.Action.SetSemantic Semantic.Horizon0
                            | Aardvark.Application.Keys.D1          -> Drawing.Action.SetSemantic Semantic.Horizon1
                            | Aardvark.Application.Keys.D2          -> Drawing.Action.SetSemantic Semantic.Horizon2
                            | Aardvark.Application.Keys.D3          -> Drawing.Action.SetSemantic Semantic.Horizon3       
                            | _                                     -> Drawing.Action.Nop
                    let picking =
                        match k with 
                            | Aardvark.Application.Keys.LeftCtrl -> true
                            |_                                   -> false
         
                    let m =
                        match m.interaction with
                            | Interactions.DrawAnnotation -> { m with drawing = DrawingApp.update w bc m.drawing a; picking = picking } |> stash
                            | Interactions.PickExploreCenter -> {m with picking = picking}
                            | Interactions.PlaceCoordinateSystem -> {m with picking = picking}
                            | _ -> failwith "interaction does not exist"

                    m 

            | KeyUp k, _,_ ->
                    let d =
                        match k with
                            | Aardvark.Application.Keys.LeftCtrl -> DrawingApp.update w bc m.drawing Drawing.Action.StopDrawing
                            | _ -> m.drawing
                    
                    { m with drawing = d; picking = false } |> stash
            | SetInteraction t,_,_ -> { m with interaction = t }    
            | ReferenceSystemMessage a,_,_ ->                 
                let m = 
                    { m with scene = { m.scene with config = { m.scene.config with coordinates = ReferenceSystemApp.update m.scene.config.coordinates a }}}

                let m =
                    match a with 
                        | ReferenceSystemApp.Action.SetUp k->
                            let cam = m.scene.navigation.camera
                            //let view' = cam.view |> CameraView.withUp m.scene.config.coordinates.up.value 
                            let view' = CameraView.lookAt cam.view.Location (cam.view.Location + cam.view.Forward) m.scene.config.coordinates.up.value
                            let cam' = { cam with view = view' }
                        
                            (Model.Lens.scene |. Scene.Lens.navigation |. NavigationModel.Lens.camera).Set(m, cam')                            
                     
                        | _ -> m

                m
            | ConfigPropertiesMessage a,_,_ -> 
                Log.line "config message %A" a
                let c' = ConfigProperties.update m.scene.config a
                { m with 
                    scene   = { m.scene with 
                                    config          = c'                                   
                                    navigation      = { m.scene.navigation with config = c' }} //; camera = { m.scene.navigation.camera with sensitivity = c'.navigationSensitivity.value} } }
                    drawing = { m.drawing with config = c' } } 
            | _ -> m

    let scene (m : MModel) =
        let opcs =
            aset {                
                for surface in m.surfaces do
                   
                    let isSelected = 
                        Mod.bind2 (fun guid (selected: Option<MSurface>) ->
                            match selected with
                                | Some s -> Mod.map (fun selectedGuid -> selectedGuid = guid) s.guid
                                | None -> Mod.constant false
                        ) surface.surface.guid m.selectedSurface

                    let createSg (sg : ISg) =
                        sg 
                        |> Sg.noEvents 
                        |> Sg.cullMode(surface.surface.cullMode) 
                        |> Sg.fillMode(surface.surface.fillMode)                         

                    let! bb = surface.globalBB
                    let sg = 
                        Mod.map createSg surface.sceneGraph 
                        |> Sg.dynamic          
                        |> Sg.uniform "selected" isSelected
                        |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))
                     //   |> Sg.uniform "LodVisEnabled" (Mod.constant true)                        
                        |> Sg.onOff(surface.surface.isVisible)                             
                        |> Sg.LodParameters( 
                            adaptive {
                                let! frustum = Navigation.UI.frustum m.scene.navigation
                                let sizes = V2i(1024,768)
                                let! quality = surface.surface.quality.value

                                return { frustum = frustum; size = sizes; factor = quality }
                            })                            
                        |> Sg.noEvents
                        |> Sg.andAlso (
                            Sg.wireBox' C4b.Red bb
                                    |> Sg.noEvents
                                    |> Sg.noEvents
                                    |> Sg.shader {
                                            do! Shader.stableTrafo
                                            do! DefaultSurfaces.vertexColor                                                                     
                                        }
                            ) 
                        |> Sg.pickable (PickShape.Box bb) 
                        |> Sg.withEvents [
                                SceneEventKind.Down, (
                                    fun sceneHit -> 
                                        true, Seq.ofList [PickSurface (sceneHit,(surface.surface.name |> Mod.force))])
                                //Sg.onMouseDownEvt (fun sceneHit -> PickSurface sceneHit)
                            ]                        
                    yield sg
            }
        opcs 
            |> Sg.set
            |> Sg.effect [
                Shader.stableTrafo |> toEffect
                //DefaultSurfaces.constantColor C4f.White |> toEffect                            
                DefaultSurfaces.diffuseTexture |> toEffect
               // Shader.selectionColor |> toEffect       
                Shader.LoDColor |> toEffect
            ]

            |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
            |> Sg.uniform "LodVisEnabled" (m.scene.config.lodColoring)

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

        let renderControlAttributes (model : MNavigationModel) =
            amap {
                let! state = model.navigationMode
                match state with
                    | NavigationMode.FreeFly -> 
                        yield! CameraController.extractAttributes model.camera Navigation.Action.FreeFlyAction (Navigation.UI.frustum model)
                    | NavigationMode.ArcBall -> 
                        yield! ArcBallController.extractAttributes model.camera Navigation.Action.ArcBallAction (Navigation.UI.frustum model)
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap |> AttributeMap.mapAttributes (AttributeValue.map NavigationMessage)       

        let renderControl =
            Incremental.renderControl
                    (Mod.map2 Camera.create m.scene.navigation.camera.view (Navigation.UI.frustum m.scene.navigation)) //(Frustum.perspective 60.0 m.scene.config.nearPlane.value 100.0 1.0 |> Mod.constant)
                    (AttributeMap.unionMany [
                        renderControlAttributes m.scene.navigation
                        [attribute "style" "width:70%; height: 100%; float:left"; ] |> AttributeMap.ofList
                        AttributeMap.ofList [
                            onKeyDown (KeyDown)
                            onKeyUp (KeyUp)
                            attribute "style" "width:70%; height: 100%; float:left"; ] 
                    ]) 
                    (                                             
                        let annotations =
                            DrawingApp.Sg.view m.drawing m.scene.navigation.camera.view
                                |> Sg.map DrawingMessage
                                |> Sg.fillMode (Mod.constant FillMode.Fill)//m.rendering.fillMode
                                |> Sg.cullMode (Mod.constant CullMode.None)//m.rendering.cullMode

                        let orientations =
                            ReferenceSystemApp.Sg.view m.scene.config.coordinates m.scene.navigation.camera.view

                        let exploreCenter =
                            Navigation.Sg.view m.scene.navigation

                        [(scene m); annotations; orientations; exploreCenter] //; 
                            |> Sg.ofList
                         //   |> Sg.fillMode (Mod.constant FillMode.Fill)
                         //   |> Sg.cullMode (Mod.constant CullMode.None)      
                    )              

        let sceneTools =
            div [clazz "ui buttons inverted"] [
                button [clazz "ui icon button"; onMouseClick (fun _ -> SaveScene)] [
                            i [clazz "large save icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> SaveAs)] [
                    i [clazz "large icons"] [
                        i [clazz "save icon"] [] 
                        i [clazz "top right corner arrow down icon"] [] 
                    ]
                ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> OpenScene)] [
                            i [clazz "large folder outline icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> NewScene)] [
                            i [clazz "large file outline icon"] [] ]
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
                (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto;max-height: 30%"]) (
                    alist {
                        for s in (m.surfaces |> AList.ofASet) do

                            let! selected = m.selectedSurface
                            let! color =
                                match selected with
                                    | Some sel -> 
                                        sel.guid |> Mod.map (fun g -> if g = (s.surface.guid |> Mod.force) then C4b.Blue else C4b.White)
                                    | None -> Mod.constant C4b.White
                            
                            let bgc = sprintf "color: %s" (Html.ofC4b color)

                            yield div [clazz "item"] [
                                i [
                                    clazz "large cube middle aligned icon"; 
                                    style bgc
                                    onClick (fun x -> SelectSurface (s.surface.guid |> Mod.force))
                                ][]
                                div [clazz "content"] [
                                    div [clazz "header"] [
                                        Incremental.text s.surface.name
                                        text " "
                                        i [
                                            clazz "home icon"; 
                                            onClick (fun x -> FlyToSurface (s.surface.guid |> Mod.force))
                                        ][]     
                                    ]
                                    div [clazz "description"] [
                                        Incremental.text (
                                            s.globalBB 
                                                |> Mod.map (fun d -> sprintf "Size: %.3f m" (d.Size.Length)))
                                    ]
                                ]
                            ]
                    }
            )
            
        let surfaceProperties (model:MModel) =
            adaptive {
                let! bla = model.selectedSurface
                let opt = 
                    match bla with
                        | Some s -> SurfaceProperties.view s
                        | None   -> div[][]

                return (opt |> UI.map SurfacePropertiesMessage)
            }

        let surfaceUI =
            div[][
                button [onClick (fun _ -> QuickLoad1)][text "Surface"]
                button [onClick (fun _ -> QuickLoad2)][text "Scene"]
                button [onClick (fun _ -> DeleteLast)][text "Delete Last"]
                GuiEx.accordion "Surfaces" "Cubes" true [
                    surfaceList
                ]
                GuiEx.accordion "Properties" "Content" true [
                     Incremental.div AttributeMap.empty (AList.ofModSingle (surfaceProperties m))
                ]
            ]

        let annotationProperties (model:MModel) =
            adaptive {
                let! bla = model.drawing.selectedAnnotation
                let opt = 
                    match bla with
                        | Some a -> AnnotationProperties.view a
                        | None   -> div[][]

                return (opt |> UI.map AnnotationMessage)
            } 

        let dns (model:MModel) =
            adaptive {
                let! bla = model.drawing.selectedAnnotation
                let opt = 
                    match bla with
                        | Some a -> DipAndStrike.view a
                        | None   -> div[][]

                return (opt |> UI.map DnSProperties)
            } 

        let annotationUI = 
            div [][
                DrawingApp.UI.viewAnnotationImportExport |> UI.map DrawingMessage
                
                GuiEx.accordion "Annotations" "Write" true [
                    DrawingApp.UI.viewAnnotations m.drawing |> UI.map DrawingMessage
                ]
                GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(annotationProperties m))
                ] 
                GuiEx.accordion "Dip&Strike" "Calculator" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(dns m))] 
            ]

        let config (model:MModel) =
            adaptive {
                let opt = ConfigProperties.view model.scene.config
                return (opt |> UI.map ConfigPropertiesMessage)
            } 

        let configUI =
            div[][
                GuiEx.accordion "ViewerConfig" "Write" true [
                     Incremental.div AttributeMap.empty (AList.ofModSingle (config m))
                ]

                GuiEx.accordion "Coordinate System" "Map Signs" false [
                    ReferenceSystemApp.UI.view m.scene.config.coordinates |> UI.map ReferenceSystemMessage
                ]

                GuiEx.accordion "Camera" "Camera Retro" false [
                    CameraProperties.view m.scene.navigation.camera
                ]
            ]
            
            
        let tabs = GuiEx.tabbed [clazz "ui inverted segment"] [
                            ("Data", surfaceUI)
                            ("Annotations", annotationUI)
                            ("Config", configUI)
                        ] "Data" 
      
        let topMenu = [
             Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large wizard icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown m.interaction SetInteraction ]                
             ]

             Navigation.UI.viewNavigationModes m.scene.navigation  |> UI.map NavigationMessage 
             DrawingApp.UI.viewAnnotationToolsHorizontal m.drawing |> UI.map DrawingMessage
        ] 
                     
        let rest (model : MModel) = 
            [             
                renderControl
                div [clazz "ui"; style "width: 30%; float:right; background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden"] [
                    div[style "height:41px"][text "spacer"]
                    tabs                    
                ]            
            ]
        
        let usingContext =             
            "$('.context.example .ui.sidebar')
              .sidebar({
                context: $('.context.example .bottom.segment.blarg')
              })
              .sidebar('attach events', '.context.example .menu.blarg .item.blarg')
              .sidebar('setting', 'transition', 'Overlay')
            ;"

        //Sidebar adorner
        let myCss = { kind = Stylesheet; name = "semui-overrides"; url = "./semui-overrides.css" }

        require (Html.semui @ [myCss]) (
            let sections = [
                "blank", [div[style "height:15px"][]]                
                "Scene", [sceneTools]
                "Add Surface", [openSurface]
                "Import Measurements", [importMeasurements]]                              
            onBoot usingContext (                            
                body [clazz "context example"] (GuiEx.adornerMenu sections topMenu (rest m))                                  
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
     
        let drawing =
            DrawingApp.threads m.drawing |> ThreadPool.map DrawingMessage
       
        let nav =
            match m.scene.navigation.navigationMode with
                | NavigationMode.FreeFly -> 
                    CameraController.threads m.scene.navigation.camera
                        |> ThreadPool.map Navigation.FreeFlyAction |> ThreadPool.map NavigationMessage
                | NavigationMode.ArcBall ->
                    ArcBallController.threads m.scene.navigation.camera
                        |> ThreadPool.map Navigation.ArcBallAction |> ThreadPool.map NavigationMessage
                | _ -> failwith "invalid nav mode"
                    
        ThreadPool.union drawing nav

    let start (w : Form) (runtime: IRuntime) (signature: IFramebufferSignature) bc =
        let m = PRo3D.Viewer.Viewer.initial        
        let m = Surface.loadLastScene m runtime signature        
       // let path = @"D:\Aardwork\Exomars\cd - Copy\Surface\Cape_Desire_RGB"
        //let m = Surface.import' runtime signature [Surface.mk path] m |> lookAtData
        
        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view
            update = update w runtime signature bc
            initial = m
        }