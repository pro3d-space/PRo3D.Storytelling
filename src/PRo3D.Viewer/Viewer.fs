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

open PRo3D.Navigation2
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Viewer

module PList =
    let append (a : plist<_>) (b : plist<_>) =
        let rec doIt xs =
            match xs with
                | x::xs -> PList.prepend x (doIt xs)
                | [] -> b
        doIt (PList.toList a)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scene =
    open PRo3D.Surfaces.Surface
    open Aardvark.UI.Incremental.Svg
    
    let surfaceModelLens = Model.Lens.scene |. Scene.Lens.surfacesModel

    let expandRelativePaths (m:Scene) =
        // Surfaces        
        
        match m.scenePath with
            | Some p ->         
                let p' = Path.Combine((Path.GetDirectoryName p),"Surfaces")
                let surfaces = 
                    m.surfacesModel.surfaces                     
                    |> List.map(
                        fun x -> 
                            if x.relativePaths then
                                let p'' = Path.Combine(p', x.name)
                                { x with opcPaths = expandNamesToPaths p'' x.opcNames }
                            else x
                        )
                { m with surfacesModel = Surface.updateSurfaces m.surfacesModel surfaces}
            | None -> m        

    let readLine (filePath:string) =
            use sr = new System.IO.StreamReader (filePath)
            sr.ReadLine ()       

    let fileExists filePath =
        match System.IO.File.Exists filePath with
            | true -> Some filePath
            | false -> None
    
    /// appends surfaces to existing surfaces
    let import' (runtime : IRuntime) (signature: IFramebufferSignature)(surfaces : list<Surface>) (m : Model) =
            
        //add legacy trafo on import
        let surfaces =
            surfaces 
                |> List.map(fun s -> s,Path.ChangeExtension(s.importPath, ".trafo"))
                |> List.map(fun(s,p) -> 
                        match (fileExists p) with
                            | Some path->
                                let t = readLine path
                                { s with trafo = Trafo3d.Parse(t) }
                            | None ->
                                s
                            )

        let getOPCxPath (surfacePath : string) = 
            let name = Path.GetFileName surfacePath
            let ocp = Path.ChangeExtension(Path.Combine(surfacePath, name), ".opcx")
            printf "%s" ocp
            ocp

        let surfaces =
            surfaces 
                |> List.map(fun s -> s, s.importPath |> getOPCxPath)
                |> List.map(fun(s,p) -> 
                        match (fileExists p) with
                            | Some path->                                
                                { s with attributeLayers = Surface.SurfaceAttributes.read path |> PList.ofSeq }
                            | None ->
                                s
                            )
                        
        //append semantic surfaces
        let surfaces' = (surfaces @ m.scene.surfacesModel.surfaces)
        let m = (surfaceModelLens |. SurfaceModel.Lens.surfaces).Set(m, surfaces')
        
        //append sg surfaces
        let surfSet = Surface.createSgSurfaces runtime signature surfaces
        let sgSurfaces = HMap.union surfSet m.scene.surfacesModel.sgSurfaces |> expandLazyKdTreePaths m.scene.scenePath
                                       
        let m = (surfaceModelLens |. SurfaceModel.Lens.sgSurfaces).Set(m, sgSurfaces)

        let grouped = m.scene.surfacesModel |> SurfaceApp.updateGrouping
        surfaceModelLens.Set(m, grouped)
        //m

    /// loads surfaces given in scene - creates their sg pendants
    let load (runtime : IRuntime) (signature: IFramebufferSignature)(s : Scene) (m : Model) =
        
        let s = s |> expandRelativePaths

        let sgSurfaces = Surface.createSgSurfaces runtime signature s.surfacesModel.surfaces |> expandLazyKdTreePaths m.scene.scenePath
        let m = (surfaceModelLens |. SurfaceModel.Lens.sgSurfaces).Set(m, sgSurfaces)

        let grouped = m.scene.surfacesModel |> SurfaceApp.updateGrouping
        surfaceModelLens.Set(m, grouped)            

    let loadScene path m runtime signature =
        let s = { (Serialization.loadAs<Scene> path) with scenePath = Some path }
        let m = { m with scene = s }

        load runtime signature s m
            
    let loadLastScene m runtime signature =
        match fileExists "./recent" with
            | Some path  ->
              try 
                let r = Serialization.loadAs<Recent> path
                let m = { m with recent = r}
                match r.recentScenes |> HMap.toList |> List.sortByDescending (fun (_,v) -> v.writeDate) |> List.tryHead with
                    | Some (_,scenePath) ->
                        try
                            loadScene scenePath.path m runtime signature        
                        with
                            | _ ->
                                Log.warn "Error parsing last scene: %s. loading empty" scenePath.path
                                m
                    | None -> 
                        Log.warn("No recent scene found")
                        m
                //m
                with e ->
                    Aardvark.Base.Log.warn "%A. deleting recent." e
                    System.IO.File.Delete path
                    m
            | _ -> 
                Log.warn("recent scene file does not exist (yet)")
                m
 
module ViewerApp =
    open Surface   
    open System.Collections.Concurrent   
    open Scene    
    open Aardvark.SceneGraph.SgFSharp.Sg
        
    type PropertyActions =
        | DrawingMessage    of Drawing.Action
        | AnnotationMessage of AnnotationProperties.Action

    type Action =                
        | DrawingMessage            of Drawing.Action
        | AnnotationGroupsMessage   of AnnotationGroups.Action
        | NavigationMessage         of Navigation.Action
        | ReferenceSystemMessage    of ReferenceSystemApp.Action
        | AnnotationMessage         of AnnotationProperties.Action
        
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
        | LoadScene                 of string
        | NewScene
        | KeyDown                   of key : Aardvark.Application.Keys
        | KeyUp                     of key : Aardvark.Application.Keys      
        | SetInteraction            of Interactions        
        
        | SurfaceActions            of SurfaceApp.Action
    
    let _surfacesModel   = Model.Lens.scene |. Scene.Lens.surfacesModel
    let _surfaces        = _surfacesModel   |. SurfaceModel.Lens.surfaces
    let _surfaces2       =  Model.Lens.scene |. Scene.Lens.surfacesModel |. SurfaceModel.Lens.surfaces    
    let _sgSurfaces      = _surfacesModel   |. SurfaceModel.Lens.sgSurfaces
    let _selectedSurface = _surfacesModel   |. SurfaceModel.Lens.selectedSurface
    
    let _drawing = Model.Lens.drawing
    let _camera= Model.Lens.scene |. Scene.Lens.navigation |. NavigationModel.Lens.camera
       
    let lookAtData (m : Model) =         
        let bb = _sgSurfaces.Get(m) |> HMap.toSeq |> Seq.map(fun (_,x) -> x.globalBB) |> Box3d.ofSeq
        let cam = 
            let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value
            { CameraController.initial with view = view }            

        { m with scene = { m.scene with navigation = {m.scene.navigation with camera = cam} }}
    
    let lookAtSurface (m : Model) id =
        let surf = _sgSurfaces.Get(m) |> HMap.tryFind id
        match surf with
            | Some s ->
                let bb = s.globalBB
                let cam = 
                    let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value
                    { CameraController.initial with view = view }            

                //{ m with scene = { m.scene with navigation = {m.scene.navigation with camera = cam} }}
                (Model.Lens.scene |. Scene.Lens.navigation |. NavigationModel.Lens.camera).Set(m, cam)
            | None -> m
                    
    let stash (model : Model) =
        { model with past = Some model.drawing; future = None }

    let addToListWithoutDup e f list =
        match List.exists f list with
            | true -> list
            | false -> e :: list
    
    let stashAndSaveRecent (s : SceneHandle) (model : Model) =
        let scenePaths = 
            model.recent.recentScenes                 
                |> HMap.update s.path 
                    (
                        fun v -> 
                            match v with
                                | Some x -> { x with writeDate = s.writeDate }
                                | None -> s
                    ) //addToListWithoutDup s (fun x -> x.path = s.path )                

        let recent = { model.recent with recentScenes = scenePaths }

        Serialization.save "./recent" recent |> ignore

        { model with recent = recent }

    let stashAndSaveRecent' (path : string) (model : Model) =
        let handle = {
            name = Path.GetFileName(path)
            path = path
            writeDate = DateTime.Now
        }
        stashAndSaveRecent handle model

    let refConfig : ReferenceSystemConfig<ViewConfigModel> =
        { 
                arrowLength = ViewConfigModel.Lens.arrowLength |. NumericInput.Lens.value 
                arrowThickness = ViewConfigModel.Lens.arrowThickness |. NumericInput.Lens.value 
        }
    
    let drawingConfig : DrawingApp.SmallConfig<ReferenceSystem> =
        { 
            up    = (ReferenceSystem.Lens.up |. V3dInput.Lens.value)
            north = ReferenceSystem.Lens.north
        }

    let navConf : Navigation.smallConfig<ViewConfigModel, ReferenceSystem> =
        {
            navigationSensitivity = ViewConfigModel.Lens.navigationSensitivity |. NumericInput.Lens.value
            up                    = ReferenceSystem.Lens.up |. V3dInput.Lens.value
        }

    let updateCameraUp m =
        let cam = m.scene.navigation.camera                            
        let view' = CameraView.lookAt cam.view.Location (cam.view.Location + cam.view.Forward) m.scene.referenceSystem.up.value
        let cam' = { cam with view = view' }
        _camera.Set(m,cam')

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (bc : BlockingCollection<string>) (m : Model) (msg : Action) =
        
        match msg, m.interaction, m.picking with
            | NavigationMessage  msg,_,false -> 
               
                let c   = m.scene.config
                let ref = m.scene.referenceSystem

                { m with scene = { m.scene with navigation = Navigation.update c ref navConf m.scene.navigation msg }}
            | AnnotationGroupsMessage msg,_,_ ->
                let ag = m.scene.groups
                { m with scene = { m.scene with groups = AnnotationGroups.update ag msg}}
            | DrawingMessage msg, Interactions.DrawAnnotation,_->
                { m with drawing = DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing msg } |> stash
            | SurfaceActions msg,_,_ ->
                match msg with
                    | SurfaceApp.Action.FlyToSurface id -> 
                        lookAtSurface m id
                    | _ ->  
                        Log.warn "%A" msg
                        let s = SurfaceApp.update m.scene.surfacesModel msg m.scene.scenePath
                        { m with scene = { m.scene with surfacesModel = s}}
            | AnnotationMessage msg,_,_ ->                
                    match m.drawing.selectedAnnotation with
                        | Some a -> 
                            let a' = AnnotationProperties.update a msg
                            let annotations' = 
                                m.drawing.annotations.AsList |> List.updateIf (fun x -> x.key = a'.key) (fun x -> a')       
                            
                            let drawing' = 
                                { m.drawing with selectedAnnotation = Some a'; annotations = annotations' |> PList.ofList }

                            { m with drawing = drawing' }
                        | None -> m                                                
            | ImportSurface,_,_ -> 
                match Dialogs.openFolders w with
                    | [] -> m
                    | paths ->
                        let surfaces = 
                            paths 
                                |> List.filter isSurfaceFolder 
                                |> List.map Surface.mk

                        Scene.import' runtime signature surfaces m
                        //if (path |> Utils.isSurfaceFolder) then                     
                        //    Scene.import' runtime signature [Surface.mk path] m //|> lookAtData
                        //else
                        //    Log.error "path %s does not point to surface folder" path
                        //    m   
            | ImportMeasurements,_,_ ->
                match Dialogs.openDialog w with
                    | Some x -> { m with drawing = {m.drawing with annotations = m.drawing.annotations |> PList.append (MeasurementsImporter.startImporter x)}}
                    | None -> m   
            | QuickLoad1,_,_ ->
                Scene.loadScene @"E:\Aardwork\Exomars\_NEWVIEWER\Scenes\TestScene\scene.scn" m runtime signature     
            | QuickLoad2,_,_ ->                                
                (MeasurementsImporter.startImporter "E:\Aardwork\Exomars\_SCENES\cd - Copy\cd.xml") |> ignore                
                m
            | DeleteLast,_,_ -> 
                File.Delete("./last") |> ignore
                m
            | PickSurface (p,name), _ ,true ->  
               // Log.line "hit surface %s" name
                let fray = p.globalRay.Ray
                let r = fray.Ray
                Log.startTimed "kdtree intersect %s" name
                let cameraLocation = m.scene.navigation.camera.view.Location 

                let hitF (p : V3d) = 

                    let ray =
                        match m.drawing.projection with
                            | Projection.Viewpoint -> 
                                let dir = (p-cameraLocation).Normalized
                                FastRay3d(cameraLocation, dir)  
                            | Projection.Sky -> 
                                let up = m.scene.referenceSystem.up.value
                                FastRay3d(p + (up * 5000.0), -up)  
                            | _ -> Log.error "projection started without proj mode"; FastRay3d()
                    
                    match SurfaceApp.doKdTreeIntersection (_surfacesModel.Get(m)) Trafo3d.Identity ray with
                        | Some (t,surf) ->                             
                            ray.Ray.GetPointOnRay t |> Some
                        | None -> None
                                    
                match SurfaceApp.doKdTreeIntersection (_surfacesModel.Get(m)) Trafo3d.Identity fray with
                    | Some (t,surf) ->                         
                        let hit = r.GetPointOnRay(t)
                        
                        match m.interaction with
                            | Interactions.DrawAnnotation ->                               
                                let drawing = DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing (Drawing.Action.AddPointAdv(hit,hitF,surf.name))
                                Log.stop()
                                { m with drawing = drawing } |> stash
                            | Interactions.PlaceCoordinateSystem -> 
                                //let refsystem' = ReferenceSystemApp.update m.scene.config innerConfig m.scene.referenceSystem a
                                let refAction = ReferenceSystemApp.Action.InferCoordSystem(hit)
                                let (refSystem',_)= 
                                    ReferenceSystemApp.update m.scene.config refConfig (m.scene.referenceSystem) refAction  
                                                                
                                let m = { m with scene = { m.scene with referenceSystem = refSystem' }}

                                //update camera upvector
                                updateCameraUp m
                            | Interactions.PickExploreCenter ->
                                let c   = m.scene.config
                                let ref = m.scene.referenceSystem
                                let navigation' = 
                                    Navigation.update c ref navConf m.scene.navigation (Navigation.Action.ArcBallAction(ArcBallController.Message.Pick hit))
                                { m with scene = {m.scene with navigation = navigation'; } }
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
                                let s' = { m.scene with scenePath = Some p}
                                { m with scene = Serialization.save p s'; } |> stashAndSaveRecent' p
                    | Some path ->                        
                        { m with scene = Serialization.save path m.scene } |> stashAndSaveRecent' path
            | SaveAs,_,_ ->
                match Dialogs.saveDialog w with
                    | None -> m
                    | Some p -> 
                        let s' = { m.scene with scenePath = Some p}                                
                        { m with scene = Serialization.save p s' } |> stashAndSaveRecent' p
            | OpenScene,_,_ ->
                match Dialogs.openDialog w with
                    | None -> m
                    | Some path -> 
                        Scene.loadScene path m runtime signature |> stashAndSaveRecent' path
            | LoadScene path,_,_ ->
                Scene.loadScene path m runtime signature |> stashAndSaveRecent' path
            | NewScene,_,_ ->
                { PRo3D.Viewer.Viewer.initial with recent = m.recent }
            | KeyDown k, _, _ ->
                    let drawingAction =
                        match k with
                            | Aardvark.Application.Keys.Enter       -> Drawing.Action.Finish
                            | Aardvark.Application.Keys.LeftCtrl    -> Drawing.Action.StartDrawing
                            | Aardvark.Application.Keys.D0          -> Drawing.Action.SetSemantic Semantic.Horizon0
                            | Aardvark.Application.Keys.D1          -> Drawing.Action.SetSemantic Semantic.Horizon1
                            | Aardvark.Application.Keys.D2          -> Drawing.Action.SetSemantic Semantic.Horizon2
                            | Aardvark.Application.Keys.D3          -> Drawing.Action.SetSemantic Semantic.Horizon3       
                            | _                                     -> Drawing.Action.Nop

                    let m =
                        match k with 
                            | Aardvark.Application.Keys.LeftCtrl -> { m with picking = true }
                            |_                                   -> { m with picking = false }
         
                    let m =
                        match m.interaction with
                            | Interactions.DrawAnnotation -> 
                                { m with drawing = DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing drawingAction; } |> stash
                            | _ -> m
                                                        

                    let sensitivity = 
                        m.scene.config.navigationSensitivity.value
                    
                    let configAction = 
                        match k with 
                            | Aardvark.Application.Keys.PageUp   -> ConfigProperties.Action.SetNavigationSensitivity (Numeric.Action.SetValue (sensitivity + 0.5))
                            | Aardvark.Application.Keys.PageDown -> ConfigProperties.Action.SetNavigationSensitivity (Numeric.Action.SetValue (sensitivity - 0.5))
                            | _ -> ConfigProperties.Action.SetNavigationSensitivity (Numeric.Action.SetValue (sensitivity))

                    let c' = ConfigProperties.update m.scene.config configAction
                    { m with scene = { m.scene with config = c' }}                                       
            | KeyUp k, _,_ ->
                    let d =
                        match k with
                            | Aardvark.Application.Keys.LeftCtrl -> DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing Drawing.Action.StopDrawing
                            | _ -> m.drawing
                    
                    { m with drawing = d; picking = false } |> stash
            | SetInteraction t,_,_ -> { m with interaction = t }
            | ReferenceSystemMessage a,_,_ ->                 
                                
                let refsystem',_ = ReferenceSystemApp.update m.scene.config refConfig m.scene.referenceSystem a
                let m = (Model.Lens.scene |. Scene.Lens.referenceSystem).Set(m, refsystem')

                let m =
                    match a with 
                        | ReferenceSystemApp.Action.SetUp _-> updateCameraUp m
                        | _ -> m

                m
            | ConfigPropertiesMessage a,_,_ -> 
                //Log.line "config message %A" a
                let c' = ConfigProperties.update m.scene.config a
                (Model.Lens.scene |. Scene.Lens.config).Set(m,c')
            | _ -> m

    let pickable' (pick :IMod<Pickable>) (sg: ISg) =
        Sg.PickableApplicator (pick, Mod.constant sg)

    let viewSingleSurfaceSg (surface : MSgSurface) (frustum : IMod<Frustum>) (lodcolor : IMod<bool>) (selectedSurf : IMod<Option<MSurface>>) (globalBB : IMod<Box3d>) =
        let isSelected = 
            Mod.bind2 (fun guid (selected: Option<MSurface>) ->
                match selected with
                    | Some s -> Mod.map (fun selectedGuid -> selectedGuid = guid) s.guid
                    | None -> Mod.constant false
            ) surface.surface.guid selectedSurf

        let createSg (sg : ISg) =
            sg 
            |> Sg.noEvents 
            |> Sg.cullMode(surface.surface.cullMode) 
            |> Sg.fillMode(surface.surface.fillMode)         
                
        let pickable = 
            adaptive {
                let! bb = globalBB
                return { shape = PickShape.Box bb; trafo = Trafo3d.Identity }
            }

        let sg = 
            Mod.map createSg surface.sceneGraph 
                |> Sg.dynamic          
                |> Sg.uniform "selected" isSelected
                |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))                                 
                |> Sg.onOff(surface.surface.isVisible)                             
                |> Sg.LodParameters( 
                    adaptive {
                        let! frustum = frustum 
                        let sizes = V2i(1024,768)
                        let! quality = surface.surface.quality.value
                        let! trafo = surface.surface.trafo

                        return { frustum = frustum; size = sizes; factor = Math.Pow(Math.E, quality); trafo = trafo }
                    })        
                |> pickable' pickable
                |> Sg.noEvents
                //|> Sg.andAlso (
                //    Sg.wireBox (Mod.constant C4b.VRVisGreen) globalBB
                //            |> Sg.noEvents
                //            |> Sg.noEvents
                //            |> Sg.shader {
                //                    do! Shader.stableTrafo
                //                    do! DefaultSurfaces.vertexColor                                                                     
                //                }
                //    ) 
                |> Sg.withEvents [
                        SceneEventKind.Down, (
                            fun sceneHit -> 
                                true, Seq.ofList [PickSurface (sceneHit,(surface.surface.name |> Mod.force))])
                        // Sg.onMouseDownEvt (fun sceneHit -> PickSurface (sceneHit,(surface.surface.name |> Mod.force)))
                    ]
        sg
                
    let viewSurfacesSg (sgSurfaces : amap<Guid,MSgSurface>) (frustum : IMod<Frustum>) (selectedSurf : IMod<Option<MSurface>>)(lodcolor : IMod<bool>)=
        let opcs =
            aset {
                for (_,surface) in sgSurfaces |> AMap.toASet do                   
                   yield viewSingleSurfaceSg surface frustum lodcolor selectedSurf surface.globalBB
            }

        opcs 
            |> Sg.set
            |> Sg.effect [
                Shader.stableTrafo |> toEffect
                //DefaultSurfaces.constantColor C4f.White |> toEffect                            
                DefaultSurfaces.diffuseTexture |> toEffect
                Shader.selectionColor |> toEffect       
               // Shader.LoDColor |> toEffect
            ]

            |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
            |> Sg.uniform "LodVisEnabled" lodcolor //()

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

        let frustum (m:MModel) =
            let near = m.scene.config.nearPlane.value
            let far = m.scene.config.farPlane.value
            (Navigation.UI.frustum near far)

        let renderControlAttributes (model : MNavigationModel) =
            amap {
                let! state = model.navigationMode
                
                match state with
                    | NavigationMode.FreeFly -> 
                        yield! CameraController.extractAttributes model.camera Navigation.Action.FreeFlyAction (frustum m)
                    | NavigationMode.ArcBall -> 
                        yield! ArcBallController.extractAttributes model.camera Navigation.Action.ArcBallAction (frustum m)
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap |> AttributeMap.mapAttributes (AttributeValue.map NavigationMessage)       

        let refSystemInner : ReferenceSystemApp.MInnerConfig<MViewConfigModel> =
            {
                getArrowLength    = fun (x:MViewConfigModel) -> x.arrowLength.value
                getArrowThickness = fun (x:MViewConfigModel) -> x.arrowThickness.value
                getNearDistance   = fun (x:MViewConfigModel) -> x.nearPlane.value
            }
        
        let cam = Mod.map2 Camera.create m.scene.navigation.camera.view (frustum m)

        let att = 
            AttributeMap.unionMany [
                renderControlAttributes m.scene.navigation
                [attribute "style" "width:70%; height: 100%; float:left"; ] |> AttributeMap.ofList
                AttributeMap.ofList [
                    onKeyDown (KeyDown)
                    onKeyUp (KeyUp)
                    attribute "style" "width:70%; height: 100%; float:left"; ] 
            ]

        let sconf : DrawingApp.MSmallConfig<MViewConfigModel> =
            {            
                getNearPlane       = fun (x:MViewConfigModel) -> x.nearPlane.value
                getHfov            = fun (x:MViewConfigModel) -> ((Mod.init 60.0) :> IMod<float>)
                getMaxDipAngle     = fun (x:MViewConfigModel) -> x.maxDipAngle.value
                getMinDipAngle     = fun (x:MViewConfigModel) -> x.minDipAngle.value
                getArrowThickness  = fun (x:MViewConfigModel) -> x.arrowThickness.value
                getArrowLength     = fun (x:MViewConfigModel) -> x.arrowLength.value
                getDnsPlaneSize    = fun (x:MViewConfigModel) -> x.dnsPlaneSize.value
            }
         
        let overlayed =
            let annotations =
                DrawingApp.Sg.view m.scene.config sconf m.drawing m.scene.navigation.camera.view
                    |> Sg.map DrawingMessage
                    |> Sg.fillMode (Mod.constant FillMode.Fill)//m.rendering.fillMode
                    |> Sg.cullMode (Mod.constant CullMode.None)//m.rendering.cullMode

            let refSystem =
                ReferenceSystemApp.Sg.view 
                    m.scene.config
                    refSystemInner
                    m.scene.referenceSystem                                   
                    m.scene.navigation.camera.view
                |> Sg.map ReferenceSystemMessage

            let exploreCenter =
                Navigation.Sg.view m.scene.navigation

            [            
                annotations
                refSystem
                exploreCenter
            ] |> Sg.ofList

        let surfaces2 (sgSurfaces:amap<Guid,MSgSurface>) frustum lodcolor =            
            sgSurfaces
                |> AMap.toASet                
                |> ASet.map(
                    fun (g, s) -> viewSingleSurfaceSg s frustum lodcolor m.scene.surfacesModel.selectedSurface s.globalBB, s)
                |> ASet.mapM(fun (sg,x) -> x.surface.priority.value |> Mod.map(fun prior -> (prior,sg)))
                |> ASet.groupBy fst
                |> AMap.toASet
                |> ASet.sortBy fst
                |> AList.map snd         
                |> AList.map (fun x -> x |> HSet.map snd)

        let cmds2 =
            let nestedSgs =
                surfaces2 m.scene.surfacesModel.sgSurfaces (frustum m) (m.scene.config.lodColoring)

            let grouped = 
                m.scene.surfacesModel.sgGrouped 
                    |> AList.map(
                        fun x -> 
                            (x 
                                |> AMap.map(fun g sf -> viewSingleSurfaceSg sf (frustum m) (m.scene.config.lodColoring) m.scene.surfacesModel.selectedSurface sf.globalBB) 
                                |> AMap.toASet 
                                |> ASet.map snd 
                            )
                    )
            
            alist {                
                for set in grouped do
                    let sg = 
                        set 
                            |> Sg.set
                            |> Sg.effect [
                                Shader.stableTrafo |> toEffect                  
                                DefaultSurfaces.diffuseTexture |> toEffect
                                Shader.selectionColor |> toEffect       
                                Shader.LoDColor |> toEffect
                            ]
                            |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
                            |> Sg.uniform "LodVisEnabled" m.scene.config.lodColoring //()

                    yield RenderCommand.SceneGraph sg
                    yield RenderCommand.Clear(None,Some (Mod.constant 1.0))

                yield RenderCommand.SceneGraph overlayed
            }

            //alist {
            //    for groups in surfaces2 do
            //        yield 0

            //}
                       
        let surfaces = 
            viewSurfacesSg m.scene.surfacesModel.sgSurfaces (frustum m) (m.scene.surfacesModel.selectedSurface) (m.scene.config.lodColoring)
         
        //let renderControl =
        //    Incremental.renderControl cam att 

        //surfaces |> group by priority |> (priority * sg)

        let cmds = 

            //let abc = surfaces2
            //Log.line "%A" abc

            alist {
                yield RenderCommand.SceneGraph overlayed
                yield RenderCommand.SceneGraph surfaces
            }

        let control = 
            DomNode.RenderControl(att, cam, cmds2, None)

        //let control =
        //    CameraController.withControls m.scene.navigation.camera CameraAction (frustum m) control

        let sceneTools =
            div [clazz "ui buttons inverted"] [ 
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Save Scene"; onMouseClick (fun _ -> SaveScene)] [
                                i [clazz "large save icon" ] [] ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Save Scene As"; onMouseClick (fun _ -> SaveAs)] [
                        i [clazz "large icons"] [
                            i [clazz "save icon"] [] 
                            i [clazz "top right corner arrow down icon"] [] 
                        ]
                    ]
                )
                
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Open Scene"; onMouseClick (fun _ -> OpenScene)] [
                                i [clazz "large folder outline icon" ] [] ]
                )

                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "New Scene"; onMouseClick (fun _ -> NewScene)] [
                                i [clazz "large file outline icon"] [] ]
                )
            ]
            
        let openSurface =
            [ text "browse"] |> button [
                clazz "ui button"
                attribute "data-content" "text text text"
                onClick (fun _ -> ImportSurface)
             ] 

        let importMeasurements =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> ImportMeasurements)
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

        let annotationGroupsUI =
            div [][
                GuiEx.accordion "Annotations" "Write" true [
                    AnnotationGroups.view m.scene.groups |> UI.map AnnotationGroupsMessage
                ]
            ]

        let config (model:MModel) =
            adaptive {
                let opt = ConfigProperties.view model.scene.config
                return (opt |> UI.map ConfigPropertiesMessage)
            } 

        let configUI =
            div[][
                GuiEx.accordion "ViewerConfig" "Settings" true [
                     Incremental.div AttributeMap.empty (AList.ofModSingle (config m))
                ]

                GuiEx.accordion "Coordinate System" "Map Signs" false [
                    ReferenceSystemApp.UI.view m.scene.referenceSystem |> UI.map ReferenceSystemMessage
                ]

                GuiEx.accordion "Camera" "Camera Retro" false [
                    CameraProperties.view m.scene.navigation.camera
                ]
            ]            
            
        let tabs = GuiEx.tabbed [clazz "ui inverted segment"] [
                            ("Data", SurfaceApp.surfaceUI m.scene.surfacesModel |> UI.map SurfaceActions)
                            ("Annotations", annotationGroupsUI) //  annotationGroupsUI annotationUI
                            ("Config", configUI)
                        ] "Data" 
      
        let topMenu = [
             Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large wizard icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown m.interaction SetInteraction ]                
             ]

             Navigation.UI.viewNavigationModes m.scene.navigation  |> UI.map NavigationMessage 
             DrawingApp.UI.viewAnnotationToolsHorizontal m.drawing |> UI.map DrawingMessage

             button [onClick(fun _ -> QuickLoad1)] [text "quickload"]
             //button [onClick (fun _ -> DeleteLast)][text "Delete Last"]
        ] 
                     
        let rest (model : MModel) = 
            [             
                control
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

        let recentStuff =
             div[] [
                    onBoot """$('#__ID__').popup({inline:true,hoverable:true, position   : 'right center'});""" (
                            button [
                                clazz "ui small button"; style "margin: 2px 0"                                
                             ] [text "Scenes"]
                    )

                    div [clazz "ui flowing popup bottom left transition hidden"] [
                        Incremental.div (AttributeMap.ofList [clazz "ui link list"]) (
                                    aset {
                                        for (_,s) in (m.recent.recentScenes |> AMap.toASet) do
                                             yield a [clazz "item inverted";
                                                      onClick (fun _ -> LoadScene (s.path |> Mod.force))] [
                                                span [style "color:black"] [Incremental.text s.name]
                                             ]
                                    } |> AList.ofASet
                                )
                    ]
                ]

        require (Html.semui @ [myCss]) (
            let sections = [
                "blank", [div[style "height:15px"][]]                
                "Scene", [sceneTools]
                "Recent", [recentStuff]
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
        let m = Scene.loadLastScene m runtime signature        
       // let path = @"D:\Aardwork\Exomars\cd - Copy\Surface\Cape_Desire_RGB"
        //let m = Surface.import' runtime signature [Surface.mk path] m |> lookAtData
        
        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view
            update = update w runtime signature bc
            initial = m
        }