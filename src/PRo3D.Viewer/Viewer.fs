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
open PRo3D.Navigation2
open PRo3D.BookmarksX
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Viewer
open PRo3D.Drawing
open PRo3D.Groups

//open Viewer.Mutable.ModelModule.Lens
//open Mutable.AnnotationGroupModule.Lens
 
module ViewerApp =    
            
    open Surface   
    open System.Collections.Concurrent   
    open Scene   
    open DomainModel.Surfaces
    open Aardvark.UI.Trafos
    open Aardvark.SceneGraph.SgFSharp.Sg
    open PRo3d.Align
    open Aardvark.Rendering.GL.OpenGl.Pointers
                 
    // surfaces
    let _surfacesModel   = Model.Lens.scene |. Scene.Lens.surfacesModel        
    let _sgSurfaces      = _surfacesModel   |. SurfaceModel.Lens.sgSurfaces
    let _selectedSurface = _surfacesModel   |. SurfaceModel.Lens.surfaces  |. GroupsModel.Lens.lastSelectedChild
       
    // navigation
    let _navigation = Model.Lens.scene |. Scene.Lens.navigation
    let _camera     = _navigation |. NavigationModel.Lens.camera
    let _view       = _camera |. CameraControllerState.Lens.view

    // drawing
    let _drawing         = Model.Lens.drawing
    let _annotations     = _drawing |. DrawingModel.Lens.annotations
    let _flat            = _annotations  |. GroupsModel.Lens.flat    
    let _groups          = _annotations  |. GroupsModel.Lens.rootGroup |. Node.Lens.subNodes
       
    let lookAtData (m : Model) =         
        let bb = m |> Lenses.get _sgSurfaces |> HMap.toSeq |> Seq.map(fun (_,x) -> x.globalBB) |> Box3d.ofSeq
        let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value             

        _view.Set(m,view)
    
    let lookAtSurface (m : Model) id =
        let surf = m |> Lenses.get _sgSurfaces |> HMap.tryFind id
        match surf with
            | Some s ->
                let bb = s.globalBB
                let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value
                
                m |> Lenses.set _view view
            | None -> m
    
    let stash (model : Model) =
        { model with past = Some model.drawing; future = None }
       
    let refConfig : ReferenceSystemConfig<ViewConfigModel> =
        { 
            arrowLength = ViewConfigModel.Lens.arrowLength |. NumericInput.Lens.value 
            arrowThickness = ViewConfigModel.Lens.arrowThickness |. NumericInput.Lens.value 
            nearPlane = ViewConfigModel.Lens.nearPlane |. NumericInput.Lens.value 
        }

    let mrefConfig : ReferenceSystemApp.MInnerConfig<MViewConfigModel> =
        {
            getArrowLength    = fun (x:MViewConfigModel) -> x.arrowLength.value
            getArrowThickness = fun (x:MViewConfigModel) -> x.arrowThickness.value
            getNearDistance   = fun (x:MViewConfigModel) -> x.nearPlane.value
        }
    
    let drawingConfig : DrawingApp.SmallConfig<ReferenceSystem> =
        { 
            up    = (ReferenceSystem.Lens.up |. V3dInput.Lens.value)
            north = ReferenceSystem.Lens.north
        }

    let mdrawingConfig : DrawingApp.MSmallConfig<MViewConfigModel> =
        {            
            getNearPlane       = fun x -> x.nearPlane.value
            getHfov            = fun (x:MViewConfigModel) -> ((Mod.init 60.0) :> IMod<float>)
            getMaxDipAngle     = fun (x:MViewConfigModel) -> x.maxDipAngle.value
            getMinDipAngle     = fun (x:MViewConfigModel) -> x.minDipAngle.value
            getArrowThickness  = fun (x:MViewConfigModel) -> x.arrowThickness.value
            getArrowLength     = fun (x:MViewConfigModel) -> x.arrowLength.value
            getDnsPlaneSize    = fun (x:MViewConfigModel) -> x.dnsPlaneSize.value
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
    
    let mutable cache = hmap.Empty

    let isGrabbed (model : Model) =
        let sel = _selectedSurface.Get(model) |> Option.bind(fun x -> _sgSurfaces.Get(model).TryFind x)
        match sel with
          | Some s -> s.trafo.grabbed.IsSome
          | None -> false       

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (bc : BlockingCollection<string>) (m : Model) (msg : ViewerAction) =        
        match msg, m.interaction, m.picking with
            | NavigationMessage  msg,_,false when (isGrabbed m |> not) ->
               
               let c   = m.scene.config
               let ref = m.scene.referenceSystem

               { m with scene = { m.scene with navigation = Navigation.update c ref navConf m.scene.navigation msg }}
            | SetCamera cv,_,false -> 
                _view.Set(m, cv)
            | SetCameraAndFrustum (cv, hfov, _),_,false ->                
                m
            | SetCameraAndFrustum2 (cv,frustum),_,false ->
                let m = _view.Set(m, cv)
                { m with frustum = frustum }
            | AnnotationGroupsMessageViewer msg,_,_ ->
                let ag = m.drawing.annotations 
                { m with drawing = { m.drawing with annotations = Groups.update ag msg}}
            | DrawingMessage msg, Interactions.DrawAnnotation,_->
                let drawing = DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing msg

                { m with drawing = drawing; } |> stash
            | SurfaceActions msg,_,_ ->
                match msg with
                    | SurfaceApp.Action.FlyToSurface id -> 
                        lookAtSurface m id
                    | _ ->  
                        Log.warn "%A" msg
                        let s = SurfaceApp.update m.scene.surfacesModel msg m.scene.scenePath
                        { m with scene = { m.scene with surfacesModel = s}}
            | AnnotationMessage msg,_,_ ->                
                match m.drawing.annotations.lastSelectedChild with
                  | Some selected ->                             
                      let f = (fun x ->
                          let a = x |> Leaf.toAnnotation
                          AnnotationProperties.update a msg |> Leaf.Annotations)
                            
                      m.drawing.annotations
                        |> Groups.updateLeaf selected f
                        |> Lenses.set' _annotations m
                  | None -> m       
            | BookmarkMessage msg,_,_ ->  
                let model, bm = Bookmarks.update m.scene.bookmarksModel msg _navigation m
                { m with scene = { m.scene with bookmarksModel = bm; navigation = model.scene.navigation }} 
            | BookmarkUIMessage msg,_,_ ->    
                let bm = Groups.update m.scene.bookmarksModel.bookmarks msg
                { m with scene = { m.scene with bookmarksModel = {m.scene.bookmarksModel with bookmarks = bm }}} 
            | RoverMessage msg,_,_ ->
                let roverModel = RoverApp.update m.scene.viewPlans.roverModel msg
                let viewPlanModel = ViewPlan.updateViewPlanFromRover roverModel m.scene.viewPlans
                { m with scene = { m.scene with viewPlans = viewPlanModel}}  
            | ViewPlanMessage msg,_,_ ->
                //let viewPlanModel = ViewPlan.update m.scene.viewPlans m.scene.navigation.camera msg
                //{ m with scene = { m.scene with viewPlans = viewPlanModel}}  
                let model, viewPlanModel = ViewPlan.update m.scene.viewPlans msg _navigation m
                { m with scene = { m.scene with viewPlans = viewPlanModel; navigation = model.scene.navigation }}  
            | ImportSurface,_,_ -> 
                match Dialogs.openFolders w with
                    | [] -> m
                    | paths ->
                        let surfaces = 
                            paths 
                                |> List.filter Files.isSurfaceFolder
                                |> List.map Surface.mk
                                |> PList.ofList

                        let m = Scene.import' runtime signature surfaces m

                        let surfaces = m.scene.surfacesModel.sgSurfaces |> HMap.toList |> List.map snd
                        let head = surfaces |> List.tryHead
                        match head with
                          | Some v ->

                            let fullBb = 
                                surfaces 
                                  |> List.map(fun x -> x.globalBB) 
                                  |> List.fold(fun a b -> Box3d.extendBy a b) v.globalBB

                            let refAction = ReferenceSystemApp.Action.InferCoordSystem(fullBb.Center)
                            let (refSystem',_)= 
                                ReferenceSystemApp.update m.scene.config refConfig (m.scene.referenceSystem) refAction  
                                                                
                            { m with scene = { m.scene with referenceSystem = refSystem' }} |> updateCameraUp
                          | None -> m
                                                                       
            | ImportAnnotationGroups,_,_ ->
                match Dialogs.openDialog w with
                    | Some x -> 
                        //update groups
                        let imported, flat = AnnotationGroupsImporter.startImporter x

                        let newGroups = 
                            m.drawing.annotations.rootGroup.subNodes
                              |> PList.append' imported

                        let m' = m |> Lenses.set _groups newGroups                        

                        m' |> Lenses.set _flat flat
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
                    
                    match SurfaceApp.doKdTreeIntersection (_surfacesModel.Get(m)) Trafo3d.Identity ray cache with
                        | Some (t,surf), c ->                             
                            cache <- c; ray.Ray.GetPointOnRay t |> Some
                        | None, c ->
                            cache <- c; None
                                    
                match SurfaceApp.doKdTreeIntersection (_surfacesModel.Get(m)) Trafo3d.Identity fray cache with
                    | Some (t,surf), c ->                         
                        cache <- c
                        let hit = r.GetPointOnRay(t)
                        
                        match m.interaction with
                            | Interactions.DrawAnnotation ->               
                                Log.error "closest intersection on %A" surf.name
                                let drawing = DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing (Drawing.Action.AddPointAdv(hit,hitF,surf.name))
                                Log.stop()
                                { m with drawing = drawing } |> stash
                            | Interactions.PlaceCoordinateSystem -> 
                                
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
                            | Interactions.PlaceRover ->
                                let camState = m.scene.navigation.camera
                                let ref = m.scene.referenceSystem 
                                let model, viewPlans' = ViewPlan.update m.scene.viewPlans (ViewPlan.Action.AddPoint(hit,ref,cache,(_surfacesModel.Get(m)))) _navigation m 
                                { m with scene = {m.scene with viewPlans = viewPlans' } }
                            | Interactions.AlignPick -> 
                                { m with alignment = AlignmentApp.update m.alignment (AlignmentActions.AddPoint({point = hit; surfaceName = surf.guid}))}
                            | _ -> m
                    | None, _ -> 
                        Log.error ("no hit")
                        m
            | SaveScene, _,_ -> 
                match m.scene.scenePath with
                    | None ->                        
                        match Dialogs.saveDialog w with
                            | None -> m
                            | Some p -> 
                                let s' = { m.scene with scenePath = Some p}
                                { m with scene = Serialization.save p s'; } |> Model.stashAndSaveRecent p
                    | Some path ->                        
                        { m with scene = Serialization.save path m.scene } |> Model.stashAndSaveRecent path
            | SaveAs,_,_ ->
                match Dialogs.saveDialog w with
                    | None -> m
                    | Some p -> 
                        let s' = { m.scene with scenePath = Some p}                                
                        { m with scene = Serialization.save p s' } |> Model.stashAndSaveRecent p
            | OpenScene,_,_ ->
                match Dialogs.openDialog w with
                    | None -> m
                    | Some path -> 
                        Scene.loadScene path m runtime signature |> Model.stashAndSaveRecent path
            | LoadScene path,_,_ ->
                Scene.loadScene path m runtime signature |> Model.stashAndSaveRecent path
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
                                let m = { m with drawing = DrawingApp.update m.scene.referenceSystem drawingConfig w bc m.drawing drawingAction } |> stash
                                match drawingAction with
                                  | Drawing.Action.Finish -> { m with tabMenu = TabMenu.Annotations }
                                  | _ -> m                                
                            | Interactions.AlignPick -> 
                                match k with
                                 | Aardvark.Application.Keys.Enter -> 
                                    let m = { m with alignment = AlignmentApp.update m.alignment PRo3d.Align.AlignmentActions.Finish }

                                    match (m.alignment.resultTrafo, m.alignment.alignment)with
                                      | Some t, Some al -> 
                                        
                                        let f = (fun x -> 
                                            let s = x |> Leaf.toSurface
                                            { s with preTransform = t} |> Leaf.Surfaces)

                                        let g = Groups.updateLeaf al.blue f m.scene.surfacesModel.surfaces                                          
                                        
                                        { m.scene.surfacesModel with surfaces = g }
                                          |> SurfaceModel.triggerSgGrouping
                                          |> Lenses.set' _surfacesModel m                                                                                
                                      | _ -> m
                                 | _ -> m
                            | _ -> m
                                                        

                    let sensitivity = 
                        m.scene.config.navigationSensitivity.value
                    
                    let configAction = 
                        match k with 
                            | Aardvark.Application.Keys.PageUp   -> ConfigProperties.Action.SetNavigationSensitivity (Numeric.Action.SetValue (sensitivity + 0.5))
                            | Aardvark.Application.Keys.PageDown -> ConfigProperties.Action.SetNavigationSensitivity (Numeric.Action.SetValue (sensitivity - 0.5))
                            | _ -> ConfigProperties.Action.SetNavigationSensitivity (Numeric.Action.SetValue (sensitivity))

                    let c' = ConfigProperties.update m.scene.config configAction

                    let kind = 
                        match k with
                          | Aardvark.Application.Keys.F1 -> TrafoKind.Translate
                          | Aardvark.Application.Keys.F2 -> TrafoKind.Rotate
                          | _ -> m.trafoKind

                    let m = { m with trafoKind = kind }

                    let m = 
                        match k with 
                          | Aardvark.Application.Keys.Space -> 
                                                        
                            let wp = {
                                name = sprintf "wp %d" m.waypoints.Count
                                cv = (_camera.Get m).view
                            }

                            let waypoints = PList.append wp m.waypoints
                            Log.line "saving waypoints %A" waypoints
                            Serialization.save "./waypoints.wps" waypoints |> ignore
                            { m with waypoints = waypoints }                                                                                  
                          | _ -> m

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
                let m = (Model.Lens.scene |. Scene.Lens.config).Set(m,c')               
                
                match a with                   
                  | ConfigProperties.Action.SetNearPlane _ | ConfigProperties.Action.SetFarPlane _ ->
                    let fov = m.frustum |> Frustum.horizontalFieldOfViewInDegrees
                    let asp = m.frustum |> Frustum.aspect
                    let f' = Frustum.perspective fov c'.nearPlane.value c'.farPlane.value asp
                    { m with frustum = f' }
                  | _ -> m
            | SetMode d,_,_ -> 
                { m with trafoMode = d }
            | SetKind d,_,_ -> 
                { m with trafoKind = d }
            | Translate (_,b),_,_ ->
                m
                //match _selectedSurface.Get(m) with
                //  | Some selected ->
                //    let sgSurf = m |> Lenses.get _sgSurfaces |> HMap.find selected.id
                //    let s' = { sgSurf with trafo = TranslateController.updateController sgSurf.trafo b }
                                        
                //    m 
                //    |> Lenses.get _sgSurfaces
                //    |> HMap.update selected.id (fun x -> 
                //        match x with 
                //            | Some _ -> printfn "%A" s'.trafo.previewTrafo.Forward.C3.XYZ; s'
                //            | None   -> failwith "surface not found")
                //    |> Lenses.set' _sgSurfaces m
                    
                //  | None -> m                               
            | Rotate (_,b),_,_ ->
                m
                 //match _selectedSurface.Get(m) with
                 //  | Some selected ->
                 //    let sgSurf = m |> Lenses.get _sgSurfaces |> HMap.find selected.id
                 //    let s' = { sgSurf with trafo = RotationController.updateController sgSurf.trafo b }

                 //    m 
                 //    |> Lenses.get _sgSurfaces
                 //    |> HMap.update selected.id (fun x -> 
                 //         match x with 
                 //           | Some _ -> s'
                 //           | None   -> failwith "surface not found")
                 //    |> Lenses.set' _sgSurfaces m
                 //  | None -> m
            | SetTabMenu tab,_,_ ->
                { m with tabMenu = tab }
            | _ -> m       
                                   
    let mkBrushISg color size trafo : ISg<Message> =
        Sg.sphere 5 color size 
                |> Sg.shader {
                    do! Shader.stableTrafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.noEvents
                |> Sg.trafo(trafo)
   
    let view (mode : ViewerMode) (m : MModel) =        

        let renderControlAttributes (model : MNavigationModel)  =            

            amap {
                let! state = model.navigationMode
                
                match state with
                    | NavigationMode.FreeFly -> 
                        yield! CameraController.extractAttributes model.camera Navigation.Action.FreeFlyAction m.frustum
                    | NavigationMode.ArcBall -> 
                        yield! ArcBallController.extractAttributes model.camera Navigation.Action.ArcBallAction m.frustum
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap |> AttributeMap.mapAttributes (AttributeValue.map NavigationMessage)       


        let att = 
            AttributeMap.unionMany [
                renderControlAttributes m.scene.navigation
                [attribute "style" "width:70%; height: 100%; float:left"; attribute "data-samples" "8" ] |> AttributeMap.ofList
                AttributeMap.ofList [
                    onKeyDown (KeyDown)
                    onKeyUp (KeyUp)
                    attribute "style" "width:70%; height: 100%; float:left"; ] 
            ]

        let cam = Mod.map2 Camera.create m.scene.navigation.camera.view m.frustum

        let instrumentAtt =            
                (CameraController.extractAttributes m.instrumentCamera Navigation.Action.FreeFlyAction m.instrumentFrustum)                
                |> AttributeMap.ofAMap |> AttributeMap.mapAttributes (AttributeValue.map NavigationMessage)
            
                        
        let instrumentCam = Mod.map2 Camera.create m.instrumentCamera.view m.instrumentFrustum

        let overlayed =
            let annotations =
                DrawingApp.Sg.view m.scene.config mdrawingConfig m.drawing m.scene.navigation.camera.view
                    |> Sg.map DrawingMessage
                    |> Sg.fillMode (Mod.constant FillMode.Fill)
                    |> Sg.cullMode (Mod.constant CullMode.None)
            
            let alignment = 
                AlignmentApp.view m.alignment m.scene.navigation.camera.view
                    |> Sg.map AlignmentActions
                    |> Sg.fillMode (Mod.constant FillMode.Fill)
                    |> Sg.cullMode (Mod.constant CullMode.None)

            let refSystem =
                ReferenceSystemApp.Sg.view
                    m.scene.config
                    mrefConfig
                    m.scene.referenceSystem
                    m.scene.navigation.camera.view
                |> Sg.map ReferenceSystemMessage

            let exploreCenter =
                Navigation.Sg.view m.scene.navigation

            let getSurface (c:IMod<MLeaf>) =
                adaptive {
                    let! child = c                     
                    match child with 
                      | MSurfaces x -> return x
                      | _ -> return failwith ""
                }

            let controller = 
                adaptive {
                    let! selected = m.scene.surfacesModel.surfaces.lastSelectedChild
                    let! kind = m.trafoKind
                    match selected with
                      | Some c ->                           

                          let! sgo = m.scene.surfacesModel.sgSurfaces |> AMap.tryFind c
                          
                          match sgo with 
                              | Some sg ->
                                    let scaling pos =                            
                                        Sg.computeInvariantScale 
                                            m.scene.navigation.camera.view 
                                            (Mod.constant 0.1)
                                            pos 
                                            (Mod.constant 0.5)
                                            (Mod.constant 60.0)

                                    match kind with
                                    | TrafoKind.Translate ->                                               
                                        return TranslateController.viewController (fun t -> Translate("", t)) scaling sg.trafo
                                    | TrafoKind.Rotate ->
                                        return RotationController.viewController (fun t -> Rotate("", t)) scaling sg.trafo
                                    | _ -> return Sg.empty
                              | None -> 
                                Log.warn "selected surface does not exist"
                                return Sg.empty                                 
                      | None -> return Sg.empty
                    
                } |> Sg.dynamic
            
            let viewPlans =
                ViewPlan.Sg.view 
                    m.scene.config 
                    mrefConfig 
                    m.scene.viewPlans 
                    m.scene.navigation.camera.view
                |> Sg.map ViewPlanMessage

            //, exploreCenter
            [controller; annotations; exploreCenter; refSystem; alignment; viewPlans] |> Sg.ofList
                                            
        let cmds = ViewerUtils.renderCommands m.scene.surfacesModel.sgGrouped overlayed m

        let control = DomNode.RenderControl(att, cam, cmds, None)
        let instrumentControl = DomNode.RenderControl(instrumentAtt, instrumentCam, cmds, None)

        let annotationGroupProperties (model:MModel) =
            adaptive {                                
                return (Groups.viewUI model.drawing.annotations |> UI.map AnnotationGroupsMessageViewer)
            } 
                        
        let bookmarkGroupProperties (model:MModel) =
            adaptive {                                
                return (Groups.viewUI model.scene.bookmarksModel.bookmarks |> UI.map BookmarkUIMessage)
            }                              

        let viewAnnotationProperties (model :MModel) =
            let view = (fun leaf ->
                match leaf with
                  | MAnnotations ann -> AnnotationProperties.view ann
                  | _ -> div[][])
            
            model.drawing.annotations |> Groups.viewSelected view AnnotationMessage
               

        let viewDipAndStrike (model:MModel) = 
            let view = (fun leaf ->
                match leaf with
                  | MAnnotations ann -> DipAndStrike.viewUI ann
                  | _ -> div[][])

            model.drawing.annotations |> Groups.viewSelected view DnSProperties             

        let viewBookmarkProperties (model:MModel) = 
            let view = (fun leaf ->
                match leaf with
                  | MBookmarks bm -> Bookmarks.UI.view bm
                  | _ -> div[][])

            model.scene.bookmarksModel.bookmarks |> Groups.viewSelected view BookmarkUIMessage
            
        let viewPlanProperties (model:MModel) =
            model.scene.viewPlans |> ViewPlan.UI.viewRoverProperties ViewPlanMessage //RoverMessage

        let annotationLeafButtonns (model:MModel) = 
            let ts = model.drawing.annotations.activeChild
            let sel = model.drawing.annotations.lastSelectedChild
            adaptive {  
                let! ts = ts
                let! sel = sel
                match sel with
                    | Some x -> return (Groups.viewLeafButtons ts |> UI.map AnnotationGroupsMessageViewer)
                    | None -> return div[][]
            } 

        let annotationGroupButtons (model:MModel) = 
            let ts = model.drawing.annotations.activeGroup
            adaptive {  
                let! ts = ts
                return (Groups.viewGroupButtons ts |> UI.map AnnotationGroupsMessageViewer)
            } 
            
        let annotationUI = 
            let prop = 
                m.drawing.annotations.lastSelectedItem
                    |> Mod.bind (fun x -> 
                        match x with 
                          | SelectedItem.Group -> annotationGroupProperties m
                          | _ -> viewAnnotationProperties m 
                    )

            let buttons = 
                m.drawing.annotations.lastSelectedItem
                    |> Mod.bind (fun x -> 
                        match x with 
                          | SelectedItem.Group -> annotationGroupButtons m
                          | _ -> annotationLeafButtonns m 
                    )
            
            div [][
                DrawingApp.UI.viewAnnotationImportExport |> UI.map DrawingMessage
               
                GuiEx.accordion "Annotations" "Write" true [
                    //Groups.viewSelectionButtons |> UI.map AnnotationGroupsMessageViewer
                    DrawingApp.UI.viewAnnotationGroups m.drawing |> UI.map DrawingMessage
                ]
                GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle prop)                                        
                ]
               
                GuiEx.accordion "Actions" "Asterisk" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
                ]     
                
                GuiEx.accordion "Dip&Strike" "Calculator" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(viewDipAndStrike m))] 
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
        
        let bookmarksLeafButtonns (model:MModel) = 
            let ts = model.scene.bookmarksModel.bookmarks.activeChild
            let sel = model.scene.bookmarksModel.bookmarks.lastSelectedChild
            adaptive {  
                let! ts = ts
                let! sel = sel
                match sel with
                    | Some x -> return (Groups.viewLeafButtons ts |> UI.map BookmarkUIMessage)
                    | None -> return (div[][])
                
            } 

        let bookmarksGroupButtons (model:MModel) = 
            let ts = model.scene.bookmarksModel.bookmarks.activeGroup
            adaptive {  
                let! ts = ts
                return (Groups.viewGroupButtons ts |> UI.map BookmarkUIMessage)
            } 

        let bookmarkUI = 
            let item2 = 
                m.scene.bookmarksModel.bookmarks.lastSelectedItem 
                    |> Mod.map (fun x -> 
                        match x with 
                          | SelectedItem.Group -> bookmarkGroupProperties m
                          | _ -> viewBookmarkProperties m 
                    )
            let buttons = 
                m.scene.bookmarksModel.bookmarks.lastSelectedItem 
                    |> Mod.bind (fun x -> 
                        match x with 
                          | SelectedItem.Group -> bookmarksGroupButtons m
                          | _ -> bookmarksLeafButtonns m 
                    )
            div [][
                Bookmarks.UI.viewGUI |> UI.map BookmarkMessage

                GuiEx.accordion "Bookmarks" "Write" true [
                    //Groups.viewSelectionButtons |> UI.map BookmarkUIMessage
                    Bookmarks.viewBookmarksGroups m.scene.bookmarksModel |> UI.map BookmarkMessage 
                ]
                GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (alist{ let! item = item2
                                                               let! i = item
                                                               yield i})
                ] 
                GuiEx.accordion "Actions" "Asterisk" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
                ]   
                ]

        let viewPlannerUI = 
            
            div [][
                GuiEx.accordion "ViewPlans" "Write" true [
                    ViewPlan.UI.viewViewPlans m.scene.viewPlans |> UI.map ViewPlanMessage
                ]
                GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (viewPlanProperties m))
                ] 
            ]
          
        let tabs = 
            GuiEx.tabbed [clazz "ui inverted segment"] [
                ("Data", SurfaceApp.surfaceUI m.scene.surfacesModel |> UI.map SurfaceActions) 
                ("Annotations", annotationUI) 
                ("Config", configUI)            
                ("Bookmarks", bookmarkUI) 
                ("ViewPlanner", viewPlannerUI)
            ] "Data" 
      
        let dynamicTop =
            adaptive {
                let! interaction = m.interaction
                match interaction with
                  | Interactions.DrawAnnotation -> 
                    return DrawingApp.UI.viewAnnotationToolsHorizontal m.drawing |> UI.map DrawingMessage
                  | Interactions.PlaceRover ->
                    return ViewPlan.UI.viewSelectRover m.scene.viewPlans.roverModel |> UI.map RoverMessage
                  | Interactions.PlaceCoordinateSystem -> 
                    return Html.Layout.horizontal [
                        Html.Layout.boxH [ Html.SemUi.dropDown' m.scene.referenceSystem.scaleChart m.scene.referenceSystem.selectedScale ReferenceSystemApp.Action.SetScale id ]
                        Html.Layout.boxH [ i [clazz "large Unhide icon"][] ]
                        Html.Layout.boxH [ Html.SemUi.toggleBox m.scene.referenceSystem.isVisible ReferenceSystemApp.Action.ToggleVisible  ]
                        
                        ] |> UI.map ReferenceSystemMessage
                  | _ -> 
                    return div[][]
            }

        let topMenu = [
             Navigation.UI.viewNavigationModes m.scene.navigation  |> UI.map NavigationMessage 

             Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large wizard icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown m.interaction SetInteraction ]                
             ]             
                         
             Incremental.div  AttributeMap.empty (AList.ofModSingle dynamicTop)
             

             //button [onClick(fun _ -> QuickLoad1)] [text "quickload"]
             //button [onClick (fun _ -> DeleteLast)][text "Delete Last"]
        ] 

        let viewTabs (tab:IMod<TabMenu>) = 
            let nodes =
                alist {
                    let! t = tab
                    match t with
                      | TabMenu.Surfaces -> 
                            yield SurfaceApp.surfaceUI m.scene.surfacesModel |> UI.map SurfaceActions
                      | TabMenu.Annotations -> 
                            yield annotationUI
                      | TabMenu.Bookmarks -> 
                            yield bookmarkUI
                      | TabMenu.Config -> 
                            yield configUI
                      | TabMenu.Viewplanner -> 
                            yield viewPlannerUI
                      | _-> failwith "tab not supported"
                    
                }
            Incremental.div AttributeMap.empty nodes                        
                     
        let rest (model : MModel) = 
            [             
                control
                div [clazz "ui"; style "width: 30%; float:right; background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] [
                    div[style "height:41px"][text "spacer"]
                    div [clazz "ui large black horizontal label"][text "Show Tab:"]
                    Html.SemUi.dropDown model.tabMenu SetTabMenu
                    viewTabs model.tabMenu                    
                ]            
            ]
        
        //Sidebar adorner
                
        let sceneTools =
            div [clazz "ui buttons inverted"] [ 
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Save Scene"; onMouseClick (fun _ -> SaveScene)] [
                                i [clazz "large save icon" ] [] 
                    ]
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
                        i [clazz "large folder outline icon" ] [] 
                    ]
                )

                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "New Scene"; onMouseClick (fun _ -> NewScene)] [
                        i [clazz "large file outline icon"] [] 
                    ]
                )
            ]

        let openSurface =
            [ text "browse"] |> button [
                clazz "ui button"
                attribute "data-content" "text text text"
                onClick (fun _ -> ImportSurface)
            ]         

        let importAnnotationGroups =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> ImportAnnotationGroups)
            ] 

        let usingContext =             
            "$('.context.example .ui.sidebar')
              .sidebar({
                context: $('.context.example .bottom.segment.blarg')
              })
              .sidebar('attach events', '.context.example .menu.blarg .item.blarg')
              .sidebar('setting', 'transition', 'Overlay')
            ;"
        
        let myCss = { kind = Stylesheet; name = "semui-overrides"; url = "./semui-overrides.css" }

        let recentScenes =
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

        let chooseView (mode : ViewerMode) =

            let stuff = 
                alist {                                     
                    match mode with
                    | ViewerMode.Standard -> 
                        let sections = [
                            "blank", [div[style "height:15px"][]]
                            "Scene", [sceneTools]
                            "Recent", [recentScenes]
                            "Add Surface", [openSurface]                
                            "Import AnnotationGroups", [importAnnotationGroups]]   


                        yield div[](GuiEx.adornerMenu sections topMenu (rest m))
                    | ViewerMode.Instrument -> 
                        yield instrumentControl
                }
                  

            Incremental.div AttributeMap.empty stuff
            
        require (Html.semui @ [myCss]) (                             
            onBoot usingContext (                            
                body [clazz "context example"; style "overflow-y:hidden"] [chooseView mode]
            )
        )
               
    /// Thread Pool stuff
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

    let loadWaypoints m = 
        match Serialization.fileExists "./waypoints.wps" with
          | Some path -> 
                let wp = Serialization.loadAs<plist<WayPoint>> path
                { m with waypoints = wp }
          | None -> m

    let loadRoverData (m:Model) =
        let vps = ViewPlan.loadRoverData m.scene.viewPlans m.scene.scenePath
        { m with scene = { m.scene with viewPlans = vps } }

    let start (w : Form) (runtime: IRuntime) (signature: IFramebufferSignature) (mode : ViewerMode) bc =
        let m = PRo3D.Viewer.Viewer.initial        
        let m = Scene.loadLastScene runtime signature m   
        let m = loadRoverData m
        
        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view mode
            update = update w runtime signature bc
            initial = m
        }