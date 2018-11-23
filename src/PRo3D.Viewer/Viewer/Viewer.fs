namespace PRo3D

open Aardvark.Service

open System
open System.Windows.Forms
open System.Diagnostics

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Operators
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text

open Aardvark.SceneGraph.Opc
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
open PRo3D.OrientationCube

open Aardium

//open Viewer.Mutable.ModelModule.Lens
//open Mutable.AnnotationGroupModule.Lens
 
module ViewerApp =    

    type muh = {
       error : double
    }
            
    open Surface   
    open System.Collections.Concurrent   
    open Scene   
    //open DomainModel.Surfaces
    open Aardvark.UI.Trafos
    open Aardvark.SceneGraph.SgFSharp.Sg
    open PRo3d.Align
    open Aardvark.Rendering.GL.OpenGl.Pointers
    open Aardvark.UI.Animation
                 
    // surfaces
    let _surfacesModel   = Model.Lens.scene |. Scene.Lens.surfacesModel        
    let _sgSurfaces      = _surfacesModel   |. SurfaceModel.Lens.sgSurfaces
    let _selectedSurface = _surfacesModel   |. SurfaceModel.Lens.surfaces  |. GroupsModel.Lens.singleSelectLeaf
       
    // navigation
    let _navigation = Model.Lens.scene |. Scene.Lens.navigation
    let _camera     = _navigation |. NavigationModel.Lens.camera
    let _view       = _camera |. CameraControllerState.Lens.view

    // drawing
    let _drawing         = Model.Lens.drawing
    let _annotations     = _drawing |. DrawingModel.Lens.annotations
    let _dnsColorLegend  = _drawing |. DrawingModel.Lens.colorLegend
    let _flat            = _annotations  |. GroupsModel.Lens.flat    
    let _lookUp          = _annotations  |. GroupsModel.Lens.groupsLookup
    let _groups          = _annotations  |. GroupsModel.Lens.rootGroup |. Node.Lens.subNodes

    // animation  
    let _animation = Model.Lens.animations
    let _animationView = _animation |. AnimationModel.Lens.cam
       
    let lookAtData (m : Model) =         
        let bb = m |> Lenses.get _sgSurfaces |> HMap.toSeq |> Seq.map(fun (_,x) -> x.globalBB) |> Box3d.ofSeq
        let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value             

        _view.Set(m,view)

    let lookAtBoundingBox (bb : Box3d) (m : Model) =
        let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value                
        m |> Lenses.set _view view
    
    let lookAtSurface (m : Model) id =
        let surf = m |> Lenses.get _sgSurfaces |> HMap.tryFind id
        match surf with
            | Some s ->
                let bb = s.globalBB
                m |> lookAtBoundingBox bb
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
            up     = (ReferenceSystem.Lens.up     |. V3dInput.Lens.value)
            north  = (ReferenceSystem.Lens.north  |. V3dInput.Lens.value)
            planet = (ReferenceSystem.Lens.planet)
        }

    let mdrawingConfig : DrawingApp.MSmallConfig<MViewConfigModel> =
        {            
            getNearPlane       = fun x -> x.nearPlane.value
            getHfov            = fun (x:MViewConfigModel) -> ((Mod.init 60.0) :> IMod<float>)
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

    let updateSceneWithNewSurface (m:Model) =
        let sgSurfaces = m.scene.surfacesModel.sgSurfaces |> HMap.toList |> List.map snd
        let head = sgSurfaces |> List.tryHead
        match head with
            | Some v ->
                let fullBb = 
                    sgSurfaces 
                        |> List.map(fun x -> x.globalBB) 
                        |> List.fold(fun a b -> Box3d.extendBy a b) v.globalBB

                let refAction = ReferenceSystemApp.Action.InferCoordSystem(fullBb.Center)
                let (refSystem',_)= 
                    ReferenceSystemApp.update m.scene.config refConfig (m.scene.referenceSystem) refAction  
                //let (orSystem',_)= 
                //    ReferenceSystemApp.update m.scene.config refConfig (m.scene.orientationSystem) refAction  
                                                                
                { m with scene = { m.scene with referenceSystem = refSystem' }} |> updateCameraUp |> lookAtBoundingBox v.globalBB //; orientationSystem = orSystem'
            | None -> m

    let isGrabbed (model : Model) =
        let sel = _selectedSurface.Get(model) |> Option.bind(fun x -> _sgSurfaces.Get(model).TryFind x)
        match sel with
          | Some s -> s.trafo.grabbed.IsSome
          | None -> false   
          
    let loadRoverData (m:Model) =
        let vps = ViewPlan.loadRoverData m.scene.viewPlans m.scene.scenePath
        { m with scene = { m.scene with viewPlans = vps } }

    let loadLastAnnotations (m:Model) = 
          match  m.scene.scenePath with
            | Some p -> let ap = DrawingApp.getAnnotationPath p
                        let groups = Groups.tryLoadAnnotations ap 
                        match groups with          
                            | Some g -> { m with drawing = { m.drawing with annotations = g.annotations; colorLegend = g.colorLegend } }
                            | None -> m
            |None -> m
          //match Serialization.fileExists "./lastmeasure" with
          //  | Some path ->                       
          //    let groups = Groups.tryLoadAnnotations path 
          //    match groups with          
          //      | Some g -> { m with drawing = { m.drawing with annotations = g.annotations; colorLegend = g.colorLegend } }
          //      | None -> m
          //  | None -> m

    let animateFowardAndLocation (pos : V3d) (dir : V3d) (duration : RelativeTime) (name : string) = 
      {
        (CameraAnimations.initial name) with 
          sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
            if localTime < duration then                  
              let rot      = Rot3d(state.Forward, dir) * localTime / duration
              let forward' = rot.TransformDir(state.Forward)
              
              let vec       = pos - state.Location
              let velocity  = vec.Length / duration                  
              let dir       = vec.Normalized
              let location' = state.Location + dir * velocity * localTime

              let view = 
                state 
                  |> CameraView.withForward forward'
                  |> CameraView.withLocation location'
      
              Some (state,view)
            else None
      }

    let rec getAllDockElements (dnc : DockNodeConfig) : (list<DockElement>) = 
        match dnc with
            | DockNodeConfig.Vertical (weight,children) -> let test = children |> List.map(fun x -> getAllDockElements x )
                                                           test |> List.concat  
            | DockNodeConfig.Horizontal (weight,children) -> let test = children |> List.map(fun x -> getAllDockElements x )
                                                             test |> List.concat 
            | DockNodeConfig.Stack (weight,activeId,children) -> children 
            | DockNodeConfig.Element element -> [element] 
    
    let updateClosedPages (m : Model) (dncUpdated : DockNodeConfig) =
        let de = getAllDockElements m.scene.dockConfig.content
        let deUpdated = getAllDockElements dncUpdated
        let diff = ((Set.ofList de) - (Set.ofList deUpdated)) |> Set.toList
        // diff contains all changed elements (not only the deleted)
        match diff with
            | [] -> m.scene.closedPages
            | _ -> let test = 
                         diff 
                            |> List.choose (fun x -> let t = deUpdated |> List.filter(fun y -> y.id = x.id)
                                                     match t with
                                                        | [] -> Some x
                                                        | _  -> None 
                                                      )
                                   
                   List.append m.scene.closedPages test 
                   

    let addDockElement (dnc : DockNodeConfig) (de:DockElement) = 
        match dnc with
            | DockNodeConfig.Vertical (weight,children) -> let add = List.append children [(Stack(weight, None, [de]))]
                                                           Horizontal(weight,add)
            | DockNodeConfig.Horizontal (weight,children) -> let add = List.append children [(Stack(weight, None, [de]))]
                                                             Horizontal(weight,add) 
            | DockNodeConfig.Stack (weight,activeId,children) -> Stack(weight, activeId, List.append [de] children)
            | DockNodeConfig.Element element ->  Stack(0.2, None, List.append [de] [element]) 

    
    let saveSceneAndAnnotations (path:string) (m:Model) = 
        let s' = { m.scene with scenePath = Some path}
        let drawing = DrawingApp.saveSceneAnnotations path m.drawing
        { m with scene = Serialization.save path s'; drawing = drawing } |> Model.stashAndSaveRecent path

    let matchPickingInteraction (bc : BlockingCollection<string>) (p:V3d) (hitFunction:(V3d-> V3d option)) (surf:Surface) (m:Model) = 
         match m.interaction with
            | Interactions.DrawAnnotation -> 
                let m = match surf.surfaceType with
                        | SurfaceType.SurfaceOBJ -> { m with drawing = { m.drawing with projection = Projection.Linear } }   
                        | _ -> m
                
                let msg = Drawing.Action.AddPointAdv(p, hitFunction, surf.name)
                let drawing = DrawingApp.update m.scene.referenceSystem drawingConfig bc m.drawing msg
                //Log.stop()
                { m with drawing = drawing } |> stash
            | Interactions.PlaceCoordinateSystem -> 
                          
                let refAction = ReferenceSystemApp.Action.InferCoordSystem(p)
                let (refSystem',_)= 
                    ReferenceSystemApp.update m.scene.config refConfig (m.scene.referenceSystem) refAction  
                          
                //let origin = new V3d(m.overlayFrustum.right * 0.5, m.overlayFrustum.bottom * 0.5, 1.0)
                //let os = { refSystem' with origin = origin }
                let m = { m with scene = { m.scene with referenceSystem = refSystem' }} //; orientationSystem = os
                          
                //update camera upvector
                updateCameraUp m
            | Interactions.PickExploreCenter ->
                let c   = m.scene.config
                let ref = m.scene.referenceSystem
                let navigation' = 
                    Navigation.update c ref navConf m.scene.navigation (Navigation.Action.ArcBallAction(ArcBallController.Message.Pick p))
                { m with scene = {m.scene with navigation = navigation'; } }
            | Interactions.PlaceRover ->
                let ref = m.scene.referenceSystem 
                let model, viewPlans' = ViewPlan.update m.scene.viewPlans (ViewPlan.Action.AddPoint(p,ref,cache,(_surfacesModel.Get(m)))) _navigation m 
                let m' = { m with scene = {m.scene with viewPlans = viewPlans' } }
                match m.scene.viewPlans.working.IsEmpty with
                    | true -> m' 
                    | false -> { m' with tabMenu = TabMenu.Viewplanner }
            //| Interactions.AlignPick -> 
            //    { m with alignment = AlignmentApp.update m.alignment (AlignmentActions.AddPoint({point = p; surfaceName = surf.guid}))}
            | Interactions.PlaceSurface -> 
                { m with scene = { m.scene with surfacesModel = SurfaceApp.update m.scene.surfacesModel (SurfaceApp.Action.PlaceSurface(p)) m.scene.scenePath }}
           // | Interactions.PickAnnotation -> m
            | _ -> m

    let mutable lastHash = -1
    
    let update (runtime : IRuntime) (signature: IFramebufferSignature) (sendQueue : BlockingCollection<string>) (mailbox : MessagingMailbox) (m : Model) (msg : ViewerAction) =        
        match msg, m.interaction, m.picking with
            | NavigationMessage  msg,_,false when (isGrabbed m |> not) && (not (AnimationApp.shouldAnimate m.animations)) ->                              
               let c   = m.scene.config
               let ref = m.scene.referenceSystem
               let nav = Navigation.update c ref navConf m.scene.navigation msg               

//               m.scene.navigation.camera.view.Location.ToString() |> NoAction |> ViewerAction |> mailbox.Post

               m 
                 |> Lenses.set _navigation nav
                 |> Lenses.set _animationView nav.camera.view
            | AnimationMessage msg,_,_ ->
              let a = AnimationApp.update m.animations msg
              { m with animations = a } |> Lenses.set _view a.cam
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
            | DrawingMessage msg,_,_-> //Interactions.DrawAnnotation
                match msg with
                | Drawing.FlyToAnnotation id ->
                    let _a = m |> Lenses.get _flat |> HMap.tryFind id |> Option.map Leaf.toAnnotation
                    match _a with 
                      | Some a ->                                                
                          m |> lookAtBoundingBox (Box3d(a.points |> PList.toList))
                      | None -> m                    
                | _ ->
                    let drawing = 
                        DrawingApp.update m.scene.referenceSystem drawingConfig sendQueue m.drawing msg
                    { m with drawing = drawing; } |> stash
            | SurfaceActions msg,_,_ ->
                    let s = SurfaceApp.update m.scene.surfacesModel msg m.scene.scenePath
                    let animation = 
                          match msg with
                            | SurfaceApp.Action.FlyToSurface id -> 
                                let surf = m |> Lenses.get _sgSurfaces |> HMap.tryFind id
                                match surf with
                                    | Some s ->
                                            let bb = s.globalBB
                                            let view = CameraView.lookAt bb.Max bb.Center m.scene.referenceSystem.up.value    
                                            let animationMessage = 
                                                animateFowardAndLocation view.Location view.Forward 2.0 "ForwardAndLocation2s"
                                            let a' = AnimationApp.update m.animations (AnimationAction.PushAnimation(animationMessage))
                                            a'
                                    | None -> m.animations
                            | _-> m.animations
                           
                    { m with scene = { m.scene with surfacesModel = s}; animations = animation}
            | AnnotationMessage msg,_,_ ->                
                match m.drawing.annotations.singleSelectLeaf with
                  | Some selected ->                             
                      let f = (fun x ->
                          let a = x |> Leaf.toAnnotation
                          AnnotationProperties.update a msg |> Leaf.Annotations)
                            
                      m.drawing.annotations
                        |> Groups.updateLeaf selected f
                        |> Lenses.set' _annotations m
                  | None -> m       
            | BookmarkMessage msg,_,_ ->  
                let m', bm = Bookmarks.update m.scene.bookmarksModel msg _navigation m

                let animation = 
                  match msg with
                  | BookmarkAction.GroupsMessage k ->
                    match k with 
                    | Groups.Action.UpdateCam _->
                      let location = m'.scene.navigation.camera.view.Location
                      let forward = m'.scene.navigation.camera.view.Forward

                      //let locationMsg = CameraAnimations.animateLocation location 2.0 "flyto 2s"
                      let animationMessage = 
                        animateFowardAndLocation location forward 3.5 "ForwardAndLocation2s"
                      let a' = AnimationApp.update m.animations (AnimationAction.PushAnimation(animationMessage))
                      //let fowardMsg = CameraAnimations.animateFoward foward 2.0 "turnAround 2s"
                      //let a' = AnimationApp.update m.animations (AnimationAction.PushAnimation(fowardMsg))

                      a'
                    | _ -> m.animations
                  | _ -> m.animations
                
                { m with scene = { m.scene with bookmarksModel = bm }; animations = animation} //; navigation = m'.scene.navigation }} 
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
            | DnSColorLegendMessage msg,_,_ ->
                let cm = FalseColorLegendApp.update m.drawing.colorLegend msg
                { m with drawing = { m.drawing with colorLegend = cm } }
            | ImportSurface sl,_,_ -> 
                match sl with
                    | [] -> m
                    | paths ->
                        let surfaces = 
                            paths 
                                |> List.filter (fun x -> Files.isSurfaceFolder x || Files.isZippedOpcFolder x)
                                |> List.map (Sg.mk SurfaceType.SurfaceOPC)
                                |> PList.ofList

                        let m = Scene.import' runtime signature surfaces m
                        updateSceneWithNewSurface m
            | ImportObject sl,_,_ -> 
                match sl with
                    | [] -> m
                    | paths ->
                        let p = paths.[0]
                        let objects = 
                            [p] 
                                |> List.map (Sg.mk SurfaceType.SurfaceOBJ)
                                |> PList.ofList

                        let m = Scene.importObj runtime signature objects m
                        updateSceneWithNewSurface m    
            | ImportAnnotationGroups sl,_,_ ->
              match sl with
                | [] -> m   
                | path -> 
                  let p = path.[0]

                  

                  //update groups
                  let imported, flat, lookup = AnnotationGroupsImporter.startImporter p m.scene.referenceSystem

                  let newGroups = 
                      m.drawing.annotations.rootGroup.subNodes
                        |> PList.append' imported

                  let flat = 
                    flat 
                      |> HMap.map(fun k v -> 
                        let a = v |> Leaf.toAnnotation
                        let a' = if a.geometry = Geometry.DnS then { a with showDns = true } else a                                                             
                        a'|> Leaf.Annotations)    
                        
                  let newflat = m.drawing.annotations.flat |> HMap.union flat

                  //let inline median input = 
                  //  let sorted = input |> Seq.toArray |> Array.sort
                  //  let m1,m2 = 
                  //      let len = sorted.Length-1 |> float
                  //      len/2. |> floor |> int, len/2. |> ceil |> int 
                  //  (sorted.[m1] + sorted.[m2] |> float)/2.

                  //let stuff = 
                  //  flat 
                  //    |> HMap.toList 
                  //    |> List.map snd 
                  //    |> List.map Leaf.toAnnotation
                  //    |> List.filter(fun x -> x.geometry = Geometry.DnS)
                  //    |> List.map(fun x -> x.points |> DipAndStrike.calculateDnsErrors)
                  //    |> List.concat
                      
                  //let avg = stuff |> List.average
                  //let med = stuff |> List.toArray |> median
                  //let std = DipAndStrike.computeStandardDeviation avg stuff
                  //Log.line "%f %f %f" std avg med
                      
                  //let result = stuff |> List.map(fun x -> { error = x } )

                  //let csvTable = Csv.Seq.csv ";" true id result
                  //Csv.Seq.write ("./error.csv") csvTable |> ignore

                  m 
                    |> Lenses.set _groups newGroups
                    |> Lenses.set _lookUp lookup
                    |> Lenses.set _flat newflat                
            | ImportSurfaceTrafo sl,_,_ ->
              match sl with
                | [] -> m 
                | path :: _ ->
                  let imported = SurfaceTrafoImporter.startImporter path |> PList.toList                  
                  let s = Surfaces.SurfaceApp.updateSurfaceTrafos imported m.scene.surfacesModel
                  s.surfaces.flat |> HMap.toList |> List.iter(fun (_,v) -> Log.warn "%A" (v |> Leaf.toSurface).preTransform)
                  m 
                    |> Lenses.set _surfaceModelLens s                                    
            | ImportRoverPlacement sl,_,_ ->
              match sl with
                | [] -> m 
                | p -> 
                  let path = p.[0]
                  let importedData = RoverPlacementImporter.startRPImporter path 
                  match m.scene.viewPlans.roverModel.selectedRover with
                    | Some r -> let vp = ViewPlan.createViewPlanFromFile importedData m.scene.viewPlans r m.scene.referenceSystem m.scene.navigation.camera
                                { m with scene = { m.scene with viewPlans = vp}} 
                    | None -> Log.error "no rover selected"; m
            | QuickLoad1,_,_ ->
                Scene.loadScene @"E:\Aardwork\Exomars\_NEWVIEWER\Scenes\TestScene\scene.scn" m runtime signature     
            | QuickLoad2,_,_ ->                                
                (MeasurementsImporter.startImporter "E:\Aardwork\Exomars\_SCENES\cd - Copy\cd.xml") |> ignore               
                m
            | DeleteLast,_,_ -> 
                File.Delete("./last") |> ignore
                m
            | PickSurface (p,name), _ ,true ->
              let fray = p.globalRay.Ray
              let r = fray.Ray
              let rayHash = r.GetHashCode()
              if rayHash = lastHash then
                m
              else                                
                Log.startTimed "[PickSurface] try intersect kdtree of %s" name                                                                     
                let hitF (camLocation : V3d) (p : V3d) = 
                  
                  let ray =
                      match m.drawing.projection with
                          | Projection.Viewpoint -> 
                              let dir = (p-camLocation).Normalized
                              FastRay3d(camLocation, dir)  
                          | Projection.Sky -> 
                              let up = m.scene.referenceSystem.up.value
                              FastRay3d(p + (up * 5000.0), -up)  
                          | _ -> Log.error "projection started without proj mode"; FastRay3d()
                  
                  match SurfaceApp.doKdTreeIntersection (_surfacesModel.Get(m)) ray cache with
                      | Some (t,surf), c ->                             
                          cache <- c; ray.Ray.GetPointOnRay t |> Some
                      | None, c ->
                          cache <- c; None
                                  
                let result = 
                  match SurfaceApp.doKdTreeIntersection (_surfacesModel.Get(m)) fray cache with
                   | Some (t,surf), c ->                         
                     cache <- c
                     let hit = r.GetPointOnRay(t)
                     let cameraLocation = m.scene.navigation.camera.view.Location 
                     let hitF = hitF cameraLocation
                   
                     lastHash <- rayHash
                     matchPickingInteraction sendQueue hit hitF surf m                                    
                   | None, _ -> 
                     Log.error "[PickSurface] no hit of %s" name
                     m
                Log.stop()
                Log.error "done intersecting"
                
                result
            | PickObject (p,id), _ ,_ ->  
                match m.picking with
                    |true ->
                          let hitF (p : V3d) = None
                          match (m.scene.surfacesModel.surfaces.flat.TryFind id) with
                            | Some x -> matchPickingInteraction sendQueue p hitF (x |> Leaf.toSurface) m 
                            | None -> m
                    | false -> m
                
            | SaveScene s, _,_ -> 
                match m.scene.scenePath with
                    | None ->                        
                        match s.IsEmptyOrNull() with
                            | true -> m
                            | _ -> saveSceneAndAnnotations s m
                    | Some path -> 
                        saveSceneAndAnnotations path m                                     
            | SaveAs s,_,_ ->
                match s.IsEmptyOrNull() with
                    | true -> m
                    | _  -> saveSceneAndAnnotations s m
            | OpenScene sl,_,_ ->
                match sl.Length < 1 with
                    | true -> m
                    | false -> 
                        let path = sl.[0]
                        Scene.loadScene path m runtime signature 
                            |> Model.stashAndSaveRecent path
                            |> loadRoverData
                            |> loadLastAnnotations  
            | LoadScene path,_,_ ->
                Scene.loadScene path m runtime signature 
                    |> Model.stashAndSaveRecent path
                    |> loadRoverData
                    |> loadLastAnnotations  
            | NewScene,_,_ ->
                let m' = { PRo3D.Viewer.Viewer.initial m.messagingMailbox with recent = m.recent }
                loadRoverData m'
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
                  | Aardvark.Application.Keys.LeftCtrl -> { m with picking = true; ctrlFlag = true }
                  |_                                   -> { m with picking = false;}

              let m =
                match (m.ctrlFlag, k, m.scene.scenePath) with
                | (true, Aardvark.Application.Keys.S, Some path) -> 
                  { (saveSceneAndAnnotations path m) with ctrlFlag = false }   
                | (true, Aardvark.Application.Keys.S, None) -> 
                   Log.error "please use \"save\" in the menu to save the scene";
                   { m with ctrlFlag = false }   // (saveSceneAndAnnotations p m)
                |_-> m
                             
              let m =
                match m.interaction with
                | Interactions.DrawAnnotation -> 
                    let m = { m with drawing = DrawingApp.update m.scene.referenceSystem drawingConfig sendQueue m.drawing drawingAction } |> stash
                    match drawingAction with
                      | Drawing.Action.Finish -> { m with tabMenu = TabMenu.Annotations }
                      | _ -> m     
                //| Interactions.PickAnnotation -> 
                //    match m.picking with
                //        | true -> { m with drawing = DrawingApp.update m.scene.referenceSystem drawingConfig sendQueue m.drawing Drawing.Action.StartPicking } |> stash
                //        | false -> m
                    
                   
                //| Interactions.AlignPick -> 
                //    match k with
                //     | Aardvark.Application.Keys.Enter -> 
                //        let m = { m with alignment = AlignmentApp.update m.alignment PRo3d.Align.AlignmentActions.Finish }

                //        match (m.alignment.resultTrafo, m.alignment.alignment)with
                //          | Some t, Some al ->                             
                //            let f = (fun x -> 
                //                let s = x |> Leaf.toSurface
                //                { s with preTransform = t} |> Leaf.Surfaces)

                //            let g = Groups.updateLeaf al.blue f m.scene.surfacesModel.surfaces
                //            { m.scene.surfacesModel with surfaces = g }
                //              |> SurfaceModel.triggerSgGrouping
                //              |> Lenses.set' _surfacesModel m
                //          | _ -> m
                //     | _ -> m
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
                  | Aardvark.Application.Keys.LeftCtrl -> 
                    match m.interaction with
                        | Interactions.DrawAnnotation -> 
                            DrawingApp.update m.scene.referenceSystem drawingConfig sendQueue m.drawing Drawing.Action.StopDrawing
                        //| Interactions.PickAnnotation -> 
                        //    DrawingApp.update m.scene.referenceSystem drawingConfig sendQueue m.drawing Drawing.Action.StopPicking 
                        |_-> m.drawing
                  | _ -> m.drawing                
                { m with drawing = d; picking = false } |> stash
            | SetInteraction t,_,_ -> { m with interaction = t }
            | ReferenceSystemMessage a,_,_ ->                                
                let refsystem',_ = ReferenceSystemApp.update m.scene.config refConfig m.scene.referenceSystem a
                let m = (Model.Lens.scene |. Scene.Lens.referenceSystem).Set(m, refsystem')
                //let m = (Model.Lens.scene |. Scene.Lens.orientationSystem).Set(m, refsystem')
               
                match a with 
                  | ReferenceSystemApp.Action.SetUp _ | ReferenceSystemApp.Action.SetPlanet _ -> updateCameraUp m
                  | _ -> m                
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
            | SwitchViewerMode _,_,_ -> 
                match m.viewerMode with
                    | ViewerMode.Standard -> { m with viewerMode = ViewerMode.Instrument }
                    | ViewerMode.Instrument -> { m with viewerMode = ViewerMode.Standard }

            | OpenSceneFileLocation p,_,_ ->                
                let argument = sprintf "/select, \"%s\"" p
                Process.Start("explorer.exe", argument) |> ignore
                m
            | NoAction s,_,_ -> 
              if s.IsEmptyOrNull() |> not then 
                Log.line "[Viewer.fs] No Action %A" s
              
              m                   
            | UpdateDockConfig dcf,_,_ ->
                let closedPages = updateClosedPages m dcf.content
                { m with scene = { m.scene with dockConfig = dcf; closedPages = closedPages } }
            | AddPage de,_,_ -> 
                let closedPages = m.scene.closedPages |> List.filter(fun x -> x.id <> de.id)                
                let cont = addDockElement m.scene.dockConfig.content de
                let dockconfig = config {content(cont);appName "PRo3D"; useCachedConfig false }
                { m with scene = { m.scene with dockConfig = dockconfig; closedPages = closedPages } }
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
   
   
    let view (m : MModel) = //(localhost: string)
        
        let renderControlAttributes (model : MNavigationModel)  =            
            amap {
                let! state = model.navigationMode
                match state with
                    | NavigationMode.FreeFly -> 
                        yield! CameraController.extractAttributes model.camera Navigation.Action.FreeFlyAction
                    | NavigationMode.ArcBall -> 
                        yield! ArcBallController.extractAttributes model.camera Navigation.Action.ArcBallAction
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap |> AttributeMap.mapAttributes (AttributeValue.map NavigationMessage)       

        let att = 
            AttributeMap.unionMany [
                renderControlAttributes m.scene.navigation

                AttributeMap.ofList [
                 attribute "style" "width:100%; height: 100%; float:left; background-color: #222222"
                 attribute "data-samples" "8"
                 attribute "useMapping" "true"
                 //attribute "showFPS" "true"                  
                 //attribute "data-renderalways" "true" 
                 onKeyDown (KeyDown)
                 onKeyUp (KeyUp)                    
                ] 
            ]            

        let annotations, discs = DrawingApp.view m.scene.config mdrawingConfig m.scene.navigation.camera.view m.drawing
            
        let discs = 
           discs
             |> Sg.map DrawingMessage
             |> Sg.fillMode (Mod.constant FillMode.Fill)
             |> Sg.cullMode (Mod.constant CullMode.None)

        let overlayed =

            let annos = 
              annotations
                    |> Sg.map DrawingMessage
                    |> Sg.fillMode (Mod.constant FillMode.Fill)
                    |> Sg.cullMode (Mod.constant CullMode.None)
            
            //let alignment = 
            //    AlignmentApp.view m.alignment m.scene.navigation.camera.view
            //        |> Sg.map AlignmentActions
            //        |> Sg.fillMode (Mod.constant FillMode.Fill)
            //        |> Sg.cullMode (Mod.constant CullMode.None)

            let refSystem =
                ReferenceSystemApp.Sg.view
                    m.scene.config
                    mrefConfig
                    m.scene.referenceSystem
                    m.scene.navigation.camera.view
                |> Sg.map ReferenceSystemMessage  

            let exploreCenter =
                Navigation.Sg.view m.scene.navigation            
          
            
            //let controller = 
            //    adaptive {
            //        let! selected = m.scene.surfacesModel.surfaces.singleSelectLeaf
            //        let! kind = m.trafoKind
            //        match selected with
            //          | Some c ->                           

            //              let! sgo = m.scene.surfacesModel.sgSurfaces |> AMap.tryFind c
                          
            //              match sgo with 
            //                  | Some sg ->
            //                        let! trafo = sg.trafo
            //                        match kind with
            //                        | TrafoKind.Translate ->                                               
            //                            return TranslateController.viewController (fun t -> Translate("", t)) m.scene.navigation.camera.view sg.trafo
            //                        | TrafoKind.Rotate ->
            //                            return RotationController.viewController (fun t -> Rotate("", t)) m.scene.navigation.camera.view sg.trafo
            //                        | _ -> return Sg.empty
            //                  | None -> 
            //                    Log.warn "selected surface does not exist"
            //                    return Sg.empty                                 
            //          | None -> return Sg.empty
                    
            //    } |> Sg.dynamic

            //let orientationCube = 
            //    OrientationCube.Sg.view
            //        m.scene.navigation.camera.view
            //        m.scene.config 
            //        m.scene.referenceSystem
               
            
            let viewPlans =
                ViewPlan.Sg.view 
                    m.scene.config 
                    mrefConfig 
                    m.scene.viewPlans 
                    m.scene.navigation.camera.view
                |> Sg.map ViewPlanMessage
                        
            [annos; exploreCenter; refSystem; viewPlans] |> Sg.ofList // (*;orientationCube*)

        let cmds    = ViewerUtils.renderCommands m.scene.surfacesModel.sgGrouped overlayed discs m
        let cam     = Mod.map2 Camera.create m.scene.navigation.camera.view m.frustum          
        let control = DomNode.RenderControl(att, cam, cmds, None)
                                                                                         
        let config (model:MModel) = 
          ConfigProperties.view model.scene.config 
            |> UI.map ConfigPropertiesMessage 
            |> Mod.constant
          
        let configUI =
          div[][
            GuiEx.accordion "ViewerConfig" "Settings" true [
                 Incremental.div AttributeMap.empty (AList.ofModSingle (config m))
            ]
            GuiEx.accordion "Coordinate System" "Map Signs" false [
                ReferenceSystemApp.UI.view m.scene.referenceSystem m.scene.navigation.camera |> UI.map ReferenceSystemMessage
            ]
            GuiEx.accordion "Camera" "Camera Retro" false [
                CameraProperties.view m.scene.navigation.camera
            ]
          ]          
                
        let viewPlanProperties (model:MModel) =
            model.scene.viewPlans |> ViewPlan.UI.viewRoverProperties ViewPlanMessage 

        let viewPlannerUI =             
          div [][
            GuiEx.accordion "ViewPlans" "Write" true [
              ViewPlan.UI.viewViewPlans m.scene.viewPlans |> UI.map ViewPlanMessage
            ]
            GuiEx.accordion "Properties" "Content" true [
              Incremental.div AttributeMap.empty (viewPlanProperties m |> AList.ofModSingle)
            ]
          ]                
      
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
                    Html.Layout.boxH [ GuiEx.iconCheckBox m.scene.referenceSystem.isVisible ReferenceSystemApp.Action.ToggleVisible  ]                        
                    ] |> UI.map ReferenceSystemMessage
              | _ -> 
                return div[][]
          }
        
        let style' = "color: white; font-family:Consolas;"
        let scenepath = 
            Incremental.div (AttributeMap.Empty) (   
                    alist {
                        let! scenePath = m.scene.scenePath 
                        let icon = 
                          match scenePath with
                            | Some p ->  i [clazz "large folder icon" ; onClick (fun _ -> OpenSceneFileLocation p) ][] |> UI.wrapToolTip "open folder"  
                            | None -> div[][]  
                                
                        let scenePath   = Mod.bindOption m.scene.scenePath "" (fun sp -> Mod.constant sp)
                        yield  div [] [ 
                          //Html.Layout.boxH [ text "Scene Path:" ]
                          Html.Layout.boxH [ 
                            Incremental.text (scenePath 
                                |> Mod.map(fun x -> 
                                    if x.IsEmpty() then "*new scene" else Path.GetFileName x))  
                          ]
                          Html.Layout.boxH [ icon ]
                        ]
                    }
                )

        let topMenuItems = [
             Navigation.UI.viewNavigationModes m.scene.navigation  |> UI.map NavigationMessage 
          
             Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large wizard icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown m.interaction SetInteraction ]                
             ]                        
             Incremental.div  AttributeMap.empty (AList.ofModSingle dynamicTop)
          
             Html.Layout.horizontal [
               Html.Layout.boxH [ i [clazz "large Globe icon"][] ]
               Html.Layout.boxH [ Html.SemUi.dropDown m.scene.referenceSystem.planet ReferenceSystemApp.Action.SetPlanet ] |> UI.map ReferenceSystemMessage
             ] 
             Html.Layout.horizontal [
               Html.Layout.boxH [ scenepath ]
             ] 
        ]        

        let topMenu =
            div[clazz "ui menu"; style "padding:0; margin:0; border:0"] [
                yield Gui.TopMenu.menu m
                for t in topMenuItems do
                    yield div [clazz "item topmenu"] [t]
            ]
                                           
        let dnsColorLegend =
          FalseColorLegendApp.Draw.createColorLegendDnS m.drawing.colorLegend (V2d(100.0, 200.0))

        let scalarsColorLegend =
            let attr = 
              AttributeMap.ofList[ 
                "display"               => "block"; 
                "width"                 => "90px"; 
                "height"                => "75%"; //100%
                "preserveAspectRatio"   => "xMidYMid meet"; 
                "style"                 => "position:absolute; right: 5px; top: 25%"
              ]
            Incremental.Svg.svg attr (SurfaceApp.showColorLegend m.scene.surfacesModel)                        
                        
        let myCss = [
          { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
          { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
          { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]
        
        let ioverlayed =
            let annos = 
              annotations
                    |> Sg.map DrawingMessage
                    |> Sg.fillMode (Mod.constant FillMode.Fill)
                    |> Sg.cullMode (Mod.constant CullMode.None)
            [annos] |> Sg.ofList

        // instrument view control
        let icmds    = ViewerUtils.renderCommands m.scene.surfacesModel.sgGrouped ioverlayed discs m // m.scene.surfacesModel.sgGrouped overlayed discs m
        let icam     = Mod.map2 Camera.create m.scene.viewPlans.instrumentCam.view m.scene.viewPlans.instrumentFrustum   
        let icontrol = DomNode.RenderControl(att, icam, icmds, None) //AttributeMap.Empty
                       
                                 
        let bodyAttributes = [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden;"] //overflow-y : visible

        page (fun request -> 
          match Map.tryFind "page" request.queryParams with
            | Some "instrumentview" ->
                require (myCss) (
                  body [style "background: #1B1C1E; height:100%; width:100%"] [ 
                        icontrol 
                        Viewer.Gui.textOverlaysInstrumentView m.scene.viewPlans
                  ]
                )
            | Some "render" -> 
                require (myCss) (
                  body [ style "background: #1B1C1E; height:100%; width:100%"] [
                      div [style "background:#000;"] [
                          control
                          Viewer.Gui.textOverlays m.scene.referenceSystem m.scene.navigation.camera.view
                          dnsColorLegend
                          scalarsColorLegend ]
                  ]                
                )
            | Some "surfaces" -> 
                require (myCss) (
                    body bodyAttributes
                        [SurfaceApp.surfaceUI m.scene.surfacesModel |> UI.map SurfaceActions] 
                )
            | Some "annotations" -> 
                require (myCss) (body bodyAttributes [Gui.Annotations.annotationUI m])
            | Some "bookmarks" -> 
                require (myCss) (body bodyAttributes [Gui.Bookmarks.bookmarkUI m])
            | Some "config" -> 
                require (myCss) (body bodyAttributes [configUI])
            | Some "viewplanner" -> 
                require (myCss) (body bodyAttributes [viewPlannerUI])
            | None -> 
                require (myCss) (
                  body [][                    
                    topMenu
                    div[clazz "dockingMainDings"] [
                      m.scene.dockConfig
                        |> docking [                                           
                           style "width:100%; height:100%; background:#F00"
                           onLayoutChanged UpdateDockConfig ]
                    ]
                  ]
                )
            | _ -> body[][])                                            
                   
    let threadPool (m : Model) =
        let unionMany xs = List.fold ThreadPool.union ThreadPool.empty xs

        let drawing =
            DrawingApp.threads m.drawing |> ThreadPool.map DrawingMessage
       
        let animation = 
          AnimationApp.ThreadPool.threads m.animations |> ThreadPool.map AnimationMessage

        let nav =
          match m.scene.navigation.navigationMode with
            | NavigationMode.FreeFly -> 
                CameraController.threads m.scene.navigation.camera
                  |> ThreadPool.map Navigation.FreeFlyAction |> ThreadPool.map NavigationMessage
            | NavigationMode.ArcBall ->
                ArcBallController.threads m.scene.navigation.camera
                  |> ThreadPool.map Navigation.ArcBallAction |> ThreadPool.map NavigationMessage
            | _ -> failwith "invalid nav mode"
         
        unionMany [drawing; animation; nav]
        
    let loadWaypoints m = 
      match Serialization.fileExists "./waypoints.wps" with
        | Some path -> 
          let wp = Serialization.loadAs<plist<WayPoint>> path
          { m with waypoints = wp }
        | None -> m
    
    let start (runtime: IRuntime) (signature: IFramebufferSignature) messagingMailbox sendQueue =

        let m = 
            PRo3D.Viewer.Viewer.initial messagingMailbox 
              |> Scene.loadLastScene runtime signature
              |> loadRoverData
              |> loadLastAnnotations
       
        App.start {
            unpersist = Unpersist.instance
            threads   = threadPool
            view      = view //localhost
            update    = update runtime signature sendQueue messagingMailbox
            initial   = m
        }