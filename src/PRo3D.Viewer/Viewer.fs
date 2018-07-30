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
            up    = (ReferenceSystem.Lens.up    |. V3dInput.Lens.value)
            north = (ReferenceSystem.Lens.north |. V3dInput.Lens.value)
        }

    let mdrawingConfig : DrawingApp.MSmallConfig<MViewConfigModel> =
        {            
            getNearPlane       = fun x -> x.nearPlane.value
            getHfov            = fun (x:MViewConfigModel) -> ((Mod.init 60.0) :> IMod<float>)
            //getMaxDipAngle     = fun (x:MViewConfigModel) -> x.maxDipAngle.value
            //getMinDipAngle     = fun (x:MViewConfigModel) -> x.minDipAngle.value
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
          
    let loadRoverData (m:Model) =
        let vps = ViewPlan.loadRoverData m.scene.viewPlans m.scene.scenePath
        { m with scene = { m.scene with viewPlans = vps } }

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
        let mutable ml = list.Empty
        match dnc with
            | DockNodeConfig.Vertical (weight,children) -> let test = children |> List.map(fun x -> getAllDockElements x )
                                                           test |> List.concat |> List.append ml  
            | DockNodeConfig.Horizontal (weight,children) -> let test = children |> List.map(fun x -> getAllDockElements x )
                                                             test |> List.concat |> List.append ml
            | DockNodeConfig.Stack (weight,activeId,children) -> List.append ml children
            | DockNodeConfig.Element element ->  List.append ml [element]
    
    let updateClosedPages (m : Model) (dncUpdated : DockNodeConfig) =
        let de = getAllDockElements m.scene.dockConfig.content
        let deUpdated = getAllDockElements dncUpdated
        let diff = ((Set.ofList de) - (Set.ofList deUpdated)) |> Set.toList
        // diff contains all changed elements (not only the deleted)
        match diff with
            | [] -> m.scene.closedPages
            | _ -> match (deUpdated |> List.filter(fun x -> x.id = diff.[0].id)) with
                    | [] -> List.append m.scene.closedPages diff
                    | _ -> m.scene.closedPages

    let addDockElement (dnc : DockNodeConfig) (de:DockElement) = 
        match dnc with
            | DockNodeConfig.Vertical (weight,children) -> let add = List.append children [(Stack(weight, None, [de]))]
                                                           Vertical(weight,add)
            | DockNodeConfig.Horizontal (weight,children) -> let add = List.append children [(Stack(weight, None, [de]))]
                                                             Horizontal(weight,add) 
            | DockNodeConfig.Stack (weight,activeId,children) -> Stack(weight, activeId, List.append [de] children)
            | DockNodeConfig.Element element ->  Stack(0.2, None, List.append [de] [element]) 

                       

    let matchPickingInteraction (bc : BlockingCollection<string>) (p:V3d) (hit:(V3d-> V3d option)) (surf:Surface) (m:Model) = 
         match m.interaction with
            | Interactions.DrawAnnotation -> 
                let m = match surf.surfaceType with
                        | SurfaceType.SurfaceOBJ -> { m with drawing = { m.drawing with projection = Projection.Linear } }   
                        | _ -> m
                Log.error "closest intersection on %A" surf.name
                let drawing = DrawingApp.update m.scene.referenceSystem drawingConfig bc m.drawing (Drawing.Action.AddPointAdv(p,hit,surf.name))
                //Log.stop()
                { m with drawing = drawing } |> stash
            | Interactions.PlaceCoordinateSystem -> 
                          
                let refAction = ReferenceSystemApp.Action.InferCoordSystem(p)
                let (refSystem',_)= 
                    ReferenceSystemApp.update m.scene.config refConfig (m.scene.referenceSystem) refAction  
                          
                let origin = new V3d(m.overlayFrustum.right * 0.5, m.overlayFrustum.bottom * 0.5, 1.0)
                let os = { refSystem' with origin = origin }
                let m = { m with scene = { m.scene with referenceSystem = refSystem'; orientationSystem = os }}
                          
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
            | Interactions.AlignPick -> 
                { m with alignment = AlignmentApp.update m.alignment (AlignmentActions.AddPoint({point = p; surfaceName = surf.guid}))}
            | Interactions.PlaceSurface -> 
                { m with scene = { m.scene with surfacesModel = SurfaceApp.update m.scene.surfacesModel (SurfaceApp.Action.PlaceSurface(p)) m.scene.scenePath }}
            | _ -> m

    let update (runtime : IRuntime) (signature: IFramebufferSignature) (bc : BlockingCollection<string>) (m : Model) (msg : ViewerAction) =        
        match msg, m.interaction, m.picking with
            | NavigationMessage  msg,_,false when (isGrabbed m |> not) && (not (AnimationApp.shouldAnimate m.animations)) ->                              
               let c   = m.scene.config
               let ref = m.scene.referenceSystem
               let nav = Navigation.update c ref navConf m.scene.navigation msg               

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
                        DrawingApp.update m.scene.referenceSystem drawingConfig bc m.drawing msg
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
                  | Bookmarks.Action.GroupsMessage k ->
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
                                |> List.map (Surface.mk SurfaceType.SurfaceOPC)
                                |> PList.ofList

                        let m = Scene.import' runtime signature surfaces m

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
                            let (orSystem',_)= 
                                ReferenceSystemApp.update m.scene.config refConfig (m.scene.orientationSystem) refAction  
                                                                
                            { m with scene = { m.scene with referenceSystem = refSystem'; orientationSystem = orSystem' }} |> updateCameraUp |> lookAtBoundingBox v.globalBB
                          | None -> m
            | ImportObject sl,_,_ -> 
                match sl with
                    | [] -> m
                    | paths ->
                        let p = paths.[0]
                        let objects = 
                            [p] 
                                |> List.map (Surface.mk SurfaceType.SurfaceOBJ)
                                |> PList.ofList

                        let m = Scene.importObj runtime signature objects m

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
                                                                
                            { m with scene = { m.scene with referenceSystem = refSystem' }} |> updateCameraUp |> lookAtBoundingBox v.globalBB
                          | None -> m                                                          
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
                    |> Lenses.set _flat flat
                
            | ImportSurfaceTrafo sl,_,_ ->
              match sl with
                | [] -> m 
                | p ->
                  let path = p.[0]
                  let imported = SurfaceTrafoImporter.startImporter path
                  let filterIds = File.ReadLines (Path.ChangeExtension(path,".filter")) |> Seq.toList

                  let k = 
                    imported 
                      |> PList.toList 
                      |> List.filter(fun x -> List.contains x.id filterIds) 
                      |> List.map (fun x ->
                        {
                          id = x.id
                          shift = x.trafo.Forward.C3.XYZ.Length
                        })

                  let csvTable = Csv.Seq.csv "," true id k
                  Csv.Seq.write (@"./shift.csv") csvTable |> ignore

                  let s = Surfaces.SurfaceApp.updateTrafos imported m.scene.surfacesModel
                  m |> Lenses.set _surfaceModelLens s                  
                  
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
                  matchPickingInteraction bc hit hitF surf m
                | None, _ -> 
                    Log.error ("no hit")
                    m
            | PickObject (p,id), _ ,_ ->  
                match m.picking with
                    |true ->
                          let hitF (p : V3d) = None
                          match (m.scene.surfacesModel.surfaces.flat.TryFind id) with
                            | Some x -> matchPickingInteraction bc p hitF (x |> Leaf.toSurface) m 
                            | None -> m
                    | false -> m
                
            | SaveScene s, _,_ -> 
                //match m.scene.scenePath with
                //    | None ->                        
                //        match Dialogs.saveDialog w with
                //            | None -> m
                //            | Some p -> 
                //                let s' = { m.scene with scenePath = Some p}
                //                { m with scene = Serialization.save p s'; } |> Model.stashAndSaveRecent p
                //    | Some path ->                        
                //        { m with scene = Serialization.save path m.scene } |> Model.stashAndSaveRecent path

               
                match m.scene.scenePath with
                    | None ->                        
                        match s.IsEmptyOrNull() with
                            | true -> m
                            | _ -> 
                                let s' = { m.scene with scenePath = Some s}
                                { m with scene = Serialization.save s s'; } |> Model.stashAndSaveRecent s
                    | Some path ->                        
                        { m with scene = Serialization.save path m.scene } |> Model.stashAndSaveRecent path
               
            | SaveAs s,_,_ ->
                //match Dialogs.saveDialog w with
                //    | None -> m
                //    | Some p -> 
                //        let s' = { m.scene with scenePath = Some p}                                
                //        { m with scene = Serialization.save p s' } |> Model.stashAndSaveRecent p
                 match s.IsEmptyOrNull() with
                    | true -> m
                    | _  -> 
                        let s' = { m.scene with scenePath = Some s}                                
                        { m with scene = Serialization.save s s' } |> Model.stashAndSaveRecent s
            | OpenScene sl,_,_ ->
                //match Dialogs.openDialog w with
                //    | None -> m
                //    | Some path -> 
                //        let m' = Scene.loadScene path m runtime signature |> Model.stashAndSaveRecent path
                //        loadRoverData m'
                match sl.Length < 1 with
                    | true -> m
                    | false -> 
                        let path = sl.[0]
                        let m' = Scene.loadScene path m runtime signature |> Model.stashAndSaveRecent path
                        loadRoverData m'
            | LoadScene path,_,_ ->
                Scene.loadScene path m runtime signature |> Model.stashAndSaveRecent path
            | NewScene,_,_ ->
                let m' = { PRo3D.Viewer.Viewer.initial with recent = m.recent }
                //m'
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
                  | Aardvark.Application.Keys.LeftCtrl -> { m with picking = true }
                  |_                                   -> { m with picking = false }
         
              let m =
                match m.interaction with
                | Interactions.DrawAnnotation -> 
                    let m = { m with drawing = DrawingApp.update m.scene.referenceSystem drawingConfig bc m.drawing drawingAction } |> stash
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
                  | Aardvark.Application.Keys.LeftCtrl -> DrawingApp.update m.scene.referenceSystem drawingConfig bc m.drawing Drawing.Action.StopDrawing
                  | _ -> m.drawing
                
                { m with drawing = d; picking = false } |> stash
            | SetInteraction t,_,_ -> { m with interaction = t }
            | ReferenceSystemMessage a,_,_ ->                                
                let refsystem',_ = ReferenceSystemApp.update m.scene.config refConfig m.scene.referenceSystem a
                let m = (Model.Lens.scene |. Scene.Lens.referenceSystem).Set(m, refsystem')
                let m = (Model.Lens.scene |. Scene.Lens.orientationSystem).Set(m, refsystem')
               
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
            | NoAction s,_,_ -> m
            | OrientationCube isg,_,_ -> m
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
   
    type MSgSurfaces with
                member x.trafo =
                    match x with
                        | MSgSurfaceOPC opc -> opc.trafo
                        | MSgSurfaceOBJ obj -> obj.trafo

    let view (m : MModel) = //(localhost: string)
        
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
                [attribute "style" "width:100%; height: 100%; float:left"; attribute "data-samples" "1" ] |> AttributeMap.ofList
                AttributeMap.ofList [
                    onKeyDown (KeyDown)
                    onKeyUp (KeyUp)
                    attribute "style" "width:100%; height: 100%; float:left"; ] 
            ]

        let annotations, discs =
                DrawingApp.Sg.view m.scene.config mdrawingConfig m.scene.navigation.camera.view m.drawing
            
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

            let orientationCube =
                aset {
                    let! draw = m.scene.config.drawOrientationCube
                    yield match draw with
                            | true -> OrientationCube.Sg.loadModel "../../data/rotationcube/rotationcube.dae"
                                        |> OrientationCube.Sg.orthoOrientation m.scene.navigation.camera.view
                                        |> Sg.map OrientationCube
                            | false -> Sg.empty
                } |> Sg.set 
               
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
            
            let viewPlans =
                ViewPlan.Sg.view 
                    m.scene.config 
                    mrefConfig 
                    m.scene.viewPlans 
                    m.scene.navigation.camera.view
                |> Sg.map ViewPlanMessage

            //let dnsColorLegend =
            //    FalseColorLegendApp.Draw.createFalseColorLegend 
            //        m.drawing.colorLegend 
            //        (Aardvark.Base.V2d(100.0, 200.0))
            //    |> Sg.map DnSColorLegendMessage

            //controller, exploreCenter
            [annos; exploreCenter; refSystem; alignment; viewPlans; orientationCube] |> Sg.ofList // orSystem; ; objects

        let cmds    = ViewerUtils.renderCommands m.scene.surfacesModel.sgGrouped overlayed discs m
        let cam     = Mod.map2 Camera.create m.scene.navigation.camera.view m.frustum          
        let control = DomNode.RenderControl(att, cam, cmds, None)

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
            
        let viewDnSColorLegendUI (model:MModel) = 
            adaptive { 
                return model.drawing.colorLegend |>  FalseColorLegendApp.viewDnSLegendProperties DnSColorLegendMessage
            }
            
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
            let sel = model.drawing.annotations.singleSelectLeaf
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
                //DrawingApp.UI.viewAnnotationImportExport |> UI.map DrawingMessage
               
                GuiEx.accordion "Annotations" "Write" true [
                    Groups.viewSelectionButtons |> UI.map AnnotationGroupsMessageViewer
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
               
                GuiEx.accordion "Dip&Strike ColorLegend" "paint brush" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(viewDnSColorLegendUI m))] 
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
                    ReferenceSystemApp.UI.view m.scene.referenceSystem m.scene.navigation.camera |> UI.map ReferenceSystemMessage
                ]

                GuiEx.accordion "Camera" "Camera Retro" false [
                    CameraProperties.view m.scene.navigation.camera
                ]
            ]          
        
        let bookmarksLeafButtonns (model:MModel) = 
            let ts = model.scene.bookmarksModel.bookmarks.activeChild
            let sel = model.scene.bookmarksModel.bookmarks.singleSelectLeaf
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
                br []
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

        let switchViewerMode =
            adaptive {
                return [ text "switch"] |> button [ 
                        clazz "ui button"
                        onClick (fun _ -> SwitchViewerMode)
                        ] 
                }

        
        let choosePlanet (model : MModel) = 
            Incremental.div (AttributeMap.Empty) (   
                alist {
                    yield div [] [
                            Html.Layout.boxH [ i [clazz "large Globe icon"][] ] //inverted
                            Html.Layout.boxH [ Html.SemUi.dropDown m.scene.referenceSystem.planet ReferenceSystemApp.Action.SetPlanet ]
                        ] |> UI.map ReferenceSystemMessage
                    yield br[]
                    let! scenePath = model.scene.scenePath 
                    let icon = 
                      match scenePath with
                        | Some p ->  i [clazz "large folder icon" ; onClick (fun _ -> OpenSceneFileLocation p) ][] |> UI.wrapToolTip "open folder"  //inverted
                        | None -> div[][] // i [clazz "large folder icon" ][] |> UI.wrapToolTip "open folder"  
                                
                    let scenePath   = Mod.bindOption model.scene.scenePath "" (fun sp -> Mod.constant sp)
                    yield  div [] [ 
                      //Html.Layout.boxH [ text "Scene Path:" ]
                      Html.Layout.boxH [ i [style "color:black"]
                        [Incremental.text (scenePath 
                            |> Mod.map(fun x -> 
                                if x.IsEmpty() then "*new scene" else Path.GetFileName x))]  
                      ]
                      Html.Layout.boxH [ icon ]
                    ]
                }
            )
               
        let topMenu = [
             Navigation.UI.viewNavigationModes m.scene.navigation  |> UI.map NavigationMessage 

             Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large wizard icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown m.interaction SetInteraction ]                
             ]                        
             Incremental.div  AttributeMap.empty (AList.ofModSingle dynamicTop)
             choosePlanet m
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
                   
        let pitchAndBearing (r:MReferenceSystem) (view:IMod<CameraView>) =
          adaptive {
            let! up    = r.up.value
            let! north = r.north.value   
            let! v     = view

            return (DipAndStrike.pitch up v.Forward, DipAndStrike.bearing up north v.Forward)
          }

        let textOverlays = 
            div [js "oncontextmenu" "event.preventDefault();"] [ 
              let planet = m.scene.referenceSystem.planet |> Mod.map(fun x -> sprintf "%s" (x.ToString()))  

              let view  = m.scene.navigation.camera.view 
              let pnb = pitchAndBearing m.scene.referenceSystem view

              let pitch    = pnb |> Mod.map(fun (p,_) -> sprintf "%s°" (p.ToString("0.00")))
              let bearing  = pnb |> Mod.map(fun (_,b) -> sprintf "%s°" (b.ToString("0.00")))
              
              let position = view |> Mod.map(fun x -> x.Location.ToString("0.00"))

              let spericalc = 
                Mod.map2 (fun (a : CameraView) b -> 
                  CooTransformation.getLatLonAlt(a.Location) b ) view m.scene.referenceSystem.planet
              
              let lon = spericalc |> Mod.map(fun x -> sprintf "%s°" ((x.longitude).ToString("0.00")))
              let lat = spericalc |> Mod.map(fun x -> sprintf "%s°" ((x.latitude).ToString("0.00")))
              let alt = spericalc |> Mod.map(fun x -> sprintf "%s°" ((x.altitude).ToString("0.00")))             
              
              let style' = "color: white; font-family:Consolas;"

              yield div [clazz "ui"; style "position: absolute; top: 45px; float:left" ] [
                  //arrowOverlay
                  yield table [] [
                      tr[][
                          td[style style'][Incremental.text planet]
                      ]
                      tr[][
                          td[style style'][text "Bearing: "]
                          td[style style'][Incremental.text bearing]
                      ]
                      tr[][
                          td[style style'][text "Pitch: "]
                          td[style style'][Incremental.text pitch]
                      ]
                      tr[][
                          td[style style'][text "Position: "]
                          td[style style'][Incremental.text position]
                      ]
                      tr[][
                          td[style style'][text "Longitude: "]
                          td[style style'][Incremental.text lon]
                      ]
                      tr[][
                          td[style style'][text "Latitude: "]
                          td[style style'][Incremental.text lat]
                      ]
                      tr[][
                          td[style style'][text "Altitude: "]
                          td[style style'][Incremental.text alt]
                      ]
                  ]
              ]                              
        ]
        
        let dnsColorLegend =
            FalseColorLegendApp.Draw.createColorLegendDnS 
                    m.drawing.colorLegend 
                    (Aardvark.Base.V2d(100.0, 200.0))

        let scalarsColorLegend =
            let attr = 
              AttributeMap.ofList[ 
                "display"               ==> "block"; 
                "width"                 ==> "55px"; 
                "height"                ==> "75%"; //100%
                "preserveAspectRatio"   ==> "xMidYMid meet"; 
                "viewBox"               ==> "0 0 5% 100%" 
                "style"                 ==> "position:absolute; left: 65%; top: 25%"
              ] // "0 0 5% 100%";
            Incremental.Svg.svg attr (SurfaceApp.showColorLegend m.scene.surfacesModel)
        
        let rest (model : MModel) = 
            [   
                control
                //orientationSystem model
                //arrowOverlay
                textOverlays
                dnsColorLegend
                scalarsColorLegend
                div [clazz "ui"; style "width: 30%; float:right; background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] [
                    div[style "height:41px"][text "spacer"]
                    //Incremental.div AttributeMap.empty (scenePath2 model) 
                    div [clazz "ui large black horizontal label"][text "Show Tab:"]
                    Html.SemUi.dropDown model.tabMenu SetTabMenu
                    viewTabs model.tabMenu                    
                ]            
            ]

        //Sidebar adorner                
        let sceneTools (model : MModel) (path : Option<string>) =
          let p = match path with
                    | Some p -> p
                    | None -> String.Empty

          div[][
           
            div [clazz "ui buttons inverted"] [ 

                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Save Scene"; Dialogs.onSaveFile SaveScene;
                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Scene'}]}));")] [ //{defaultPath: p}
                                                                                                                                
                                i [clazz "large save icon" ] [] 
                    ]
                )
               
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Save Scene As"; Dialogs.onSaveFile SaveAs;
                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({properties: []}));")] [
                        i [clazz "large icons"] [
                            i [clazz "save icon"] [] 
                            i [clazz "top right corner arrow down icon"] [] 
                        ]
                    ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Open Scene"; Dialogs.onChooseFiles OpenScene;
                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ] [
                        i [clazz "large folder outline icon" ] [] 
                    ]
                )

                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "New Scene"; onMouseClick (fun _ -> NewScene)] [
                        i [clazz "large file outline icon"] [] 
                    ]
                )
            ]

            br[]
            br[]

            choosePlanet m
          ]

        let openSurface =
            [ text "browse"] |> button [
                clazz "ui button"
                attribute "data-content" "text text text"
                Dialogs.onChooseFiles ImportSurface;
                clientEvent "onclick" ("parent.aardvark.processEvent('__ID__', 'onchoose', parent.aardvark.dialog.showOpenDialog({properties: ['openDirectory', 'multiSelections']}));")
            ]         

        let openObject =
            [ text "browse"] |> button [
                clazz "ui button"
                attribute "data-content" "text text text"
                Dialogs.onChooseFiles ImportObject;
                clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));")
            ]         

        let importAnnotationGroups =
            [ text "browse"] |> button [ 
                clazz "ui button"
                Dialogs.onChooseFiles ImportAnnotationGroups;
                clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));")
            ] 

        let importSurfaceTrafos =
            [ text "browse"] |> button [ 
                clazz "ui button"
                Dialogs.onChooseFiles ImportSurfaceTrafo;
                clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));")
            ] 

        let usingContext =             
            "$('.context.example .ui.sidebar')
              .sidebar({
                context: $('.context.example .bottom.segment.blarg')
              })
              .sidebar('attach events', '.context.example .menu.blarg .item.blarg')
              .sidebar('setting', 'transition', 'Overlay')
            ;"
        
        //let myCss = { kind = Stylesheet; name = "semui-overrides"; url = "./semui-overrides.css" }
        let myCss = [
          { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
          { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
          { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]

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
        // instrument view control
        let icmds    = ViewerUtils.renderCommands AList.empty Sg.empty Sg.empty m //m.scene.surfacesModel.sgGrouped
        let icam     = Mod.map2 Camera.create m.scene.viewPlans.instrumentCam.view m.scene.viewPlans.instrumentFrustum   
        let icontrol = DomNode.RenderControl(att, icam, icmds, None) //AttributeMap.Empty
        
        let scenemenu = 
                div [clazz "menu-bar"] [
                        // menu
                        div [ clazz "ui top attached menu"; style "z-index: 1000"] [
                            onBoot "$('#__ID__').dropdown('on', 'hover');" (
                                div [ clazz "ui dropdown item" ] [
                                    i [clazz "large sidebar icon"][] 

                                    div [ clazz "ui inverted menu" ] [

                                        div [ clazz "ui dropdown item"] [
                                            text "Scene"
                                            i [clazz "dropdown icon"][] 
                                            div [ clazz "menu"] [
                                                div [ clazz "ui inverted item"; Dialogs.onSaveFile SaveScene;
                                                      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Scene'}]}));")][
                                                      text "Save"
                                                ]
                                                div [ clazz "ui inverted item"; Dialogs.onSaveFile SaveAs;
                                                      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({properties: []}));") ][
                                                      text "Save as..."
                                                ]
                                                div [ clazz "ui inverted item"; 
                                                      Dialogs.onChooseFiles OpenScene;
                                                      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));")][
                                                      text "Open"
                                                ]
                                                div [ clazz "ui inverted item"; onMouseClick (fun _ -> NewScene)][
                                                      text "New"
                                                ]
                                                div[ clazz "ui inverted item" ] [
                                                    onBoot """$('#__ID__').popup({inline:true,hoverable:true, position   : 'right center'});""" (
                                                        text "Recent"
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
                                            ]
                                        ]

                                        div [ clazz "ui dropdown item"] [
                                            text "Open"
                                            i [clazz "dropdown icon"][] 
                                            div [ clazz "menu"] [
                                                div [ clazz "ui inverted item"; 
                                                      Dialogs.onChooseFiles ImportSurface;
                                                      clientEvent "onclick" ("parent.aardvark.processEvent('__ID__', 'onchoose', parent.aardvark.dialog.showOpenDialog({properties: ['openDirectory', 'multiSelections']}));") ][
                                                      text "Surface"
                                                ]
                                                div [ clazz "ui inverted item"; 
                                                    Dialogs.onChooseFiles ImportObject;
                                                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ][
                                                    text "Object"
                                                ]
                                            ]
                                        ]
                                        div [ clazz "ui dropdown item"] [
                                            text "Import"
                                            i [clazz "dropdown icon"][] 
                                            div [ clazz "menu"] [
                                                div [ clazz "ui inverted item";
                                                    Dialogs.onChooseFiles ImportAnnotationGroups;
                                                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ][
                                                    text "Annotations Groups"
                                                ]
                                                div [ clazz "ui inverted item";
                                                    Dialogs.onChooseFiles ImportSurfaceTrafo;
                                                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ] [
                                                    text "Surface Trafos"
                                                ]
                                            ]
                                        ]

                                        DrawingApp.UI.annotationMenu  |> UI.map DrawingMessage;
                                        
                                        div[ clazz "ui inverted item" ] [
                                            onBoot """$('#__ID__').popup({inline:true,hoverable:true, position   : 'right center'});""" (
                                                text "Closed Pages"
                                            )

                                            div [clazz "ui flowing popup bottom left transition hidden"] [
                                                Incremental.div (AttributeMap.ofList [clazz "ui link list"]) (
                                                            aset {
                                                                let! closedP = m.scene.closedPages
                                                                for cp in closedP do
                                                                     yield a [clazz "item inverted";
                                                                              onClick (fun _ -> AddPage cp)] [
                                                                        span [style "color:black"] [Incremental.text (cp.id |> Mod.constant)]
                                                                     ]
                                                            } |> AList.ofASet
                                                        )
                                            ]
                                        ]
                                    ] 
                                ]
                            )
                        ]
                    ]
                       
        let topMenu1 =
            div[clazz "ui top small fixed menu blarg"] [
                yield 
                    a [clazz "item blarg topmenu"] [ scenemenu 
                        //i [clazz "large sidebar icon"] []
                ]
                for t in topMenu do
                    yield div [clazz "item topmenu"] [t]
                
            ]
           
            
                
        //let control2 = 
        //    alist {
        //        yield div[](GuiEx.adornerMenu [] [] [icontrol])
        //        }
         
        //let mainview = 
        //    require (GuiEx.semui @ [myCss]) (                             
        //        onBoot usingContext (                             
        //            body [clazz "context example"; style "overflow-y:hidden"] [Incremental.div AttributeMap.empty control1] 
        //        )
        //    )

        //let instrumentview = 
        //    require (GuiEx.semui @ [myCss]) (                             
        //        onBoot usingContext (                             
        //            body [clazz "context example"; style "overflow-y:hidden"] [Incremental.div AttributeMap.empty control2] 
        //        )
        //    )
            
        //require (GuiEx.semui @ [myCss]) (                             
        //        onBoot usingContext (                             
        //            body [clazz "context example"; style "overflow-y:hidden"] [chooseView] //ViewerMode.Standard
        //        )
        //    )

        let st = 
            require GuiEx.semui (
                                Html.table [        
                                    Html.row "Recent:"       [recentScenes]
                                    Html.row "Add Surface:"  [openSurface ]
                                    Html.row "Add Object:"  [openObject ]
                                    Html.row "Import AnnotationGroups:"  [importAnnotationGroups ]
                                    Html.row "Import SurfaceTrafos:"  [importSurfaceTrafos ]
                                ]
                            )
        let sc = 
                alist {
                        let! path = m.scene.scenePath
                        yield (sceneTools m path)
                }

        let sceneTab =
            div[][
                br[]
                Incremental.div AttributeMap.empty sc
                GuiEx.accordion "SceneConfig" "Settings" true [
                     Incremental.div AttributeMap.empty (alist{ yield st })
                ]
            ]          

        page (fun request -> 
                match Map.tryFind "page" request.queryParams with
                    
                    //| Some "main" -> 
                    //    body [] [ mainview ]
                    //| Some "topMenu1" -> 
                    //    body [style "background: #1B1C1E;"] [sidemenu] //Incremental.div AttributeMap.empty 

                    | Some "instrumentview" ->
                        body [style "background: #1B1C1E; height:100%; width:100%"] [ icontrol ]

                    | Some "render" -> 
                        body [ style "background: #1B1C1E; height:100%; width:100%"] [
                            div [clazz "pusher"; style "background:#000;"] [
                                control
                                textOverlays
                                dnsColorLegend
                                scalarsColorLegend ]
                        ]

                    //| Some "tabs" -> 
                    //    body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                    //        [Html.SemUi.dropDown m.tabMenu SetTabMenu
                    //         viewTabs m.tabMenu               ] 

                    //| Some "scene" -> 
                    //    require (myCss) (
                    //        body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                    //            [sceneTab] 
                    //    )

                    | Some "surfaces" -> 
                        require (myCss) (
                            body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                                [SurfaceApp.surfaceUI m.scene.surfacesModel |> UI.map SurfaceActions] 
                        )

                    | Some "annotations" -> 
                        require (myCss) (
                            body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                                [annotationUI] 
                        )

                    | Some "bookmarks" -> 
                        require (myCss) (
                            body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                                [bookmarkUI] 
                        )

                    | Some "config" -> 
                        require (myCss) (
                            body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                                [configUI]
                        )

                    | Some "viewplanner" -> 
                        require (myCss) (
                            body [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden; overflow-y : visible"] 
                                [viewPlannerUI] 
                        )

                    | None -> 
                        require (myCss) (
                            body [][
                                //sidemenu
                                topMenu1
                                //let dc = 
                                m.scene.dockConfig |> Mod.force |> Mod.constant |> docking [
                                            style "width:100%;height:100%;"
                                            onLayoutChanged UpdateDockConfig ]
                                //yield Incremental.div AttributeMap.empty (alist{ yield topMenu1; yield dc })
                                //yield div[](GuiEx.adornerMenu sections topMenu [dc])
                            ]
                        )
                    | _ -> body [] []
                    )//]
                    //]
        
        //let control1 = 
        //    alist {
        //        let! mode = m.viewerMode
        //        let! path = m.scene.scenePath
        //        let sections = [
        //            "blank", [div[style "height:15px"][]]
        //            "Scene", [sceneTools m path]
        //            // "Scene Path", [scenePath2 m]
        //            "Recent", [recentScenes]
        //            "Add Surface", [openSurface]      
        //            "Add Object", [openObject]      
        //            "Import AnnotationGroups", [importAnnotationGroups]  
        //            "Import SurfaceTrafos", [importSurfaceTrafos]]
        //            //"Switch View", [switchViewerMode] 
                            
        //        yield div[](GuiEx.adornerMenu sections topMenu [ppages])

        //        }
       
        
                   
    let pool (runtime : IRuntime) (signature: IFramebufferSignature) (m : Model) =        
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

    let loadLastAnnotations (m:Model) = 
      match Serialization.fileExists "./lastmeasure" with
        | Some path ->             
            //let groups = Groups.tryLoadGroups path 
            let groups = Groups.tryLoadAnnotations path 
            match groups with
            //| Some g -> g |> Lenses.set' _annotations m
            | Some g -> { m with drawing = { m.drawing with annotations = g.annotations; colorLegend = g.colorLegend } }
            | None -> m
        | None -> m

    let start (runtime: IRuntime) (signature: IFramebufferSignature) bc =
        let m = 
            PRo3D.Viewer.Viewer.initial 
              |> Scene.loadLastScene runtime signature
              |> loadRoverData
              |> loadLastAnnotations  
              
        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view //localhost
            update = update runtime signature bc
            initial = m
        }