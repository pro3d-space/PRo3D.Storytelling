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

//open Viewer.Mutable.ModelModule.Lens
//open Mutable.AnnotationGroupModule.Lens
 
module ViewerApp =
    open Surface   
    open System.Collections.Concurrent   
    open Scene   
    
                
    let _surfacesModel   = Model.Lens.scene |. Scene.Lens.surfacesModel
    let _surfaces        = _surfacesModel   |. SurfaceModel.Lens.surfaces
    let _surfaces2       =  Model.Lens.scene |. Scene.Lens.surfacesModel |. SurfaceModel.Lens.surfaces    
    let _sgSurfaces      = _surfacesModel   |. SurfaceModel.Lens.sgSurfaces
    let _selectedSurface = _surfacesModel   |. SurfaceModel.Lens.selectedSurface
    
    let _drawing = Model.Lens.drawing
    let _camera= Model.Lens.scene |. Scene.Lens.navigation |. NavigationModel.Lens.camera
    let _navigation= Model.Lens.scene |. Scene.Lens.navigation

    let _annotations = Model.Lens.drawing |. DrawingModel.Lens.annotationGroups |. AnnotationGroupsModel.Lens.rootGroup |. AnnotationGroup.Lens.annotations
    let _flatAnnotations = Model.Lens.drawing |. DrawingModel.Lens.annotationGroups |. AnnotationGroupsModel.Lens.annotationsFlat
    let _groups = Model.Lens.drawing |. DrawingModel.Lens.annotationGroups |. AnnotationGroupsModel.Lens.rootGroup |. AnnotationGroup.Lens.subGroups
       
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
            getNearPlane       = fun (x:MViewConfigModel) -> x.nearPlane.value
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

    let update (w : Form) (runtime : IRuntime) (signature: IFramebufferSignature) (bc : BlockingCollection<string>) (m : Model) (msg : ViewerAction) =
        
        match msg, m.interaction, m.picking with
            | NavigationMessage  msg,_,false -> 
               
                let c   = m.scene.config
                let ref = m.scene.referenceSystem

                { m with scene = { m.scene with navigation = Navigation.update c ref navConf m.scene.navigation msg }}
            | SetCamera cv,_,false ->
                let lens = (Model.Lens.scene |. Scene.Lens.navigation |. NavigationModel.Lens.camera)
                let cam = lens.Get m
                let c' = { cam with view = cv }

                let m = lens.Set(m, c')

                m
            | AnnotationGroupsMessageViewer msg,_,_ ->
                let ag = m.drawing.annotationGroups 
                { m with drawing = { m.drawing with annotationGroups = AnnotationGroups.update ag msg}}
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
                                m.drawing.allAnnotations.AsList |> List.updateIf (fun x -> x.key = a'.key) (fun _ -> a') |> PList.ofList   

                            let flatAnnos = 
                                m.drawing.annotationGroups.annotationsFlat.AsList |> List.updateIf (fun x -> x.key = a'.key) (fun _ -> a') |> PList.ofList

                            let func = (fun (x:AnnotationGroup) -> { x with annotations = if PList.count x.annotations > 0 then AnnotationGroups.updateChild x.annotations a else x.annotations })
                            let path = m.drawing.annotationGroups.activeChildPath
                            let root = AnnotationGroups.updateItemAt path func m.drawing.annotationGroups.rootGroup
                            let groups = AnnotationGroups.updateGroups m.drawing.annotationGroups root flatAnnos

                            let drawing' = { m.drawing with selectedAnnotation = Some a'; allAnnotations = annotations'; annotationGroups = groups }

                            { m with drawing = drawing' }
                        | None -> m       
            | BookmarkMessage msg,_,_ ->    
                let model, bm = Bookmarks.update m.scene.bookmarksModel msg _navigation m
                { m with scene = { m.scene with bookmarksModel = bm; navigation = model.scene.navigation }} 
            | ImportSurface,_,_ -> 
                match Dialogs.openFolders w with
                    | [] -> m
                    | paths ->
                        let surfaces = 
                            paths 
                                |> List.filter Files.isSurfaceFolder
                                |> List.map Surface.mk
                                |> PList.ofList

                        Scene.import' runtime signature surfaces m  
            | ImportMeasurements,_,_ ->
                match Dialogs.openDialog w with
                    | Some x -> 
                        let newAnnotations = m.drawing.annotationGroups.rootGroup.annotations |> PList.append' (MeasurementsImporter.startImporter x)
                        let m' = _annotations.Set(m, newAnnotations) 
                        m'
                    | None -> m   
            | ImportAnnotationGroups,_,_ ->
                match Dialogs.openDialog w with
                    | Some x -> 
                        let newGroups = m.drawing.annotationGroups.rootGroup.subGroups |> PList.append' (AnnotationGroupsImporter.startImporter x)
                        let m' = _groups.Set(m, newGroups)
                        let flatA = AnnotationGroups.flattenAnnotations m'.drawing.annotationGroups.rootGroup |> PList.ofList
                        let m'' = _flatAnnotations.Set(m', flatA)
                        m''
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
                (Model.Lens.scene |. Scene.Lens.config).Set(m,c')
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
   
    let view (m : MModel) =        

        let renderControlAttributes (model : MNavigationModel) =            

            amap {
                let! state = model.navigationMode
                
                match state with
                    | NavigationMode.FreeFly -> 
                        yield! CameraController.extractAttributes model.camera Navigation.Action.FreeFlyAction (ViewerUtils.frustum m)
                    | NavigationMode.ArcBall -> 
                        yield! ArcBallController.extractAttributes model.camera Navigation.Action.ArcBallAction (ViewerUtils.frustum m)
                    | _ -> failwith "Invalid NavigationMode"
            } |> AttributeMap.ofAMap |> AttributeMap.mapAttributes (AttributeValue.map NavigationMessage)       

        let cam = Mod.map2 Camera.create m.scene.navigation.camera.view (ViewerUtils.frustum m)

        let att = 
            AttributeMap.unionMany [
                renderControlAttributes m.scene.navigation
                [attribute "style" "width:70%; height: 100%; float:left"; attribute "data-samples" "8" ] |> AttributeMap.ofList
                AttributeMap.ofList [
                    onKeyDown (KeyDown)
                    onKeyUp (KeyUp)
                    attribute "style" "width:70%; height: 100%; float:left"; ] 
            ]

        let overlayed =
            let annotations =
                DrawingApp.Sg.view m.scene.config mdrawingConfig m.drawing m.scene.navigation.camera.view
                    |> Sg.map DrawingMessage
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

            [ annotations; refSystem; exploreCenter] |> Sg.ofList
                                            
        let cmds = ViewerUtils.renderCommands m.scene.surfacesModel.sgGrouped overlayed m
        let control = DomNode.RenderControl(att, cam, cmds, None)

        let annotationGroupProperties (model:MModel) =
            adaptive {
                let bla = Some model.drawing.annotationGroups
                let opt = 
                    match bla with
                        | Some a -> AnnotationGroups.viewUI a
                        | None   -> div[][]

                return (opt |> UI.map AnnotationGroupsMessageViewer)
            } 
        
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
                        | Some a -> DipAndStrike.viewUI a
                        | None   -> div[][]

                return (opt |> UI.map DnSProperties)
            } 

        let bookmarkProperties (model:MModel) =
            adaptive {
                let! sbm = model.scene.bookmarksModel.selectedBookmark
                let opt = 
                    match sbm with
                        | Some a -> Bookmarks.UI.view a
                        | None   -> div[][]

                return (opt |> UI.map BookmarkMessage)
            } 

        let annotationUI = 
            let item2 = m.drawing.annotationGroups.lastSelectedItem 
                            |> Mod.map( fun x -> match x with 
                                                    | SelectedItem.AnnotationGroup -> annotationGroupProperties m
                                                    | _ -> annotationProperties m //SelectedItem.Annotation
                                                    )
            
            div [][
                DrawingApp.UI.viewAnnotationImportExport |> UI.map DrawingMessage
                    
                GuiEx.accordion "Annotations" "Write" true [
                    DrawingApp.UI.viewAnnotationGroups m.drawing |> UI.map DrawingMessage
                ]
                GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (alist{ let! item = item2
                                                               let! i = item
                                                               yield i})
                ] 
                GuiEx.accordion "Dip&Strike" "Calculator" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(dns m))] 
            ]

        //let annotationGroupsUI =
        //    div [][
        //        GuiEx.accordion "Annotations" "Write" true [
        //            AnnotationGroups.view m.scene.groups |> UI.map AnnotationGroupsMessage
        //        ]
        //    ]

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
            
        let bookmarkUI = 
            div [][
                Bookmarks.viewGUI |> UI.map BookmarkMessage

                GuiEx.accordion "Bookmarks" "Write" true [
                    Bookmarks.view m.scene.bookmarksModel |> UI.map BookmarkMessage 
                ]
                GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(bookmarkProperties m))
                ]
            ]
          
        let tabs = 
            GuiEx.tabbed [clazz "ui inverted segment"] [
                ("Data", SurfaceApp.surfaceUI m.scene.surfacesModel |> UI.map SurfaceActions) 
                ("Annotations", annotationUI) 
                ("Config", configUI)            
                ("Bookmarks", bookmarkUI)      
            ] "Data" 
      
        let topMenu = [
             Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large wizard icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown m.interaction SetInteraction ]                
             ]

             Navigation.UI.viewNavigationModes m.scene.navigation  |> UI.map NavigationMessage 
             DrawingApp.UI.viewAnnotationToolsHorizontal m.drawing |> UI.map DrawingMessage

             //button [onClick(fun _ -> QuickLoad1)] [text "quickload"]
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

        let importMeasurements =
            [ text "browse"] |> button [ 
                clazz "ui button"
                onClick (fun _ -> ImportMeasurements)
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

        require (Html.semui @ [myCss]) (
            let sections = [
                "blank", [div[style "height:15px"][]]                
                "Scene", [sceneTools]
                "Recent", [recentScenes]
                "Add Surface", [openSurface]
                "Import Annotations", [importMeasurements]
                "Import AnnotationGroups", [importAnnotationGroups]]                              
            onBoot usingContext (                            
                body [clazz "context example"] (GuiEx.adornerMenu sections topMenu (rest m))                                  
        ))
               
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


    let start (w : Form) (runtime: IRuntime) (signature: IFramebufferSignature) bc =
        let m = PRo3D.Viewer.Viewer.initial        
        let m = Scene.loadLastScene runtime signature m       
        
        App.start {
            unpersist = Unpersist.instance
            threads = pool runtime signature
            view = view
            update = update w runtime signature bc
            initial = m
        }