namespace PRo3D.Viewer

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis

open PRo3D
open PRo3d
open PRo3D.Surfaces
open PRo3D.Navigation2
//open PRo3D.BookmarksX
open PRo3D.Groups
open PRo3D.ReferenceSystem
open Aardvark.Base.Frustum
open Aardvark.UI.Trafos
open PRo3d.Align
open Aardvark.UI.Animation

  
[<DomainType>] 
type Scene = {
    navigation        : NavigationModel
    interaction       : InteractionMode
    surfacesModel     : SurfaceModel  
    config            : ViewConfigModel
    scenePath         : option<string>
    referenceSystem   : ReferenceSystem
    orientationSystem : ReferenceSystem
    bookmarksModel    : BookmarksModel
    viewPlans         : ViewPlanModel
    dockConfig        : DockConfig
    closedPages       : list<DockElement>
}

[<DomainType>] 
type SceneHandle = {
    path        : string
    name        : string
    writeDate   : DateTime
}

[<DomainType>] 
type Recent = {
    recentScenes : hmap<string,SceneHandle>
}

type Properties = 
    | AnnotationProperties of Annotation
    | NoProperties

type Interactions =
    | PickExploreCenter     = 0
    | PlaceCoordinateSystem = 1  // compute up north vector at that point
    | DrawAnnotation        = 2
    | PlaceRover            = 3
    | TrafoControls         = 4
    | AlignPick             = 5
    | PlaceSurface          = 6

type TabMenu = 
    | Surfaces    = 0
    | Annotations = 1
    | Viewplanner = 2
    | Bookmarks   = 3
    | Config      = 4


type WayPoint = {
    name : string
    cv   : CameraView
}

type ViewerMode =
    | Standard
    | Instrument

[<DomainType>]
type Model = { 
   
    scene           : Scene
    drawing         : Drawing.DrawingModel   
    properties      : Properties
    interaction     : Interactions
    picking         : bool
    recent          : Recent
    waypoints       : plist<WayPoint>
    frustum         : Frustum
    overlayFrustum  : Frustum
    aspect          : double

    trafoKind       : TrafoKind
    trafoMode       : TrafoMode

    alignment       : AlignmentModel
    tabMenu         : TabMenu

    viewerMode      : ViewerMode

    animations      : AnimationModel
    
    [<TreatAsValue>]
    past : Option<Drawing.DrawingModel> 

    [<TreatAsValue>]
    future : Option<Drawing.DrawingModel> 
}


module Viewer =

    let navInit = 
        let init = NavigationModel.initial
        let init = (NavigationModel.Lens.camera |. CameraControllerState.Lens.sensitivity).Set(init, 3.0)
        let init = (NavigationModel.Lens.camera |. CameraControllerState.Lens.panFactor).Set(init, 0.0008)
        let init = (NavigationModel.Lens.camera |. CameraControllerState.Lens.zoomFactor).Set(init, 0.0008)
        init        

    let sceneElm = {id = "scene"; title = (Some "Scene"); weight = 0.4; deleteInvisible = None }
    
    let initial = {           
       // surfaces = HSet.empty
        //let test = SurfaceModel.initial
        scene = {
                navigation      = navInit
                interaction     = InteractionMode.PickOrbitCenter
                surfacesModel   = SurfaceModel.initial
                config          = ViewConfigModel.initial
                scenePath       = None
                referenceSystem = ReferenceSystem.initial
                orientationSystem = ReferenceSystem.initial
                bookmarksModel  = BookmarksModel.initial
                viewPlans       = ViewPlanModel.initial
                
                //groups          = AnnotationGroupsModel.initial

                dockConfig  =
                  config {
                      content (
                        //vertical 0.6 [
                         // element { id "topMenu1"; title "Menu"; weight 0.03 }
                          horizontal 1.0 [
                            
                            //element {id "sidebar"; title "Sidebar"; weight 0.3}
                            //element {id "render"; title "Renderview"; weight 0.6}
                            //element {id "instrumentview"; title "Instrumentview"; weight 0.6}
                            stack 0.7 None [
                                {id = "render"; title = Some "Renderview"; weight = 0.6; deleteInvisible = None}
                                {id = "instrumentview"; title = Some "Instrumentview"; weight = 0.6; deleteInvisible = None}
                            ]
                            //horizontal 0.4 [
                            //    element {id "scene"; title "Scene"; weight 0.4; }
                            //    element {id "surfaces"; title "Surfaces"; weight 0.4}
                            //    element {id "annotations"; title "Annotations"; weight 0.4}
                            //    element {id "bookmarks"; title "Bookmarks"; weight 0.4}
                            //    element {id "config"; title "Config"; weight 0.4}
                            //    element {id "viewplanner"; title "ViewPlanner"; weight 0.4}
                            //]
                            stack 0.3 None [
                                //{id = "scene"; title = Some "Scene"; weight = 0.4; deleteInvisible = None }
                                {id = "surfaces"; title = Some "Surfaces"; weight = 0.4; deleteInvisible = None }
                                {id = "annotations"; title = Some "Annotations"; weight = 0.4; deleteInvisible = None }
                                {id = "bookmarks"; title = Some "Bookmarks"; weight = 0.4; deleteInvisible = None }
                                {id = "config"; title = Some "Config"; weight = 0.4; deleteInvisible = None }
                                {id = "viewplanner"; title = Some "ViewPlanner"; weight = 0.4; deleteInvisible = None }
                            ]
                          ]
                  
                      //  ]
                      )
                      appName "PRo3D"
                      useCachedConfig false
                  }

                closedPages = list.Empty
                
        }

        drawing         = Drawing.DrawingModel.initialdrawing
        properties      = NoProperties
        interaction     = Interactions.DrawAnnotation
        picking         = false
        
        frustum         = Frustum.perspective 60.0 0.1 10000.0 1.0
        overlayFrustum  = Frustum.perspective 20.0 0.1 10000.0 1.0
        aspect          = 1.6

        recent          = { recentScenes = HMap.empty }
        waypoints       = PList.empty

        trafoKind       = TrafoKind.Rotate
        trafoMode       = TrafoMode.Local

        alignment       = Alignment.initModel

        past            = None
        future          = None

        tabMenu = TabMenu.Surfaces

        animations = 
         { 
           animations = PList.empty
           animation  = Animate.On
           cam        = CameraController.initial.view
         }
        
        //instrumentCamera = { CameraController.initial with view = CameraView.lookAt V3d.Zero V3d.One V3d.OOI }        
        //instrumentFrustum = Frustum.perspective 60.0 0.1 10000.0 1.0

        viewerMode = ViewerMode.Standard

        
    }