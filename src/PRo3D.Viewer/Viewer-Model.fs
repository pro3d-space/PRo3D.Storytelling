namespace PRo3D.Viewer

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.Opc
open Aardvark.SceneGraph
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

  
[<DomainType>] 
type Scene = {
    navigation      : NavigationModel
    interaction     : InteractionMode
    surfacesModel   : SurfaceModel  
    config          : ViewConfigModel
    scenePath       : option<string>
    referenceSystem : ReferenceSystem
    bookmarksModel  : BookmarksModel
    viewPlans       : ViewPlanModel
    //groups          : AnnotationGroupsModel
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

type WayPoint = {
    name : string
    cv   : CameraView
}

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
    aspect          : double

    trafoKind       : TrafoKind
    trafoMode       : TrafoMode

    alignment       : AlignmentModel

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

    let initial = {           
       // surfaces = HSet.empty
        scene = {
                navigation      = navInit
                interaction     = InteractionMode.PickOrbitCenter
                surfacesModel   = SurfaceModel.initial
                config          = ViewConfigModel.initial
                scenePath       = None
                referenceSystem = ReferenceSystem.initial
                bookmarksModel  = BookmarksModel.initial
                viewPlans       = ViewPlanModel.initial
                
                //groups          = AnnotationGroupsModel.initial
        }

        drawing         = Drawing.DrawingModel.initialdrawing
        properties      = NoProperties
        interaction     = Interactions.DrawAnnotation
        picking         = false
        
        frustum         = Frustum.perspective 60.0 0.1 10000.0 1.0
        aspect          = 1.6

        recent          = { recentScenes = HMap.empty }
        waypoints       = PList.empty

        trafoKind       = TrafoKind.Rotate
        trafoMode       = TrafoMode.Local

        alignment       = Alignment.initModel

        past            = None
        future          = None
    }