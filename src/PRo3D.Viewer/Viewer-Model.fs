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


[<DomainType>]
type SgSurface = {
    //[<PrimaryKey>]
    //guid : System.Guid

    surface : Surface    

    globalBB : Box3d   
    sceneGraph : ISg
    kdTree : KdTreeSet    
}
  
[<DomainType>] 
type Scene = {
    cameraModel : CameraControllerState 
    navigation  : NavigationModel
    interaction : InteractionMode
    surfaces : list<Surface>
}

type Properties = 
    | AnnotationProperties of Annotation
    | NoProperties

type Interactions =
    | PickExploreCenter     = 0
    | PlaceCoordinateSystem = 1  // compute up north vector at that point
    | DrawAnnotation        = 2

[<DomainType>]
type Model = { 
   
    scene           : Scene
    drawing         : Drawing.DrawingModel
    surfaces        : hset<SgSurface>        
    selectedSurface : option<Surface>
    properties      : Properties
    interaction     : Interactions
    picking         : bool
    orientations    : OrientationDisplayModel
    
    //measurementPath : string
  //  draw    : bool 
//    hoverPosition : option<Trafo3d>
}


module Viewer =

    let initial = {           
        surfaces = HSet.empty                       
        scene = {
                cameraModel = CameraController.initial       
                navigation = NavigationModel.initial
                interaction = InteractionMode.PickOrbitCenter
                surfaces = []
        }

        drawing = Drawing.Model.initialdrawing
        properties = NoProperties
        interaction = Interactions.DrawAnnotation
        picking = false
        orientations = OrientationDisplayModel.initial
        selectedSurface = None
        //measurementPath = @"."

        // draw = false
        //hoverPosition = None
    }