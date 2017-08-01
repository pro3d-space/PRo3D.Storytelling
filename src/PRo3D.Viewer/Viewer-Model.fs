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
open PRo3D.Surfaces


  
[<DomainType>] 
type Scene = {
    navigation      : NavigationModel
    interaction     : InteractionMode
    surfaces        : SurfaceModel  
    config          : ViewConfigModel
    scenePath       : option<string>
    referenceSystem : ReferenceSystem
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
   // selectedSurface : option<Surface>
    properties      : Properties
    interaction     : Interactions
    picking         : bool       

    [<TreatAsValue>]
    past : Option<Drawing.DrawingModel> 

    [<TreatAsValue>]
    future : Option<Drawing.DrawingModel> 
  
    //measurementPath : string
  //  draw    : bool 
//    hoverPosition : option<Trafo3d>
}


module Viewer =

    let initial = {           
        surfaces = HSet.empty                       
        scene = {
                navigation      = NavigationModel.initial
                interaction     = InteractionMode.PickOrbitCenter
                surfaces        = Surface.initial                
                config          = ViewConfigModel.initial
                scenePath       = None
                referenceSystem = ReferenceSystem.initial
        }

        drawing         = Drawing.DrawingModel.initialdrawing
        properties      = NoProperties
        interaction     = Interactions.DrawAnnotation
        picking         = false
                
        past            = None
        future          = None
    }