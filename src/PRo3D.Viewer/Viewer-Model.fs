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
    [<PrimaryKey>]
    guid : System.Guid

    surface : Surface    

    globalBB : Box3d   
    sceneGraph : ISg
    kdTree : KdTreeSet    
}
  
[<DomainType>] 
type Scene = {
    navigation  : NavigationModel
    interaction : InteractionMode
    surfaces : list<Surface>
    referenceSystem : ReferenceSystem
    config : ViewConfigModel
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
    //measurementPath : string
  //  draw    : bool 
//    hoverPosition : option<Trafo3d>
}


module Viewer =

    let initial = {           
        surfaces = HSet.empty                       
        scene = {
                navigation = NavigationModel.initial
                interaction = InteractionMode.PickOrbitCenter
                surfaces = []
                referenceSystem = ReferenceSystem.initial
                config = ViewConfigModel.initial
        }

        drawing = Drawing.Model.initialdrawing
        properties = NoProperties
        interaction = Interactions.DrawAnnotation
        picking = false
        
        selectedSurface = None
        //measurementPath = @"."

        // draw = false
        //hoverPosition = None
    }