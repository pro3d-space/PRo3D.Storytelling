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
    interaction : InteractionMode
    surfaces : list<Surface>
}

type Properties = 
    | AnnotationProperties of Annotation
    | NoProperties

[<DomainType>]
type Model = { 
   
    scene       : Scene
    drawing     : Drawing.DrawingModel
    surfaces    : hset<SgSurface>        
    properties  : Properties

    //measurementPath : string
  //  draw    : bool 
//    hoverPosition : option<Trafo3d>
}


module Viewer =

    let initial = {           
        surfaces = HSet.empty                       
        scene = {                   
                cameraModel = CameraController.initial            
                interaction = InteractionMode.PickOrbitCenter
                surfaces = []
        }

        drawing = Drawing.Model.initialdrawing
        properties = NoProperties
        //measurementPath = @"."

        // draw = false
        //hoverPosition = None
    }