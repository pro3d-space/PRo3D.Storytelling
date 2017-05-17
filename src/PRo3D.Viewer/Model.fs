namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.Opc
open Aardvark.SceneGraph

type NavigationMode = FreeFly = 0 | ArcBall = 1
type InteractionMode = PickOrbitCenter = 0 | Draw = 1 | PlaceObject = 2

[<DomainType>]
type Surface = {    
    
    [<PrimaryKey>]
    guid : System.Guid
    globalBB : Box3d   
    sceneGraph : ISg
}

module Surface =     

    let mk sg bb =
        { guid = System.Guid.NewGuid(); sceneGraph = sg; globalBB = bb}

[<DomainType>] 
type Scene = {
    cameraModel : CameraControllerState
    importOpcs : list<string>
    interaction : InteractionMode
}

[<DomainType>]
type Model = { 
   
    scene : Scene
    surfaces   : hset<Surface>        

}

