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
type Model = { 

    cameraModel : CameraControllerState

    importOpcs : list<string>

    surfaces   : hset<Surface>        
}

