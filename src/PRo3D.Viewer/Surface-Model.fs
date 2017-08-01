namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
open Aardvark.VRVis
open Aardvark.SceneGraph

[<DomainType>]
type Surface = {    
    
    [<PrimaryKey>]
    guid        : System.Guid
    
    name        : string
    importPath  : string
    opcPaths    : list<string>
    
    fillMode    : FillMode
    cullMode    : CullMode
    isVisible   : bool
    quality     : NumericInput
}

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
type SurfaceModel = {
    surfaces        : list<Surface>
    selectedSurface : option<Surface>
    //sgSurfaces : list<SgSurface>
}



