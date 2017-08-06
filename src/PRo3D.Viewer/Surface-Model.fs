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
    guid          : System.Guid
    
    name          : string
    importPath    : string
    opcNames      : list<string>
    opcPaths      : list<string>    
    relativePaths : bool
    
    fillMode      : FillMode
    cullMode      : CullMode
    isVisible     : bool
    quality       : NumericInput
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
    sgSurfaces      : hset<SgSurface>
}

module SurfaceModel =
    open MBrace.FsPickler
    open MBrace.FsPickler.Combinators

    let makeSurfaceModel a b =
        {
            surfaces = a
            selectedSurface = b
            sgSurfaces = hset.Empty
        }

    let surfaceModelPickler : Pickler<SurfaceModel> =
        Pickler.product makeSurfaceModel
        ^+ Pickler.field (fun s -> s.surfaces)          Pickler.auto<list<Surface>>
        ^. Pickler.field (fun s -> s.selectedSurface)   Pickler.auto<option<Surface>>

    let initial = {
        surfaces        = []
        selectedSurface = None
        sgSurfaces      = hset.Empty
    }

    FsPickler.RegisterPicklerFactory (fun _ -> surfaceModelPickler)



