namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
open Aardvark.SceneGraph
open DevILSharp.ILU
open Aardvark.VRVis
open PRo3D
open Aardvark.UI.Trafos


//[<DomainType>]
type ScalarLayer = {
    label        : string
    actualRange  : Range1d
    definedRange : Range1d
    index        : int
}

//[<DomainType>]
type TextureLayer = {
    label : string
    index : int
}

//[<DomainType>]
type AttributeLayer = 
    | ScalarLayer of ScalarLayer 
    | TextureLayer of TextureLayer

//[<DomainType>]
//type SurfaceAttributes = {
//    attributeLayers : list<AttributeLayer>
//}

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
    priority      : NumericInput

    preTransform  : Trafo3d
    

    scalarLayers   : plist<ScalarLayer>
    selectedScalar : option<ScalarLayer>

    textureLayers   : plist<TextureLayer>
    selectedTexture : option<TextureLayer>
}

[<DomainType>]
type SgSurface = {    
    surface     : Surface    
    trafo       : Transformation
    globalBB    : Box3d
    sceneGraph  : ISg
    kdTree      : hmap<Box3d,Level0KdTree>
}

//[<DomainType>]
//type SurfaceModel = {
//    surfaces        : plist<Surface>
//    selectedSurface : option<Surface>
//    sgSurfaces      : hmap<Guid,SgSurface>
//    sgGrouped       : plist<hmap<Guid,SgSurface>>
//    kdTreeCache     : hmap<string, ConcreteKdIntersectionTree>
//}

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module SurfaceModel =
//    open MBrace.FsPickler
//    open MBrace.FsPickler.Combinators    

//    let makeSurfaceModel a b =
//        {
//            surfaces = a
//            selectedSurface = b
//            sgSurfaces = hmap.Empty
//            sgGrouped = plist.Empty
//            kdTreeCache = hmap.Empty
//        }

//    let surfaceModelPickler : Pickler<SurfaceModel> =
//        Pickler.product makeSurfaceModel
//        ^+ Pickler.field (fun s -> s.surfaces)          Pickler.auto<plist<Surface>>
//        ^. Pickler.field (fun s -> s.selectedSurface)   Pickler.auto<option<Surface>>

//    let initial = makeSurfaceModel PList.empty None    

    
//    registry.RegisterFactory (fun _ -> surfaceModelPickler)

////    //FsPickler.RegisterPicklerFactory (fun _ -> surfaceModelPickler)


module Init =
    let quality = 
            {
                value = 0.1
                min =  -2.0
                max = 5.0
                step = 0.1
                format = "{0:0.0}"
            }

    let priority = 
        {
            value = 0.0
            min =  0.0
            max = 10.0
            step = 1.0
            format = "{0:0}"
        }

