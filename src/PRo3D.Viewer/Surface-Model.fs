namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open DevILSharp.ILU
open Aardvark.VRVis
open PRo3D
open Aardvark.UI.Trafos
open Aardvark.SceneGraph.IO.Loader
open Aardvark.VRVis.Opc


[<DomainType>]
type ScalarLayer = {
    label        : string
    actualRange  : Range1d
    definedRange : Range1d
    index        : int
    colorLegend  : FalseColorsModel
}

[<DomainType>]
type ColorCorrection = {
    contrast    : NumericInput
    useContrast : bool
    brightness  : NumericInput
    useBrightn  : bool
    gamma       : NumericInput
    useGamma    : bool
    color       : ColorInput
    useColor    : bool
    useGrayscale: bool
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

type SurfaceType = 
    | SurfaceOPC = 0
    | SurfaceOBJ = 1

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
    isActive      : bool
    quality       : NumericInput
    priority      : NumericInput

    triangleSize  : NumericInput
    scaling       : NumericInput

    preTransform  : Trafo3d
    

    scalarLayers   : hmap<int, ScalarLayer> //plist<ScalarLayer>
    selectedScalar : option<ScalarLayer>

    textureLayers   : plist<TextureLayer>
    selectedTexture : option<TextureLayer>

    surfaceType     : SurfaceType     

    colorCorrection : ColorCorrection
    homePosition    : Option<CameraView>
    
}

type Picking =
  | PickMesh of ISg
  | KdTree   of hmap<Box3d,KdTrees.Level0KdTree>

[<DomainType>]
type SgSurface = {    
    [<NonIncremental>]
    surface     : Guid    
    trafo       : Transformation
    globalBB    : Box3d
    sceneGraph  : ISg
    picking     : Picking
}

module Init =
    let quality = 
            {
                value = 0.1
                min =  -2.0
                max = 5.0
                step = 0.1
                format = "{0:0.0}"
            }

    let triangleSize =
        {
            value = 100.0
            min = 0.0
            max = 100.0
            step = 0.01
            format = "{0:0.000}"
        }


    let priority = 
        {
            value = 0.0
            min =  0.0
            max = 10.0
            step = 1.0
            format = "{0:0}"
        }
    let scaling = 
        {
            value = 1.00
            min =  0.01
            max = 50.00
            step = 0.01
            format = "{0:0.00}"
        }

    // colorCorrection
    let contrast = 
        {
            value = 0.0
            min =  -255.0
            max = 255.0
            step = 1.0
            format = "{0:0}"
        }
    let brightness = 
        {
            value = 0.0
            min =  -255.0
            max = 255.0
            step = 1.0
            format = "{0:0}"
        }
    let gamma = 
        {
            value = 1.0
            min =  0.01
            max = 10.0
            step = 0.01
            format = "{0:0.00}"
        }

   

    let initColorCorrection = {
        contrast     = contrast
        useContrast  = false
        brightness   = brightness
        useBrightn   = false
        gamma        = gamma
        useGamma     = false
        color        = { c = C4b.Magenta }
        useColor     = false
        useGrayscale = false
    } 



