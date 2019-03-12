namespace PRo3D.Versioned

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open PRo3D
open Chiron

#nowarn "0686"

[<DomainType>]
type Statistics' = {
  version      : int
  average      : float
  min          : float
  max          : float
  stdev        : float
  sumOfSquares : float
}

module Statistics' =
  let current = 0
  //conversion
  let convert (old : Statistics) : Statistics' =
    failwith "convert non-versioned in versioned form"

  //initial
  let initial = 
     {
        version = current
        average      = Double.NaN
        min          = Double.NaN
        max          = Double.NaN
        stdev        = Double.NaN
        sumOfSquares = Double.NaN
      }
  //version modules
  module V0 =
    let read = 
      json {
        let! average      = Json.read "average"
        let! min          = Json.read "min"
        let! max          = Json.read "max"
        let! stdev        = Json.read "stdev"
        let! sumOfSquares = Json.read "sumOfSquares"        

        return {
          version      = current
          average      = average
          min          = min         
          max          = max         
          stdev        = stdev       
          sumOfSquares = sumOfSquares       
        }
      }
  
  type Statistics' with 
    static member FromJson(_ : Statistics') = 
      json {
          let! v = Json.read "version"
          match v with            
            | 0 -> return! V0.read 
            | _ -> return! v |> sprintf "don't know version %A  of Statistics" |> Json.error
      }
    static member ToJson (x : Statistics') =
      json {
          do! Json.write "sumOfSquares" x.sumOfSquares            
          do! Json.write "stdev" x.stdev            
          do! Json.write "max" x.max            
          do! Json.write "min" x.min                                 
          do! Json.write "average" x.average             
          do! Json.write "version" x.version
      }

[<DomainType>]
type DipAndStrikeResults' = {
    version         : int
    plane           : Plane3d
    dipAngle        : float
    dipDirection    : V3d
    strikeDirection : V3d
    dipAzimuth      : float
    strikeAzimuth   : float
    centerOfMass    : V3d
    error           : Statistics
}

module DipAndStrikeResults' =
  let current = 0
  //conversion
  let convert (old : DipAndStrikeResults) : DipAndStrikeResults' =
    failwith ""

  //initial
  //version modules

[<DomainType>]
type AnnotationResults' = {
    version     : int
    height      : float
    heightDelta : float
    avgAltitude : float
    length      : float
    wayLength   : float
    bearing     : float
    slope       : float
}

module AnnotationResults' =
  let current = 0
  //conversion
  let convert (old : AnnotationResults) : AnnotationResults' =
    failwith ""

  //initial
  //version modules

[<DomainType>]
type Annotation' = {
  version     : int

  [<PrimaryKey; NonIncremental>]
  key         : Guid

  modelTrafo  : Trafo3d

  geometry    : Geometry
  projection  : Projection
  semantic    : Semantic

  points      : plist<V3d>
  segments    : plist<Segment>

  color       : ColorInput
  thickness   : NumericInput

  results     : Option<AnnotationResults>
  dnsResults  : Option<DipAndStrikeResults>

  visible     : bool
  showDns     : bool
  text        : string
  textsize    : NumericInput

  surfaceName : string
  view        : CameraView
}

module Annotation' =
  let current = 0
  //conversion
  let convert (old : Annotation) : Annotation' =
    failwith ""

  //initial
  //version modules

[<DomainType>]
type ViewConfigModel' = {
    [<NonIncremental>]
    version                 : int
    nearPlane               : NumericInput
    farPlane                : NumericInput
    navigationSensitivity   : NumericInput
    importTriangleSize      : NumericInput
    arrowLength             : NumericInput
    arrowThickness          : NumericInput
    dnsPlaneSize            : NumericInput
    offset                  : NumericInput
    lodColoring             : bool
    drawOrientationCube     : bool
    }

#nowarn "0044"

module ViewConfigModel' =
  //convert from nonversioned to versioned
  let current = 1
  let convert (oldConfig : PRo3D.ViewConfigModel) : ViewConfigModel' = 
    {
        version               = 0
        nearPlane             = oldConfig.nearPlane
        farPlane              = oldConfig.farPlane
        navigationSensitivity = oldConfig.navigationSensitivity
        arrowLength           = oldConfig.arrowLength          
        arrowThickness        = oldConfig.arrowThickness
        dnsPlaneSize          = oldConfig.dnsPlaneSize       
        lodColoring           = oldConfig.lodColoring
        importTriangleSize    = oldConfig.importTriangleSize
        drawOrientationCube   = oldConfig.drawOrientationCube
        offset                = PRo3D.ViewConfigModel.depthOffset
    }

  let initial = {
    version = current
    nearPlane             = PRo3D.ViewConfigModel.initNearPlane
    farPlane              = PRo3D.ViewConfigModel.initFarPlane
    navigationSensitivity = PRo3D.ViewConfigModel.initNavSens
    arrowLength           = PRo3D.ViewConfigModel.initArrowLength
    arrowThickness        = PRo3D.ViewConfigModel.initArrowThickness
    dnsPlaneSize          = PRo3D.ViewConfigModel.initPlaneSize
    lodColoring           = false
    importTriangleSize    = PRo3D.ViewConfigModel.initImportTriangleSize        
    drawOrientationCube   = false
    offset                = PRo3D.ViewConfigModel.depthOffset       
  }
     
  module V0 =
    let read = 
      json {
        let! nearPlane                    = Json.readWith Ext.fromJson<NumericInput,Ext> "nearPlane"
        let! farPlane                     = Json.readWith Ext.fromJson<NumericInput,Ext> "farPlane"
        let! navigationSensitivity        = Json.readWith Ext.fromJson<NumericInput,Ext> "navigationSensitivity"
        let! arrowLength                  = Json.readWith Ext.fromJson<NumericInput,Ext> "arrowLength"
        let! arrowThickness               = Json.readWith Ext.fromJson<NumericInput,Ext> "arrowThickness"
        let! dnsPlaneSize                 = Json.readWith Ext.fromJson<NumericInput,Ext> "dnsPlaneSize"
        let! (lodColoring : bool)         = Json.read "lodColoring"
        let! importTriangleSize           = Json.readWith Ext.fromJson<NumericInput,Ext> "importTriangleSize"
        let! (drawOrientationCube : bool) = Json.read "drawOrientationCube"                        

        //return initial

        return {            
            version               = current
            nearPlane             = nearPlane
            farPlane              = farPlane
            navigationSensitivity = navigationSensitivity
            arrowLength           = arrowLength
            arrowThickness        = arrowThickness
            dnsPlaneSize          = dnsPlaneSize
            lodColoring           = lodColoring
            importTriangleSize    = importTriangleSize      
            drawOrientationCube   = drawOrientationCube
            offset                = PRo3D.ViewConfigModel.depthOffset               
        }
      }
  module V1 =
    let read = 
      json {
        let! nearPlane                    = Json.readWith Ext.fromJson<NumericInput,Ext> "nearPlane"
        let! farPlane                     = Json.readWith Ext.fromJson<NumericInput,Ext> "farPlane"
        let! navigationSensitivity        = Json.readWith Ext.fromJson<NumericInput,Ext> "navigationSensitivity"
        let! arrowLength                  = Json.readWith Ext.fromJson<NumericInput,Ext> "arrowLength"
        let! arrowThickness               = Json.readWith Ext.fromJson<NumericInput,Ext> "arrowThickness"
        let! dnsPlaneSize                 = Json.readWith Ext.fromJson<NumericInput,Ext> "dnsPlaneSize"
        let! (lodColoring : bool)         = Json.read "lodColoring"
        let! importTriangleSize           = Json.readWith Ext.fromJson<NumericInput,Ext> "importTriangleSize"
        let! (drawOrientationCube : bool) = Json.read "drawOrientationCube"                        
        let! depthoffset           = Json.readWith Ext.fromJson<NumericInput,Ext> "depthOffset"
    
        //return initial
    
        return {            
            version               = current
            nearPlane             = nearPlane
            farPlane              = farPlane
            navigationSensitivity = navigationSensitivity
            arrowLength           = arrowLength
            arrowThickness        = arrowThickness
            dnsPlaneSize          = dnsPlaneSize
            lodColoring           = lodColoring
            importTriangleSize    = importTriangleSize      
            drawOrientationCube   = drawOrientationCube
            offset = depthoffset
        }
      }
   
type ViewConfigModel' with 
  static member FromJson(_ : ViewConfigModel') = 
    json {
        let! v = Json.read "version"
        match v with
            | 0 -> return! ViewConfigModel'.V0.read
            | 1 -> return! ViewConfigModel'.V1.read
            | _ -> return! v |> sprintf "don't know version %A  of ViewConfigModel" |> Json.error
    }
  static member ToJson (x : ViewConfigModel') =
         json {
             do! Json.write "drawOrientationCube" x.drawOrientationCube                       
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "importTriangleSize" x.importTriangleSize
             do! Json.write "lodColoring" x.lodColoring
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "dnsPlaneSize" x.dnsPlaneSize
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "arrowThickness" x.arrowThickness
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "arrowLength" x.arrowLength
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "navigationSensitivity" x.navigationSensitivity
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "farPlane" x.farPlane
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "nearPlane" x.nearPlane
             do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "offset" x.offset
             do! Json.write "version" x.version
         }

module Testing = 
  let writeToFile path (contents : string) =
      System.IO.File.WriteAllText(path, contents)

  let readFromFile path =
      System.IO.File.ReadAllText(path)

  let writeStuff() =
    let oldconfig = ViewConfigModel.initial |> ViewConfigModel'.convert

    let v0Path = ".\configtest_v0.json"

    //let result = oldconfig |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty 
    //Log.line "writing configjson v%A to %A: \n %A" oldconfig.version v0Path result
    //result |> writeToFile v0Path

    let (v0config : ViewConfigModel') = v0Path |> readFromFile |> Json.parse |> Json.deserialize

  //  let (readConfig : ViewConfigModel') = result |> Json.parse |> Json.deserialize
    Log.line "reading configjson v%A: \n %A" v0config.version v0config
    //readConfig



