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
with 
  //version modules  
  static member current = 0
  static member private readV0 = 
      json {
        let! average      = Json.read "average"
        let! min          = Json.read "min"
        let! max          = Json.read "max"
        let! stdev        = Json.read "stdev"
        let! sumOfSquares = Json.read "sumOfSquares"
  
        return {
          version      = Statistics'.current
          average      = average
          min          = min         
          max          = max         
          stdev        = stdev       
          sumOfSquares = sumOfSquares       
        }
      }

  static member FromJson(_ : Statistics') = 
    json {
        let! v = Json.read "version"
        match v with            
          | 0 -> return! Statistics'.readV0
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

module Statistics' =
  let current = 0
  //conversion
  let convert (old : Statistics) : Statistics' =
    {
      version      = current
      average      = old.average     
      min          = old.min         
      max          = old.max         
      stdev        = old.stdev       
      sumOfSquares = old.sumOfSquares
    }

  //initial
  let initial = 
     {
        version      = current
        average      = Double.NaN
        min          = Double.NaN
        max          = Double.NaN
        stdev        = Double.NaN
        sumOfSquares = Double.NaN
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
    error           : Statistics'
}
with 
  static member current = 0
  static member private readV0 = 
    json {
      let! plane           = Json.read "plane"
      let! dipAngle        = Json.read "dipAngle"
      let! dipDirection    = Json.read "dipDirection"
      let! strikeDirection = Json.read "strikeDirection"
      let! dipAzimuth      = Json.read "dipAzimuth"
      let! strikeAzimuth   = Json.read "strikeAzimuth"
      let! centerOfMass    = Json.read "centerOfMass"
      let! error           = Json.read "error"

      return {
          version         = DipAndStrikeResults'.current
          plane           = plane |> Plane3d.Parse
          dipAngle        = dipAngle
          dipDirection    = dipDirection |> V3d.Parse
          strikeDirection = strikeDirection |> V3d.Parse
          dipAzimuth      = dipAzimuth
          strikeAzimuth   = strikeAzimuth
          centerOfMass    = centerOfMass |> V3d.Parse
          error           = error
      }
    }
    
  static member FromJson(_ : DipAndStrikeResults') = 
    json {
        let! v = Json.read "version"
        match v with            
          | 0 -> return! DipAndStrikeResults'.readV0
          | _ -> return! v |> sprintf "don't know version %A  of DipAndStrikeResults" |> Json.error
    }
  static member ToJson (x : DipAndStrikeResults') =
    json {
        do! Json.write "version"          x.version                    
        do! Json.write "plane"            (x.plane.ToString())
        do! Json.write "dipAngle"         x.dipAngle          
        do! Json.write "dipDirection"     (x.dipDirection.ToString())
        do! Json.write "strikeDirection"  (x.strikeDirection.ToString())        
        do! Json.write "dipAzimuth"       x.dipAzimuth     
        do! Json.write "strikeAzimuth"    x.strikeAzimuth  
        do! Json.write "centerOfMass"     (x.centerOfMass.ToString())
        do! Json.write "error"            x.error
    }

module DipAndStrikeResults' =  
  //conversion
  let convert (old : DipAndStrikeResults) : DipAndStrikeResults' =
    {
        version         = DipAndStrikeResults'.current
        plane           = old.plane          
        dipAngle        = old.dipAngle       
        dipDirection    = old.dipDirection   
        strikeDirection = old.strikeDirection
        dipAzimuth      = old.dipAzimuth     
        strikeAzimuth   = old.strikeAzimuth  
        centerOfMass    = old.centerOfMass   
        error           = old.error |> Statistics'.convert
    }

  //initial
  let initial = 
    {
        version         = DipAndStrikeResults'.current
        plane           = Plane3d.Invalid      
        dipAngle        = Double.NaN  
        dipDirection    = V3d.NaN
        strikeDirection = V3d.NaN
        dipAzimuth      = Double.NaN  
        strikeAzimuth   = Double.NaN  
        centerOfMass    = V3d.NaN  
        error           = Statistics'.initial     
    }  
        
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
with 
  static member current = 0
  static member private readV0 =
    json {      
      let! height     = Json.read "height"     
      let! heightDelta= Json.read "heightDelta"
      let! avgAltitude= Json.read "avgAltitude"
      let! length     = Json.read "length"     
      let! wayLength  = Json.read "wayLength"  
      let! bearing    = Json.read "bearing"    
      let! slope      = Json.read "slope"
      
      return {
        version     = AnnotationResults'.current    
        height      = height     
        heightDelta = heightDelta
        avgAltitude = avgAltitude
        length      = length     
        wayLength   = wayLength  
        bearing     = bearing    
        slope       = slope            
      }
    }
  static member FromJson(_: AnnotationResults') =
    json {
      let! v = Json.read "version"
      match v with 
        | 0 -> return! AnnotationResults'.readV0
        | _ -> return! v |> sprintf "don't know version %A  of AnnotationResults" |> Json.error
    }
  static member ToJson (x : AnnotationResults') =
    json {
      do! Json.write "version"     x.version     
      do! Json.write "height"      x.height      
      do! Json.write "heightDelta" x.heightDelta 
      do! Json.write "avgAltitude" x.avgAltitude 
      do! Json.write "length"      x.length      
      do! Json.write "wayLength"   x.wayLength   
      do! Json.write "bearing"     x.bearing     
      do! Json.write "slope"       x.slope       
    }

module AnnotationResults' =
  let current = 0
  //conversion
  let convert (old : AnnotationResults) : AnnotationResults' =
    {
        version     = current
        height      = old.height
        heightDelta = old.heightDelta
        avgAltitude = old.avgAltitude
        length      = old.length
        wayLength   = old.wayLength
        bearing     = old.bearing
        slope       = old.slope
    }

  let initial = 
    {
        version     = current
        height      = Double.NaN
        heightDelta = Double.NaN
        avgAltitude = Double.NaN
        length      = Double.NaN
        wayLength   = Double.NaN
        bearing     = Double.NaN
        slope       = Double.NaN
    }  

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

  results     : Option<AnnotationResults'>
  dnsResults  : Option<DipAndStrikeResults'>

  visible     : bool
  showDns     : bool
  text        : string
  textsize    : NumericInput

  surfaceName : string
  view        : CameraView
}
with 
  static member current = 0
  static member private readV0 =
    json {
      let! key          = Json.read "key"
      let! modelTrafo   = Json.read "modelTrafo" //|> Trafo3d.Parse
      let! geometry     = Json.read "geometry"
      let! projection   = Json.read "projection"
      let! semantic     = Json.read "semantic"

      let! points   = Json.read "points"
      let! segments = Json.read "segments"

      let! color        = Json.readWith Ext.fromJson<ColorInput,Ext> "color"
      let! thickness    = Json.readWith Ext.fromJson<NumericInput,Ext> "thickness"

      let! results      = Json.read "results"
      let! dnsResults   = Json.read "dnsResults"

      let! visible  = Json.read "visible"
      let! showDns  = Json.read "showDns"
      let! text     = Json.read "text"
      let! textSize = Json.readWith Ext.fromJson<NumericInput,Ext> "textsize"

      let! surfaceName = Json.read "surfaceName"

      let! (cameraView : list<string>) = Json.read "view"
      
      let cameraView = cameraView |> List.map V3d.Parse
      let cameraView = CameraView(cameraView.[0],cameraView.[1],cameraView.[2],cameraView.[3], cameraView.[4])

      return {
        version     = Annotation'.current
        key         = key           |> Guid.Parse
        modelTrafo  = modelTrafo    |> Trafo3d.Parse        
        geometry    = geometry      |> enum<Geometry>
        projection  = projection    |> enum<Projection>
        semantic    = semantic      |> enum<Semantic>
        points      = points        |> Serialization.jsonSerializer.UnPickleOfString
        segments    = segments      |> Serialization.jsonSerializer.UnPickleOfString
        color       = color
        thickness   = thickness      
        results     = results    
        dnsResults  = dnsResults         
        visible     = visible 
        showDns     = showDns   
        text        = text      
        textsize    = textSize         
        surfaceName = surfaceName
        view        = cameraView 
      }
    }
  static member FromJson(_:Annotation') = 
    json {
      let! v = Json.read "version"
      match v with 
      | 0 -> return! Annotation'.readV0
      | _ -> return! v |> sprintf "don't know version %A of Annotation" |> Json.error
    }

  static member ToJson (x : Annotation') =
    json {
      do! Json.write "version"    x.version
      do! Json.write "key"        (x.key.ToString())
      do! Json.write "modelTrafo" (x.modelTrafo.ToString())
      do! Json.write "geometry"   (x.geometry |> int)
      do! Json.write "projection" (x.projection |> int)
      do! Json.write "semantic"   (x.semantic |> int)
      do! Json.write "points"     (x.points |> Serialization.jsonSerializer.PickleToString)
      do! Json.write "segments"   (x.segments |> Serialization.jsonSerializer.PickleToString)
      do! Json.writeWith (Ext.toJson<ColorInput,Ext>) "color" x.color
      do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "thickness" x.thickness
      do! Json.write "results"       x.results
      do! Json.write "dnsResults"    x.dnsResults
      do! Json.write "visible"    x.visible
      do! Json.write "showDns"    x.showDns
      do! Json.write "text"    x.text
      do! Json.writeWith (Ext.toJson<NumericInput,Ext>) "textsize" x.textsize
      do! Json.write "surfaceName"    x.surfaceName

      let camView = [x.view.Sky; x.view.Location; x.view.Forward; x.view.Up ; x.view.Right] |> List.map(fun x -> x.ToString())      
      do! Json.write "view" camView
    }

module Annotation' =
  let current = 0
  //conversion
  let convert (old : Annotation) : Annotation' =
   {
      version     = Annotation'.current
      key         = old.key         
      modelTrafo  = old.modelTrafo  
      geometry    = old.geometry    
      projection  = old.projection  
      semantic    = old.semantic    
      points      = old.points      
      segments    = old.segments    
      color       = old.color       
      thickness   = old.thickness   
      results     = old.results    |> Option.map AnnotationResults'.convert
      dnsResults  = old.dnsResults |> Option.map DipAndStrikeResults'.convert
      visible     = old.visible     
      showDns     = old.showDns     
      text        = old.text        
      textsize    = old.textsize    
      surfaceName = old.surfaceName 
      view        = old.view
   }

  let initial : Annotation' =
   {
      version     = Annotation'.current
      key         = Annotation.initial.key
      modelTrafo  = Annotation.initial.modelTrafo
      geometry    = Annotation.initial.geometry    
      projection  = Annotation.initial.projection  
      semantic    = Annotation.initial.semantic    
      points      = Annotation.initial.points      
      segments    = Annotation.initial.segments    
      color       = Annotation.initial.color       
      thickness   = Annotation.initial.thickness   
      results     = Annotation.initial.results    |> Option.map AnnotationResults'.convert
      dnsResults  = Annotation.initial.dnsResults |> Option.map DipAndStrikeResults'.convert
      visible     = Annotation.initial.visible     
      showDns     = Annotation.initial.showDns     
      text        = Annotation.initial.text        
      textsize    = Annotation.initial.textsize    
      surfaceName = Annotation.initial.surfaceName 
      view        = Annotation.initial.view
   }
    

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

module IO = 
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



