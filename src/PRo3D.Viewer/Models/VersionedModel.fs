namespace PRo3D.Versioned

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open PRo3D
open Chiron
open Aardvark.UI
open Aardvark.UI.Primitives

#nowarn "0686"

open PRo3D.Base

module Json = 
  let writeFloat name (floatValue : float)  = 
    json {
      if floatValue.IsNaN() then      
        do! Json.writeNone name
      else
        do! Json.write name floatValue
    }

  let readFloat name : Json<float> = 
    json {
      let! v = Json.tryRead name
      return 
        match v with
        | Some k -> k
        | None -> Double.NaN      
    }

  let parsePlane3d (s:string) =
    let x = s.NestedBracketSplitLevelOne() |> Seq.toArray
    let n = x.[0] |> V3d.Parse
    let p = float(x.[1]) //|> Double.Parse
    Plane3d(n, p)
            
  //let writeFloat name (floatValue : float)  = 
  //  json {
  //    if floatValue.IsNaN() then      
  //      do! Json.writeNone name
  //    else
  //      do! Json.write name floatValue
  //  }

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
  
  //conversion
  let convert (old : Statistics) : Statistics' =
    {
      version      = Statistics'.current
      average      = old.average     
      min          = old.min         
      max          = old.max         
      stdev        = old.stdev       
      sumOfSquares = old.sumOfSquares
    }

  //initial
  let initial = 
     {
        version      = Statistics'.current
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
          plane           = plane |> Json.parsePlane3d //plane |> Plane3d.Parse
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
      let! height     = Json.readFloat "height"     
      let! heightDelta= Json.readFloat "heightDelta"
      let! avgAltitude= Json.readFloat "avgAltitude"
      let! length     = Json.readFloat "length"     
      let! wayLength  = Json.readFloat "wayLength"  
      let! bearing    = Json.readFloat "bearing"    
      let! slope      = Json.readFloat "slope"
      
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
      do! Json.write      "version"     x.version     
      do! Json.writeFloat "height"      x.height      
      do! Json.writeFloat "heightDelta" x.heightDelta       
      do! Json.writeFloat "avgAltitude" x.avgAltitude 
      do! Json.writeFloat "length" x.length            
      do! Json.writeFloat "wayLength" x.wayLength       
      do! Json.writeFloat "bearing"     x.bearing     
      do! Json.writeFloat "slope"       x.slope       
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

type SemanticId = SemanticId of string
type SemanticType = Metric = 0 | Angular = 1 | Hierarchical = 2 | Undefined = 3

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

  semanticId   : SemanticId
  semanticType : SemanticType
}
with 
  static member current = 1
  static member private readV0 =
    json {
      let! key         = Json.read "key"
      let! modelTrafo  = Json.read "modelTrafo" //|> Trafo3d.Parse
      let! geometry    = Json.read "geometry"
      let! projection  = Json.read "projection"
      let! semantic    = Json.read "semantic"

      let! points      = Json.read "points"
      let! segments    = Json.read "segments"

      let! color       = Json.readWith Ext.fromJson<ColorInput,Ext> "color"
      let! thickness   = Json.readWith Ext.fromJson<NumericInput,Ext> "thickness"

      let! results     = Json.read "results"
      let! dnsResults  = Json.read "dnsResults"

      let! visible     = Json.read "visible"
      let! showDns     = Json.read "showDns"
      let! text        = Json.read "text"
      let! textSize    = Json.readWith Ext.fromJson<NumericInput,Ext> "textsize"

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
        semanticId   = SemanticId ""
        semanticType = SemanticType.Undefined
      }
    }
  static member private readV1 =
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

      let! semanticId    = Json.read "semanticId"
      let! semanticType  = Json.read "semanticType"

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
        semanticId   = semanticId |> SemanticId
        semanticType = semanticType |> enum<SemanticType>
      }
    }
  static member FromJson(_:Annotation') = 
    json {
      let! v = Json.read "version"
      match v with 
      | 0 -> return! Annotation'.readV0
      | 1 -> return! Annotation'.readV1
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
      do! Json.write "semanticId" (x.semanticId.ToString())
      do! Json.write "semanticType" (x.semanticType |> int)
      
    }

module Annotation' =
  
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
      semanticId   = SemanticId ""
      semanticType = SemanticType.Undefined
   }

  let thickn = {
      value   = 3.0
      min     = 1.0
      max     = 8.0
      step    = 1.0
      format  = "{0:0}"
  }

  let texts = {
      value   = 0.05
      min     = 0.01
      max     = 5.0
      step    = 0.01
      format  = "{0:0.00}"
  }

  let make_old (projection) (geometry) (color) (thickness) (surfName) : Annotation  =       
    {
      key         = Guid.NewGuid()
      geometry    = geometry
      semantic    = Semantic.None
      points      = plist.Empty
      segments    = plist.Empty //[]
      color       = color
      thickness   = thickness
      results     = None
      dnsResults  = None            
      projection  = projection
      visible     = true
      text        = ""
      textsize    = texts
      modelTrafo  = Trafo3d.Identity
      showDns     = 
          match geometry with 
            | Geometry.DnS -> true 
            | _ -> false 
      surfaceName = surfName
      view = FreeFlyController.initial.view
    }

  let initial_old =
      make_old Projection.Viewpoint Geometry.Polyline { c = C4b.Magenta } thickn ""

  let initial : Annotation' =
   {
      version     = Annotation'.current
      key         = initial_old.key
      modelTrafo  = initial_old.modelTrafo
      geometry    = initial_old.geometry    
      projection  = initial_old.projection  
      semantic    = initial_old.semantic    
      points      = initial_old.points      
      segments    = initial_old.segments    
      color       = initial_old.color       
      thickness   = initial_old.thickness   
      results     = initial_old.results    |> Option.map AnnotationResults'.convert
      dnsResults  = initial_old.dnsResults |> Option.map DipAndStrikeResults'.convert
      visible     = initial_old.visible     
      showDns     = initial_old.showDns     
      text        = initial_old.text        
      textsize    = initial_old.textsize    
      surfaceName = initial_old.surfaceName 
      view        = initial_old.view
      semanticId   = SemanticId ""
      semanticType = SemanticType.Undefined
   }
    
  let thickness = [1.0; 2.0; 3.0; 4.0; 5.0; 1.0; 1.0]
  let color = [new C4b(241,238,246); new C4b(189,201,225); new C4b(116,169,207); new C4b(43,140,190); new C4b(4,90,141); new C4b(241,163,64); new C4b(153,142,195) ]

  let make (projection) (geometry) (color) (thickness) (surfName) : Annotation'  =       
      {
        version     = Annotation'.current
        key         = Guid.NewGuid()
        geometry    = geometry
        semantic    = Semantic.None
        points      = plist.Empty
        segments    = plist.Empty //[]
        color       = color
        thickness   = thickness
        results     = None
        dnsResults  = None            
        projection  = projection
        visible     = true
        text        = ""
        textsize    = texts
        modelTrafo  = Trafo3d.Identity
        showDns     = 
            match geometry with 
              | Geometry.DnS -> true 
              | _ -> false 
        surfaceName = surfName
        view = FreeFlyController.initial.view
        semanticId   = SemanticId ""
        semanticType = SemanticType.Undefined
      }

  //initial
  //version modules







