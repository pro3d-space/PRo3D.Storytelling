namespace PRo3D.Versioned

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open PRo3D
open Chiron

#nowarn "0686"

[<AutoOpen>]
module ChironPrimitives =      
  type NumericInput
    with 
      static member FromJson(_ : NumericInput) = 
        json {
            let! value  = Json.read "value"
            let! min    = Json.read "min"
            let! max    = Json.read "max"
            let! step   = Json.read "step"
            let! format = Json.read "format"
      
            return {
                value   = value
                min     = min
                max     = max
                step    = step
                format  = format
            }
        }
      static member ToJson (x : NumericInput) =
        json {
            do! Json.write "value"  x.value
            do! Json.write "min"    x.min
            do! Json.write "max"    x.max
            do! Json.write "step"   x.step
            do! Json.write "format" x.format
          }

type Ext = Ext
 
type Ext with
     static member FromJson1 (ext : Ext, v : NumericInput) = 
         json {
             let! value  = Json.read "value"
             let! min    = Json.read "min"
             let! max    = Json.read "max"
             let! step   = Json.read "step"
             let! format = Json.read "format"
     
             return {
                 value   = value
                 min     = min
                 max     = max
                 step    = step
                 format  = format
             }
         }
     static member ToJson1 (ext : Ext, v : NumericInput) = 
       json {
         do! Json.write "value"  v.value
         do! Json.write "min"    v.min
         do! Json.write "max"    v.max
         do! Json.write "step"   v.step
         do! Json.write "format" v.format
       }
       
     static member FromJson1 (ext : Ext, v : int) = 
           json {       
               return 1
           }
module Ext =
  let inline fromJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b or ^e) : (static member FromJson1: ^e * ^a -> ^a Json)(Unchecked.defaultof<_>,a))

  let inline fromJson x =
    fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)

  let inline toJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b or ^e) : (static member ToJson1: ^e * ^a -> unit Json)(Unchecked.defaultof<_>,a))

  let inline toJson (x: 'a) =
    snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))

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



