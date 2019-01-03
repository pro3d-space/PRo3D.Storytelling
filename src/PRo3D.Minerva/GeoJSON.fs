namespace PRo3D.Minerva

open Aardvark.Base
open FSharp.Data
open FSharp.Data.JsonExtensions

module MinervaGeoJSON =
  open FSharp.Data.Runtime

  let shout s =
    Log.line "%A" s  

  let parseBoundingBox (bbox : JsonValue) : Box2d =
    let bbox = bbox.AsArray()
    
    if bbox.Length <> 4 then failwith "invalid bounding box of size other than 4"
    
    let minLat = bbox.[0].AsFloat()
    let minLon = bbox.[1].AsFloat()
    let maxLat = bbox.[2].AsFloat()
    let maxLon = bbox.[3].AsFloat()
    
    Box2d(minLon, minLat, maxLon, maxLat)

  let parseTypus (typus : JsonValue) : Typus =
    match typus.AsString().ToLowerInvariant() with
      | "featurecollection" -> Typus.FeatureCollection
      | "feature"           -> Typus.Feature
      | "polygon"           -> Typus.Polygon
      | "point"             -> Typus.Point
      | s -> s |> sprintf "[parseTypus] string %A unknown" |> failwith

  let parseProperties (properties : JsonValue) : Properties = 
    let id = (properties?id).AsString()                
    match id |> Model.toInstrument with
        | Instrument.MAHLI ->          
          {
            MAHLI_Properties.id = id |> FeatureId
            beginTime = (properties?begin_time).AsDateTime()
            endTime = (properties?end_time).AsDateTime()
          } |> Properties.MAHLI
        | Instrument.FrontHazcam ->
          {
            FrontHazcam_Properties.id = id |> FeatureId
            beginTime = (properties?begin_time).AsDateTime()
            endTime = (properties?end_time).AsDateTime()
          } |> Properties.FrontHazcam
        | Instrument.Mastcam ->
          {
            Mastcam_Properties.id = id |> FeatureId
            beginTime = (properties?begin_time).AsDateTime()
            endTime = (properties?end_time).AsDateTime()
          } |> Properties.Mastcam
        | Instrument.APXS ->
          {
            APXS_Properties.id = id |> FeatureId
          } |> Properties.APXS       
        | _ -> failwith "encountered invalid instrument from parsing"

  let parseSingleCoord (c : array<float>) : V2d =
    if c.Length <> 2 then failwith "invalid coordinate of size other than 2"
    V2d(c.[1],c.[0])

  let parseCoordinates typus (coordinates : JsonValue) = 
    match typus with
    | Typus.Point -> coordinates.AsArray() |> Array.map(fun x -> x.AsFloat()) |> parseSingleCoord
    | _ -> typus |> sprintf "typus %A not implemented" |> failwith

  let parseGeometry (geometry : JsonValue) : PRo3D.Minerva.Geometry = 
    let typus = geometry.GetProperty("type") |> parseTypus
    {
      typus       = typus
      coordinates = geometry?coordinates |> parseCoordinates typus |> List.singleton
    }        

  let parseFeature (feature : JsonValue) : Feature =
    let prop = feature?properties |> parseProperties
    let (FeatureId s) = prop.id

    {
      id          = s
      instrument  = s |> Model.toInstrument
      typus       = feature.GetProperty("type") |> parseTypus
      boundingBox = feature?bbox |> parseBoundingBox
      properties  = prop
      geometry    = feature?geometry |> parseGeometry
    } 

  let parseFeatures (features : JsonValue) : list<Feature> =  
    features.AsArray() |> List.ofArray |> List.map parseFeature
        
  let parseRoot(root : JsonValue) : FeatureCollection =
    {
      name        = (root?name).AsString()
      typus       = root.GetProperty("type") |> parseTypus    
      boundingBox = (root?bbox) |> parseBoundingBox
      features    = (root?features) |> parseFeatures |> PList.ofList
    }

  let load (siteUrl:string) : FeatureCollection =     
     JsonValue.Load(siteUrl) |> parseRoot   
     
  let combineFeatureCollections (collections : seq<FeatureCollection>) : FeatureCollection =
    {
      name = "layers"
      typus = FeatureCollection
      boundingBox = collections |> Seq.fold (fun (a:Box2d) (fc:FeatureCollection) -> a.ExtendedBy(fc.boundingBox)) Box2d.Invalid
      features = collections |> Seq.map(fun x -> x.features) |> PList.concat
    }

  let loadMultiple sites : FeatureCollection =     
    Log.startTimed "fetching data from sites"
    let result = 
      sites 
        |> List.map JsonValue.AsyncLoad
        |> Async.Parallel
        |> Async.RunSynchronously 
        |> Array.map parseRoot
        |> combineFeatureCollections
    Log.stop()
    result