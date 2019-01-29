namespace PRo3D.Minerva

open Aardvark.Base
open FSharp.Data
open FSharp.Data.JsonExtensions

module MinervaGeoJSON =
  open FSharp.Data.Runtime
  open PRo3D.Base

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

  let parseProperties (ins : Instrument) (properties : JsonValue) : Properties = 
    let id = (properties?id).AsString()                
    match ins with
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

  let parseSingleCoord (c : array<float>) : V3d =
    if c.Length <> 3 then failwith "invalid coordinate of size other than 3"
    V3d(c.[1],c.[0],c.[2])

  let parseSingleCoordXYZ (c : array<float>) : V3d =
    if c.Length <> 3 then failwith "invalid coordinate of size other than 3"
    V3d(c.[0],c.[1],c.[2])

  let parseCoordinates typus (coordinates : JsonValue) = 
    match typus with
    | Typus.Point -> coordinates.AsArray() |> Array.map(fun x -> x.AsFloat()) |> parseSingleCoord
    | _ -> typus |> sprintf "typus %A not implemented" |> failwith

  let parseGeometry (geometry : JsonValue) : PRo3D.Minerva.Geometry = 
    let typus = geometry.GetProperty("type") |> parseTypus
    let coords = geometry?coordinates |> parseCoordinates typus |> List.singleton
    
    {
      typus       = typus
      coordinates = coords
      positions   = List.empty
    }        

  let parseFeature (ins : Instrument) (feature : JsonValue) : Feature =
    let prop = feature?properties |> parseProperties ins
    
    let position = feature?properties?position.AsArray() |> Array.map(fun x -> x.AsFloat()) |> parseSingleCoordXYZ
    let geometry = feature?geometry |> parseGeometry    

    let sol = (feature?properties?planet_day_number).AsInteger()

    {
      id          = (feature?id).AsString()
      instrument  = ins
      typus       = feature.GetProperty("type") |> parseTypus
      boundingBox = Box2d.Invalid//feature?bbox |> parseBoundingBox
      properties  = prop
      geometry    = { geometry with positions = position |> List.singleton }
      sol         = sol
    } 

  let parseFeatures (ins : Instrument) (features : JsonValue) : list<Feature> =  
    features.AsArray() |> List.ofArray |> List.map (parseFeature ins)
    
  let parseRoot(root : JsonValue) : FeatureCollection =
    let instrument = (root?id).AsString() |> Model.toInstrument

    {
      name        = (root?id).AsString()
      typus       = root.GetProperty("type") |> parseTypus    
      boundingBox = (root?bbox) |> parseBoundingBox
      features    = (root?features) |> parseFeatures instrument |> PList.ofList
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

  let parseRootProperties (props : JsonValue) =         
    {
      totalCount   = (props?totalCount).AsInteger()
      startIndex   = (props?startIndex).AsInteger()
      itemsPerPage = (props?itemsPerPage).AsInteger()
      published    = (props?published).AsDateTime()
    }

  let loadFromUrls sites = 
    sites 
      |> List.map JsonValue.AsyncLoad
      |> Async.Parallel
      |> Async.RunSynchronously 
      |> Array.map parseRoot
      |> combineFeatureCollections

  let loadPaged (site:string) = 
    let bla = JsonValue.Load site
    let probs = bla?properties |> parseRootProperties
    let pages =
      [0 .. probs.itemsPerPage .. probs.totalCount]
        |> List.map(fun index -> sprintf "%s?startIndex=%d&count=%d" site index probs.itemsPerPage)

    pages |> loadFromUrls
    

  let loadMultiple (sites : list<string>) : FeatureCollection =     
    Log.startTimed "fetching data from sites"
    let result = 
      sites |> List.map(loadPaged) |> combineFeatureCollections
    //let result = 
    //  sites |> Load
    Log.stop()
    result