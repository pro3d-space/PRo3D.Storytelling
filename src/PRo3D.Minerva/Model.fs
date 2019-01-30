namespace PRo3D.Minerva

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open FSharp.Data
open FSharp.Data.JsonExtensions
open Aardvark.Application

type FeatureId = FeatureId of string

type Typus = 
  | FeatureCollection
  | Feature
  | Polygon
  | Point

type MAHLI_Properties =
  {
    id        : FeatureId
    beginTime : DateTime
    endTime   : DateTime
  }

type FrontHazcam_Properties =
  {
    id        : FeatureId
    beginTime : DateTime
    endTime   : DateTime
  }

type Mastcam_Properties =
  {
    id        : FeatureId
    beginTime : DateTime
    endTime   : DateTime
  }

type APXS_Properties =
  {
    id        : FeatureId
  }

type Properties =
  | MAHLI       of MAHLI_Properties
  | FrontHazcam of FrontHazcam_Properties
  | Mastcam     of Mastcam_Properties
  | APXS        of APXS_Properties
  member this.id =
    match this with
    | MAHLI       k -> k.id
    | FrontHazcam k -> k.id
    | Mastcam     k -> k.id
    | APXS        k -> k.id

type Instrument = 
  | MAHLI       = 0
  | FrontHazcam = 1
  | Mastcam     = 2
  | APXS        = 3

type Geometry = 
  {
    typus       : Typus
    coordinates : list<V3d>
    positions   : list<V3d>
  }

type Feature =
  { 
    id          : string
    instrument  : Instrument
    typus       : Typus
    properties  : Properties
    boundingBox : Box2d
    geometry    : Geometry
    sol         : int
  }

type RootProperties = 
  {
    totalCount   : int
    startIndex   : int 
    itemsPerPage : int    
    published    : DateTime
  }

[<DomainType>]
type FeatureCollection = 
  {
    name : string
    typus       : Typus
    boundingBox : Box2d    
    features    : plist<Feature>
  }

[<DomainType>]
type Model = 
  {
    data : FeatureCollection
    queries  : list<string>
  }

[<StructuredFormatDisplay("{AsString}"); Struct>]
type Len(meter : float) =
  member x.Angstrom       = meter * 10000000000.0
  member x.Nanometer      = meter * 1000000000.0
  member x.Micrometer     = meter * 1000000.0
  member x.Millimeter     = meter * 1000.0
  member x.Centimeter     = meter * 100.0
  member x.Meter          = meter
  member x.Kilometer      = meter / 1000.0
  member x.Astronomic     = meter / 149597870700.0
  member x.Lightyear      = meter / 9460730472580800.0
  member x.Parsec         = meter / 30856775777948584.0

  member private x.AsString = x.ToString()

  override x.ToString() =
      if x.Parsec > 0.5 then sprintf "%.3fpc" x.Parsec
      elif x.Lightyear > 0.5 then sprintf "%.3fly" x.Lightyear
      elif x.Astronomic > 0.5 then sprintf "%.3fau" x.Astronomic
      elif x.Kilometer > 0.5 then sprintf "%.3fkm" x.Kilometer
      elif x.Meter > 1.0 then sprintf "%.2fm" x.Meter
      elif x.Centimeter > 1.0 then sprintf "%.2fcm" x.Centimeter
      elif x.Millimeter > 1.0 then sprintf "%.0fmm" x.Millimeter
      elif x.Micrometer > 1.0 then sprintf "%.0fµm" x.Micrometer
      elif x.Nanometer > 1.0 then sprintf "%.0fµm" x.Nanometer
      elif meter > 0.0 then sprintf "%.0f" x.Angstrom
      else "0"

module Model = 
  let toInstrument (id : string) =
 //   let instr = id.ToCharArray() |> Array.takeWhile(fun x -> x <> '_')
   // let instr = new string(instr)
    match id.ToLowerInvariant() with
      | "mahli"       -> Instrument.MAHLI
      | "fronthazcam" -> Instrument.FrontHazcam        
      | "mastcam"     -> Instrument.Mastcam
      | "apxs"        -> Instrument.APXS
      | _ -> id |> sprintf "unknown instrument %A" |> failwith

  let instrumentColor (instr : Instrument) =
    match instr with 
    | Instrument.MAHLI -> C4b(27,158,119)
    | Instrument.FrontHazcam -> C4b(217,95,2)
    | Instrument.Mastcam -> C4b(231,41,138)
    | Instrument.APXS -> C4b(230,171,2)
    | _ -> failwith "unknown instrument"

module Initial = 
  let data = 
    { 
      name = "initial"
      boundingBox = Box2d.Invalid
      typus       = Typus.Feature
      features    = PList.empty
    }

          
  let sites = [
    @"https://minerva.eox.at/opensearch/collections/MAHLI/json/"
    @"https://minerva.eox.at/opensearch/collections/FrontHazcam/json/"
    @"https://minerva.eox.at/opensearch/collections/Mastcam/json/"    
    @"https://minerva.eox.at/opensearch/collections/APXS/json/"
  ]

  let model = 
    {
      data = data
      queries = sites
    }
