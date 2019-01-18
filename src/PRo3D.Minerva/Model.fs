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
    @"http://minerva1.eox.at:8600/opensearch/collections/MAHLI/json/"
    @"http://minerva1.eox.at:8600/opensearch/collections/FrontHazcam/json/"
    @"http://minerva1.eox.at:8600/opensearch/collections/Mastcam/json/"
    @"http://minerva1.eox.at:8600/opensearch/collections/APXS/json/"
  ]

  let model = 
    {
      data = data
      queries = sites
    }
