namespace PRo3D.Minerva

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open FSharp.Data
open FSharp.Data.JsonExtensions
open Aardvark.Application
open PRo3D.Minerva.Communication

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

type ChemCam_Properties =
  {
    id        : FeatureId    
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
  | ChemCam     of ChemCam_Properties
  member this.id =
    match this with
    | MAHLI       k -> k.id
    | FrontHazcam k -> k.id
    | Mastcam     k -> k.id
    | APXS        k -> k.id
    | ChemCam     k -> k.id

type Instrument = 
  | MAHLI          =  0
  | FrontHazcam    =  1
  | Mastcam        =  2
  | APXS           =  3
  | FrontHazcamR   =  4
  | FrontHazcamL   =  5
  | MastcamR       =  6
  | MastcamL       =  7
  | ChemLib        =  8
  | ChemRmi        =  9
  | NotImplemented = 10

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

type MinervaAction =
  | PerformQueries
  | SaveFeatureCollection
  | LoadFeatureCollection
  | Reset
  | WriteScreenCoordsDebug
  | PerformIdQuery of list<string>
  | ConnectVisplore
  | FlyToProduct of V3d
  | SetMinSol of Numeric.Action
  | SetMaxSol of Numeric.Action
  | SetDistance of Numeric.Action
  | CheckMAHLI
  | CheckFrontHazcam
  | CheckMastcam
  | CheckAPXS
  | CheckFrontHazcamR
  | CheckFrontHazcamL
  | CheckMastcamR
  | CheckMastcamL
  | CheckChemLib
  | CheckChemRmi
      

[<DomainType>]
type QueryFilter = {
    minSol               : NumericInput
    maxSol               : NumericInput
    instrument           : Instrument
    distance             : NumericInput
    distFilteredFeatures : plist<Feature>
    checkMAHLI           : bool
    checkFrontHazcam     : bool
    checkMastcam         : bool
    checkAPXS            : bool
    checkFrontHazcamR    : bool
    checkFrontHazcamL    : bool
    checkMastcamR        : bool
    checkMastcamL        : bool
    checkChemLib         : bool
    checkChemRmi         : bool
}

type MessagingMailbox = MailboxProcessor<MailboxAction>

and MailboxState = {
  events  : list<MailboxAction>
  update  : seq<MinervaAction> -> unit
}
and MailboxAction =
  | MiniAction  of MinervaAction
  | InitMailboxState of MailboxState  

module MailboxState = 
  let empty = 
    {
      events = list.Empty
      update = fun _ -> ()
    }
  
[<DomainType>]
type Model = 
  {
    data     : FeatureCollection
    queries  : list<string>

    [<NonIncremental>]
    comm     : option<Communicator.Communicator>

    vplMessages : ThreadPool<MinervaAction>

    minervaMessagingMailbox : MessagingMailbox
    minervaMailboxState     : MailboxState
    queryFilter             : QueryFilter
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
      | "mahli"        -> Instrument.MAHLI
      | "apxs"         -> Instrument.APXS
      | "fhaz_left_b"  -> Instrument.FrontHazcamL    
      | "fhaz_right_b" -> Instrument.FrontHazcamR    
      | "mast_left"    -> Instrument.MastcamL
      | "mast_right"   -> Instrument.MastcamR
      | "chemcam_libs" -> Instrument.ChemLib
      | "chemcam_rmi"  -> Instrument.ChemRmi      
      | _ -> id |> sprintf "unknown instrument %A" |> failwith

  let instrumentColor (instr : Instrument) =
    match instr with 
    | Instrument.MAHLI -> C4b(27,158,119)
    | Instrument.FrontHazcam -> C4b(255,255,255)
    | Instrument.Mastcam -> C4b(255,255,255)
    | Instrument.APXS -> C4b(230,171,2)
    | Instrument.FrontHazcamR -> C4b(31,120,180)
    | Instrument.FrontHazcamL -> C4b(166,206,227)
    | Instrument.MastcamR -> C4b(227,26,28)
    | Instrument.MastcamL -> C4b(251,154,153)
    | Instrument.ChemRmi  -> C4b(173,221,142)
    | Instrument.ChemLib  -> C4b(49,163,84)
    | Instrument.NotImplemented -> C4b(0,0,0)
    | _ -> failwith "unknown instrument"

module Initial = 
  let data = 
    { 
      name = "initial"
      boundingBox = Box2d.Invalid
      typus       = Typus.Feature
      features    = PList.empty
    }
  let minS = 
        {
            value = 751.0
            min =  0.0
            max = 10000.0
            step = 1.0
            format = "{0:0}"
        }
  let maxS = 
        {
            value = 926.0
            min =  0.0
            max = 10000.0
            step = 1.0
            format = "{0:0}"
        }
  let dist = 
        {
            value = 10000000.0
            min =  0.0
            max = 10000000.0
            step = 100.0
            format = "{0:0}"
        }
  let queryFilter = 
    {
        minSol                = minS
        maxSol                = maxS
        instrument            = Instrument.NotImplemented
        distance              = dist
        distFilteredFeatures  = PList.empty
        checkMAHLI            = true
        checkFrontHazcam      = true
        checkMastcam          = true
        checkAPXS             = true
        checkFrontHazcamR     = true
        checkFrontHazcamL     = true
        checkMastcamR         = true
        checkMastcamL         = true
        checkChemLib          = true
        checkChemRmi          = true
    }
          
  let sites = [
    //@"https://minerva.eox.at/opensearch/collections/MAHLI/json/"
    //@"https://minerva.eox.at/opensearch/collections/FrontHazcam-Right/json/"
    //@"https://minerva.eox.at/opensearch/collections/FrontHazcam-Left/json/"
    //@"https://minerva.eox.at/opensearch/collections/Mastcam-Right/json/"    
    //@"https://minerva.eox.at/opensearch/collections/Mastcam-Left/json/"
    //@"https://minerva.eox.at/opensearch/collections/APXS/json/"
    @"https://minerva.eox.at/opensearch/collections/all/json/"
  ]

  let model mailbox = 
    {
      data = data
      queries = sites
      comm = None
      vplMessages = ThreadPool.Empty

      minervaMailboxState = MailboxState.empty
      minervaMessagingMailbox = mailbox
      queryFilter = queryFilter
    }
