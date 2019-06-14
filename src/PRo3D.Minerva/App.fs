namespace PRo3D.Minerva

//open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Events

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 

open PRo3D.Base
open PRo3D.Minerva.Communication
open Aardvark.GeoSpatial.Opc
open System
open Aardvark.UI

[<AutoOpen>]
module MissingInBase = 
    type ProcListBuilder with   
        member x.While(predicate : unit -> bool, body : ProcList<'m,unit>) : ProcList<'m,unit> =
            proclist {
                let p = predicate()
                if p then 
                    yield! body
                    yield! x.While(predicate,body)
                else ()
            }

type HarriSchirchWrongBlockingCollection<'a>() =
    let sema = new System.Threading.SemaphoreSlim(0)
    let l = obj()
    let queue = System.Collections.Generic.Queue<'a>()
    let mutable finished = false

    member x.TakeAsync() =
        async {
            do! sema.WaitAsync() |> Async.AwaitTask
            if finished then return None
            else
                return 
                    lock l (fun _ -> 
                        queue.Dequeue() |> Some
                    )
        }

    member x.Enqueue(v) =
        lock l (fun _ -> 
            queue.Enqueue(v)
        )
        sema.Release() |> ignore

    member x.CompleteAdding() =
        finished <- true
        sema.Release()

    member x.IsCompleted = finished
        

module App =
  open System.Net.Sockets
  open Aardvark.SceneGraph.Opc
  open System.Threading  

  //coords  -4831.341805

  let computeXYZFeatures (features : FeatureCollection) : FeatureCollection =
    let a = 
      features.features 
        |> PList.map(fun feature ->
          let pos =
            feature.geometry.coordinates 
              |> List.map(fun c -> 
                    let sc = { longitude = 360.0 - c.Y; latitude = c.X; altitude = -4831.341805; radian = 0.0 }
                    CooTransformation.getXYZFromLatLonAlt sc Planet.Mars)
          { feature with geometry = { feature.geometry with positions = pos }})

    { features with features = a }

  let instrumentText (instr : Instrument) =
    match instr with
    | Instrument.MAHLI        -> "MAHLI"
    | Instrument.FrontHazcam  -> "FrontHazcam"
    | Instrument.Mastcam      -> "Mastcam"
    | Instrument.APXS         -> "APXS"
    | Instrument.FrontHazcamR -> "FrontHazcamR"
    | Instrument.FrontHazcamL -> "FrontHazcamL"
    | Instrument.MastcamR     -> "MastcamR"    
    | Instrument.MastcamL     -> "MastcamL"
    | Instrument.ChemLib      -> "ChemLib"    
    | Instrument.ChemRmi      -> "ChemRmi"
    | Instrument.NotImplemented -> "not impl. yet"
    | _ -> instr |> sprintf "unknown instrument identifier %A" |> failwith

  let writeLinesToFile path (contents : list<string>) =
        System.IO.File.WriteAllLines(path, contents)

  let sendMessage2Visplore (address) (port) (message : string) =
    let client = new TcpClient(address, port);
    let data = System.Text.Encoding.ASCII.GetBytes(message)
    let stream = client.GetStream()
    stream.Write(data, 0, data.Length)
    
    printfn "Sending message: %A" message
    
    //let bytes = stream.Read(data, 0, data.Length);
    //let responseData = System.Text.Encoding.ASCII.GetString(data, 0, bytes);
    
    //printfn "Receiving response: %A" responseData
    
    stream.Close()
    client.Close()

  //https://minerva.eox.at/opensearch/collections/all/json/?cql=(identifier%20IN%20(%271092ML0047930090500286E01_DRCX%27))
  //271092ML0047930090500286E01_DRCX
  //1092MR0047900090600628I01_DRCX
  //https://minerva.eox.at/opensearch/collections/all/json/?cql=(identifier%20IN%20(%271092ML0047930090500286E01_DRCX%27,%271092MR0047900090600628I01_DRCX%27))

  let idTestList = [ "271092ML0047930090500286E01_DRCX"; "1092MR0047900090600628I01_DRCX"]
   
   
  let getInstrumentQuery (filter:QueryModel) =
    let mutable cqlStringInst = []
    if filter.checkMAHLI then cqlStringInst <- (cqlStringInst @ ["MAHLI"])
    if filter.checkAPXS then cqlStringInst <- (cqlStringInst @ ["APXS"])
    if filter.checkFrontHazcamR then cqlStringInst <- (cqlStringInst @ ["FHAZ_RIGHT_B"])
    if filter.checkFrontHazcamL then cqlStringInst <- (cqlStringInst @ ["FHAZ_LEFT_B"])
    if filter.checkMastcamR then cqlStringInst <- (cqlStringInst @ ["MAST_RIGHT"])
    if filter.checkMastcamL then cqlStringInst <- (cqlStringInst @ ["MAST_LEFT"])
    if filter.checkChemLib then cqlStringInst <- (cqlStringInst @ ["CHEMCAM_LIBS"])
    if filter.checkChemRmi then cqlStringInst <- (cqlStringInst @ ["CHEMCAM_RMI"])
    let groupString = cqlStringInst |> String.concat("','")
    sprintf @"(instrumentId IN ('" + groupString + "'))"

    

  let getIdQuerySite (filter:QueryModel) (ids:list<string>)  = 
    let groupString = ids |> String.concat("','")
    //let cql1 = [("cql", @"(identifier IN ('" + groupString + "'))")] |> HMap.ofList
    let cqlStringSol = sprintf @"(planetDayNumber >= %f AND planetDayNumber <= %f)"  filter.minSol.value filter.maxSol.value
    let cqlStringInst = getInstrumentQuery filter
    let cqlStringAll = sprintf @"(" + cqlStringSol + " AND " + cqlStringInst + ")"
    let cql = [("cql", cqlStringSol)] |> HMap.ofList
    (@"https://minerva.eox.at/opensearch/collections/all/json/", cql)

  
  let loadFeatureCollection (model: Model) =
    Log.startTimed "[Minerva] loading products"
    let data = PRo3D.Base.Serialization.loadAs ".\data.minerva"
    Log.stop()
    //Log.line "[Minerva] found %d entries" model.data.features.Count
    //{ model with data = data }
    data
  
  let sendReplaceSelectionRequest(comm : Communicator.Communicator) = 
    /// Minerva Example --> Replace Selection

    let IdList = [|"APB_494375951RWP10910491876_______P1"; "APB_493587808RWP10820491216_______P1"; "APB_494212310RWP10900491876_______P1"|]
    
    let name : string = "MinervaReplaceSelection"
    let para : JsonNetworkCommand.Params = {names = [|"IdList"|]; numOfLists = 1; numOfEntries = (IdList |> Array.length); image = None; imageResolution = None; messageID = "SelectionMessageID"}
    let data : JsonNetworkCommand.Data = {ids = Some(IdList); positions = None; colors = None}

    let jsonMessage = JsonNetworkCommand.createJsonNetworkCommand {name = name; para = para; data = data}
    
    comm.GetClient().SendMessageWaitForResponse(jsonMessage)
    

  let update (view:CameraView) (model : Model) (msg : MinervaAction) : Model =
      match msg with
      | Reset ->
        let m' = { model with data = Initial.data }
        { m' with queryFilter = Initial.queryFilter }
      | WriteScreenCoordsDebug -> 
        match model.comm with
        | Some c -> sendReplaceSelectionRequest c
        | None   -> ()
        model
      | PerformQueries -> 
        try
          //let data = model.queries |> MinervaGeoJSON.loadMultiple        
          Log.startTimed "[Minerva] Fetching full dataset from Server"
          let data = idTestList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
          //let features = data.features |> PList.sortBy(fun x -> x.sol)    
          Log.stop()

          let queryM = QueryApp.updateFeaturesForRendering model.queryFilter data.features
          { model with data = data; queryFilter = queryM }

        with e ->
          Log.error "%A" e.Message
          model
      | PerformIdQuery idList ->
        Log.startTimed "fetching %d entries by ids" idList.Length
        let data = idList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
        Log.stop()

        { model with data = data }
      | SaveFeatureCollection -> 
        Log.startTimed "[Minerva] saving %d products" model.data.features.Count
        PRo3D.Base.Serialization.save ".\data.minerva" model.data |> ignore
        Log.stop()
        model
      | LoadFeatureCollection -> 
        //Log.startTimed "[Minerva] loading products"
        //let data = PRo3D.Base.Serialization.loadAs ".\data.minerva"
        //Log.stop()
        //Log.line "[Minerva] found %d entries" model.data.features.Count
        let data = loadFeatureCollection model
        let queryM = QueryApp.applyFilterQueries data.features model.queryFilter view
        Log.line "[Minerva] found %A features in collection" queryM.filteredFeatures.Count
        { model with data = data; queryFilter = queryM}
        //{ model with data = data }
      | ConnectVisplore -> 
        if model.vplMessages.store.IsEmpty then          
          Log.line "[Minerva] connecting to visplore"
          let comm = if model.comm.IsSome then model.comm.Value else new Communicator.Communicator()
          let createWorker () =
             proclist {
               let bc = new HarriSchirchWrongBlockingCollection<_>()               
               comm.Start(fun cmd ->
                 match cmd.name with
                 | "MinervaReplaceSelection" ->
                    Log.line "[Minerva] received vpl message"
                    // Visplore sends Pro3D the selected ids --> do something with them (here: print them)
                    // cmd |> PRo3D.Minerva.Communication.JsonNetworkCommand.printCommand 
                    match cmd.data.ids with 
                    | Some ids -> 
                        let action = ids |> Array.toList |> PerformIdQuery 
                        bc.Enqueue action
                        ///|> MailboxAction.MiniAction |> model.minervaMessagingMailbox.Post
                    | None -> ()
                    
                    cmd.para.messageID
                 | "Shutdown" -> 
                    //bc.CompleteAdding()
                    "todo"
                 | _ -> sprintf "dont know %A" cmd               
               )
               while not bc.IsCompleted do
                 let! action = bc.TakeAsync()
                 Log.line "[Minerva] take async"
                 match action with
                    | Some a -> yield a
                    | None -> ()
               yield FlyToProduct V3d.Zero              
             }          
          { model with vplMessages = ThreadPool.add "vpl server" (createWorker()) model.vplMessages; comm = Some comm}
        else
          model
      | FlyToProduct _ -> model //handled in higher level app
      | QueryMessage msg -> 
        let queryM = QueryApp.applyFilterQueries model.data.features model.queryFilter view
        { model with queryFilter = queryM}
      | QueryDataFile ->
        let cachepath = ".\data.minerva"
        let data = 
            match System.IO.File.Exists cachepath with
             | true -> loadFeatureCollection model
             | false ->
                Log.startTimed "[Minerva] Fetching full dataset from data file"
                let data = Files.loadDataFile
                Log.stop()
                data
        Log.line "[Minerva] found %d entries" model.data.features.Count
        let queryM = QueryApp.applyFilterQueries data.features model.queryFilter view
        Log.line "[Minerva] found %A features in collection" queryM.filteredFeatures.Count
        { model with data = data; queryFilter = queryM}

      | SetPointSize s ->
        let size = Numeric.update model.featureProperties.pointSize s
        { model with featureProperties = { model.featureProperties with pointSize = size }}
      | SetTextSize s ->
        let size = Numeric.update model.featureProperties.textSize s
        { model with featureProperties = { model.featureProperties with textSize = size }}


  let viewFeatures (instr : Instrument) (features : list<Feature>) = 
    [
      for f in features do                                            
        let iconAttr = 
          [
            clazz "ui map pin inverted middle aligned icon"; 
            style (sprintf "color: %s" (f.instrument |> Model.instrumentColor |> Html.ofC4b))
            onClick(fun _ -> FlyToProduct f.geometry.positions.Head)
          ]            
        let headerAttr : AttributeMap<_> = [clazz "ui header small"] |> AttributeMap.ofList
                        
        let item =             
          div [clazz "ui inverted item"][
            i iconAttr []
            div [clazz "ui content"] [
              Incremental.div (headerAttr) (AList.single (instr |> instrumentText |> text))
              div [clazz "ui description"] [
                f.sol |> sprintf "Sol: %A" |> text                               
              ] //[text (id.Replace('_',' '))]
            ]            
          ]
        yield item 
    ]

  let accordion text' icon active content' =
     let title = if active then "title active inverted" else "title inverted"
     let content = if active then "content active" else "content"     
                             
     onBoot "$('#__ID__').accordion();" (
         div [clazz "ui inverted segment"] [
             div [clazz "ui inverted accordion fluid"] [
                 div [clazz title; style "background-color: #282828"] [
                         i [clazz ("dropdown icon")][]
                         text text'                                
                         div[style "float:right"][i [clazz (icon + " icon")][]]
                         
                 ]
                 div [clazz content;  style "overflow-y : auto; "] content' //max-height: 35%
             ]
         ]
     )  
  

  let viewFeaturesGui (model :MModel) =
     let queryGui (model:MModel) = 
          QueryApp.viewQueryFilters model.queryFilter
            |> UI.map QueryMessage 
     
     let propertiesGui =
        require Html.semui ( 
            Html.table [ 
                Html.row "point size:" [Numeric.view' [NumericInputType.Slider] model.featureProperties.pointSize |> UI.map (fun x -> SetPointSize x)] 
                Html.row "text size:" [Numeric.view' [NumericInputType.Slider] model.featureProperties.textSize |> UI.map (fun x -> SetTextSize x)] 
            ]        
        ) 

     let take' (n : int) (input : list<'a>) : list<'a> =
       if n >= input.Length then input else input |> List.take n
       
     let listOfFeatures =
        alist { 
          
          let! features = model.queryFilter.filteredFeatures |> AList.toMod
          let groupedFeatures = features |> PList.toList |> List.groupBy(fun x -> x.instrument)
          let! pos = model.queryFilter.filterLocation

          for (instr, group) in groupedFeatures do
            let header = sprintf "%s (%d)" (instr |> instrumentText) group.Length
            let g = 
              group 
                |> List.sortBy(fun x -> V3d.DistanceSquared(pos, x.geometry.coordinates.Head)) 
                |> take'(100)

            yield div [clazz "ui inverted item"][
                yield accordion header "Content" false [                    
                  div [clazz "ui list"] (viewFeatures instr g)
                ]
              ]
        }

     div [clazz "ui inverted segment"] [
       h2 [clazz "ui"] [text "Data:"]
       div [clazz "ui buttons"] [         
         button [clazz "ui button"; onClick (fun _ -> QueryDataFile)][text "Load"]
         button [clazz "ui button"; onClick (fun _ -> Reset)][text "Clear"]         
       ]

       h2 [clazz "ui"] [text "Comm:"]
       div [clazz "ui buttons"] [
         button [clazz "ui button"; onClick (fun _ -> ConnectVisplore)][text "Connect"]
         button [clazz "ui button"; onClick (fun _ -> WriteScreenCoordsDebug)][text "Send Selection"]
       ]
       
       queryGui model               
       propertiesGui          
       Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) listOfFeatures //AList.empty  
     ]


  let getSolBillboards (model : MModel) (view:IMod<CameraView>) (near:IMod<float>) =
        let lables =
            model.queryFilter.solLables
             |> AMap.map(fun txt pos ->
                Drawing.text view near (Mod.constant 60.0) pos (Trafo3d.Translation(pos)) (Mod.constant txt) (model.featureProperties.textSize.value)
                ) 
             |> AMap.toASet  
             |> ASet.map(fun x -> snd x)
             |> Aardvark.UI.``F# Sg``.Sg.set
        lables
        

  //let viewFeaturesSg1 (model : MModel) =
    //let points = model.queryFilter.sgFeatures.positions
    //Drawing.dots (positions |> AList.ofArray) model.queryFilter.pointSize.value  (C4b.Red |> Mod.constant)
    //Drawing.dots points model.queryFilter.pointSize.value  (C4b.Red |> Mod.constant)
        //|> AList.toASet 
        //|> Aardvark.UI.``F# Sg``.Sg.set 
       
  let viewFeaturesSg (model : MModel) =
    Drawing.drawIndexedGeometryPoints model.queryFilter.sgFeatures model.featureProperties.pointSize.value

  
  let myCss = [
    { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
    { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
    { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
  ]

  //let bodyAttributes = [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden;"]

  let viewWrapped pos (model : MModel) =
    require Html.semui (
      body [style "width: 100%; height:100%; background: #252525; overflow-x: hidden; overflow-y: scroll"] [
        div [clazz "ui inverted segment"] [viewFeaturesGui model]
      ])

  let threads (m : Model) = m.vplMessages

  let processMailboxAction (state:MailboxState) (cancelTokenSrc:CancellationTokenSource) (inbox:MessagingMailbox) (action : MailboxAction) =
      match action with
      | MailboxAction.InitMailboxState s ->     
        s
      | MailboxAction.MiniAction a ->        
        a |> Seq.singleton |> state.update 
        state      
        

  let messagingMailbox (cancelTokenSrc:CancellationTokenSource) (inbox:MessagingMailbox) =
        let rec messageLoop state = async {
            let! msg = inbox.Receive()
            
            let newState =
                try msg |> processMailboxAction state cancelTokenSrc inbox
                with
                | e ->
                    Log.line "ViewerMgmt Error: Dropping msg (Exception:  %O)" e
                    state
            
            return! messageLoop newState
        }
        messageLoop MailboxState.empty
 
  let start mailbox =
    
    App.start {
        unpersist = Unpersist.instance
        threads   = fun m -> m.vplMessages
        view      = viewWrapped (Mod.constant V3d.Zero) //localhost
        update    = update (CameraView.lookAt V3d.Zero V3d.One V3d.OOI)
        initial   = Initial.model mailbox
    }

    