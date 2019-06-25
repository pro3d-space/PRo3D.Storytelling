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
open PRo3D.Base.AnnotationDraft

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
  open PRo3D.Minerva.Communication.JsonNetworkCommand
  
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
        
    stream.Close()
    client.Close()
  
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
  
  let sendReplaceSelectionRequest (idList : list<string>) (comm : Communicator.Communicator) =         
    let name = "MinervaReplaceSelection"
    let para = {
      names           = [JsonNetworkCommand.Channel.IdList]
      numOfLists      = 1
      numOfEntries    = idList |> List.length
      image           = None
      imageResolution = None
      messageID       = "SelectionMessageID"
    }

    let data = { idList = idList; positions = List.empty; colors = List.empty}

    let jsonMessage = {name = name; parameters = para; data = data} |> JsonNetworkCommand.toJson
    
    comm.GetClient().SendMessageWaitForResponse(jsonMessage)

  let sendReplaceProjectionRequest (idList : list<string>) (positions : list<V2d>) (comm : Communicator.Communicator) =
                    
    let name = "MinervaReplaceProjection"
    let para = 
      { 
        names           = [ JsonNetworkCommand.Channel.IdList; JsonNetworkCommand.Channel.Positions]
        numOfLists      = 2
        numOfEntries    = idList |> List.length
        image           = "This/is/some/path/to/an/image.png" |> Some
        imageResolution = V2i(1024, 768) |> Some     
        messageID       = ""
      }

    let data = { idList = idList; positions = positions; colors = List.empty }

    let jsonMessage = {name = name; parameters = para; data = data} |> JsonNetworkCommand.toJson

    comm.GetClient().SendMessage(jsonMessage)
    
  let take' (n : int) (input : list<'a>) : list<'a> =
       if n >= input.Length then input else input |> List.take n

  let updateSolLabels (features:plist<Feature>) (position : V3d) = 
    features 
      |> PList.toList      
      |> List.map(fun x -> x.sol |> string, x.geometry.positions.Head) 
      |> HMap.ofList
      |> HMap.toList
      |> List.sortBy(fun (_,p) -> V3d.DistanceSquared(position, p))
      |> take' 50
      |> HMap.ofList
      
  let updateSgFeatureColors (features:plist<Feature>) (selected:hset<string>) =

    features
        |> PList.map( fun x -> 
            if HSet.contains x.id selected
            then 
                C4b.VRVisGreen
            else
                x.instrument |> Model.instrumentColor )
        |> PList.toArray

  let updateSgFeatures (features:plist<Feature>) =
    let names =
      features
        |> PList.map(fun f -> f.id)            
        |> PList.toArray

    let positions = 
      features
        |> PList.map(fun f -> f.geometry.positions.Head)            
        |> PList.toArray
    
    let trafo = 
      match positions |> Array.tryHead with
        | Some p -> Trafo3d.Translation p
        | _ -> Trafo3d.Identity
    
    let colors = 
      features
        |> PList.map( fun x -> x.instrument |> Model.instrumentColor )
        |> PList.toArray
                     
    {
        names = names
        positions = positions
        colors = colors
        trafo = trafo
    }

  let updateSelectedSgFeature (features:plist<Feature>) (selected:hset<string>) =
    features
        |> PList.filter( fun x -> HSet.contains x.id selected)
        |> updateSgFeatures
     
  let updateSelection (name:string) (model:Model) =
    let selection = model.selection.selectedProducts
    let newSelection = 
        if HSet.contains name selection
            then 
                HSet.remove name selection
            else
                HSet.add name selection
    //let colors = updateSgFeatureColors model.filteredFeatures newSelection
    let selectedSgs = updateSelectedSgFeature model.filteredFeatures newSelection
    { model with selection = { model.selection with selectedProducts = newSelection}; selectedSgFeatures = selectedSgs}
                     //sgFeatures = { model.sgFeatures with colors = colors}}

  let updateFeaturesForRendering (pos :V3d) (model:Model) =
    Log.startTimed "[Minerva] building sgs"
    let solLabels = updateSolLabels model.filteredFeatures pos //view frustum culling AND distance culling
    let sgFeatures = updateSgFeatures model.filteredFeatures
    Log.line "[Minerva] showing %d labels and %d products" solLabels.Count sgFeatures.positions.Length
    Log.stop()
    { model with solLabels = solLabels; sgFeatures = sgFeatures }

  let update (view:CameraView) frustum (model : Model) (msg : MinervaAction) : Model =
      match msg with     
      | SendSelection -> 
        match model.comm with
          | Some c -> sendReplaceSelectionRequest (model.data.features |> PList.map(fun x -> x.id) |> PList.toList) c
          | None   -> ()
        model
      | SendScreenSpaceCoordinates -> 
        let viewProj = view.ViewTrafo * Frustum.projTrafo frustum
        let coords = 
          model.data.features
            |> PList.toList 
            |> List.map(fun feat -> (feat.id, feat.geometry.positions.Head))
            |> List.map(fun (id,p) ->  
              let coord = (viewProj.Forward.TransformPosProj p)
              let coord = (V2d(coord.X, coord.Y) + V2d.One) * 0.5
              (id, coord))
        
        let width = 1920
        let height = ((float)width / (frustum |> Frustum.aspect)) |> int

        PRo3D.Base.Utilities.takeScreenshot "http://localhost:54321" width height "overview.jpg" @"c:\Users\ortner\Documents\visplore\Minerva"
      
        match model.comm with
        | Some c -> sendReplaceProjectionRequest (coords |> List.map fst) (coords |> List.map snd) c
        | None   -> ()
        model
      | PerformQueries -> failwith "[Minerva] not implemented"
        //try
        //  //let data = model.queries |> MinervaGeoJSON.loadMultiple        
        //  Log.startTimed "[Minerva] Fetching full dataset from Server"
        //  let data = idTestList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
        //  //let features = data.features |> PList.sortBy(fun x -> x.sol)    
        //  Log.stop()

        //  let queryM = QueryApp.updateFeaturesForRendering model.queryFilter data.features
        //  { model with data = data; queryFilter = queryM }

        //with e ->
        //  Log.error "%A" e.Message
        //  model
      | PerformIdQuery idList -> //failwith "[Minerva] not implemented"
        //Log.startTimed "fetching %d entries by ids" idList.Length
        //let data = idList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
        //Log.stop()
        Log.line "%A" idList
        //{ model with data = data }     
        model
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
                                        
                    let action = cmd.data.idList |> PerformIdQuery
                    bc.Enqueue action
                                                            
                    cmd.parameters.messageID
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
        let filters = QueryApp.update model.queryFilter msg        
        { model with queryFilter = filters}
      | ApplyFilters ->
        let filtered = QueryApp.applyFilterQueries model.data.features model.queryFilter view
        { model with filteredFeatures = filtered } |> updateFeaturesForRendering view.Location
      | Reset ->
        { model with filteredFeatures = model.data.features; queryFilter = Initial.queryFilter }        
      | LoadProducts ->                            
        Log.startTimed "[Minerva] Fetching full dataset from data file"
        let data = Files.loadDataFile
        Log.stop()        
        Log.line "[Minerva] found %d entries" data.features.Count   
        //let filtered = QueryApp.applyFilterQueries model.data.features model.queryFilter view
        
        { model with data = data; filteredFeatures = data.features} |> updateFeaturesForRendering view.Location //
      | SetPointSize s ->
        let size = Numeric.update model.featureProperties.pointSize s
        { model with featureProperties = { model.featureProperties with pointSize = size }}
      | SetTextSize s ->
        let size = Numeric.update model.featureProperties.textSize s
        { model with featureProperties = { model.featureProperties with textSize = size }}   
      | SingleSelectProduct name ->
        { model with selection = { model.selection with singleSelectProduct = Some name }}
      | AddProductToSelection name ->
        updateSelection name model
        ////let colors = updateSgFeatureColors model.filteredFeatures newSelection
        //let selectedSgs = updateSelectedSgFeature model.filteredFeatures newSelection
        //{ model with selection = { model.selection with selectedProducts = newSelection}; selectedSgFeatures = selectedSgs}
        //             //sgFeatures = { model.sgFeatures with colors = colors}}
      | PickProduct (hit, name) -> updateSelection name model
      

      //| ChangeInstrumentColor (c,instrument) -> 
      //  let color' = ColorPicker.update model.featureProperties.instrumentColor.color c
      //  let m' = _color.Set(model, color')
      //  let instC = _instColors.Get(m')
      //  let instC' =
      //      match instrument with
      //      | Instrument.MAHLI -> { instC with mahli = color'.c }
      //      | Instrument.FrontHazcam -> { instC with mahli = color'.c }
      //      | Instrument.Mastcam -> { instC with mahli = color'.c }
      //      | Instrument.APXS -> { instC with mahli = color'.c }
      //      | Instrument.FrontHazcamR -> { instC with mahli = color'.c }
      //      | Instrument.FrontHazcamL -> { instC with mahli = color'.c }
      //      | Instrument.MastcamR -> { instC with mahli = color'.c }
      //      | Instrument.MastcamL -> { instC with mahli = color'.c }
      //      | Instrument.ChemLib -> { instC with mahli = color'.c }
      //      | Instrument.ChemRmi -> { instC with mahli = color'.c }
      //      | _ -> sprintf "unknown instrument" |> failwith

  // GUI

  let mkColor (model :MModel) (feature : Feature) =
    model.selection
        |> Mod.map( fun s ->
            s.selectedProducts
            |> HSet.map(fun x -> x = feature.id)
            |> HSet.contains true
        )
        |> Mod.map (function x -> if x then C4b.VRVisGreen else (feature.instrument |> Model.instrumentColor))

  let isSingleSelect (model :MModel) (feature : Feature) =
    model.selection |> Mod.map( fun x -> 
        let selection = x.singleSelectProduct
        match selection with 
        | Some s -> s = feature.id
        | None -> false )

  let selectionColor (model :MModel) (feature : Feature) =
    model.selection |> Mod.map( fun s -> 
         s.selectedProducts
            |> HSet.map(fun x -> x = feature.id)
            |> HSet.contains true
        )
        |> Mod.map (function x -> if x then C4b.VRVisGreen else C4b.White)

  let viewFeatures (model :MModel) (instr : Instrument) (features : list<Feature>) = 
    [
      let ac = sprintf "color: %s" (Html.ofC4b C4b.White)

      for f in features do      
         //selection
        //let headerColor = 
        //    (isSingleSelect model f) 
        //        |> Mod.map(fun x -> 
        //        (if x then C4b.VRVisGreen else C4b.White) 
        //            |> Html.ofC4b 
        //            |> sprintf "color: %s"
        //        ) 

        let headerAttributes =
            amap {
                //yield onClick (fun _ -> SingleSelectProduct f.id)
                yield  onClick(fun _ -> AddProductToSelection f.id)
            } |> AttributeMap.ofAMap

        //let iconAttr = 
        //  [
        //    clazz "ui map pin inverted middle aligned icon"; 
        //    style (sprintf "color: %s" (f.instrument |> Model.instrumentColor |> Html.ofC4b))
        //    onClick(fun _ -> AddProductToSelection f.id)
        //    //onClick(fun _ -> FlyToProduct f.geometry.positions.Head)
        //  ]  
          
        let iconAttributes =
              amap {                  
                yield clazz "ui map pin inverted middle aligned icon"
                //yield  onClick(fun _ -> AddProductToSelection f.id)

                yield style (sprintf "color: %s" (Html.ofC4b (f.instrument |> Model.instrumentColor)))
              } |> AttributeMap.ofAMap

        let headerAttr : AttributeMap<_> = [clazz "ui header small"] |> AttributeMap.ofList
                        
        let item =             
          div [clazz "ui inverted item"][
            Incremental.i iconAttributes AList.empty //i iconAttributes []
            div [clazz "ui content"] [
              Incremental.div (AttributeMap.ofList [style ac]) (
                alist {
                        let! hc = selectionColor model f
                        let c = hc |> Html.ofC4b |> sprintf "color: %s"
                        yield div[clazz "header"; style c][
                             Incremental.div (headerAttributes) (AList.single (instr |> instrumentText |> text))
                        ]
                        yield div [clazz "ui description"] [
                                f.sol |> sprintf "Sol: %A" |> text
                                i [clazz "binoculars icon"; onClick (fun _ -> FlyToProduct f.geometry.positions.Head)][]
                                ]
                        //yield i [clazz "binoculars icon"; onClick (fun _ -> FlyToProduct f.geometry.positions.Head)][] //|> UI.wrapToolTip "FlyTo"
                         
                } )
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
                Html.row "text size:" [Numeric.view' [NumericInputType.InputBox] model.featureProperties.textSize |> UI.map (fun x -> SetTextSize x)] 
            ]        
        )      
       
     let listOfFeatures =
        alist {           
          let! features = model.filteredFeatures |> AList.toMod
          let groupedFeatures = features |> PList.toList |> List.groupBy(fun x -> x.instrument)
          let! pos = model.queryFilter.filterLocation

          for (instr, group) in groupedFeatures do

            let header = sprintf "%s (%d)" (instr |> instrumentText) group.Length
            let g = 
              group 
                |> List.sortBy(fun x -> V3d.DistanceSquared(pos, x.geometry.coordinates.Head)) 
                |> take'(20)

           

            yield div [clazz "ui inverted item"][
                yield accordion header "Content" false [                    
                  div [clazz "ui list"] (viewFeatures model instr g)
                ]
              ]
        }

     div [clazz "ui inverted segment"] [
       h4 [clazz "ui"] [text "Comm:"]
       div [clazz "ui buttons"] [
         button [clazz "ui button"; onClick (fun _ -> ConnectVisplore)][text "Connect"]
         button [clazz "ui button"; onClick (fun _ -> SendSelection)][text "Send Selection"]
         button [clazz "ui button"; onClick (fun _ -> SendScreenSpaceCoordinates)][text "Send Coords"]
         button [
           clazz "ui button"; 
           onEvent "onGetRenderId" [] (fun args -> Reset)
           clientEvent "onclick" "aardvark.processEvent(__ID__,'onGetRenderId', document.getElementsByClassName('mainrendercontrol')[0].id)"
         ] [text "SCREAM SHOT"]
       ]

       h4 [clazz "ui"] [text "Data:"]
       div [clazz "ui buttons"] [         
         button [clazz "ui button"; onClick (fun _ -> LoadProducts)][text "Load"]
         button [clazz "ui button"; onClick (fun _ -> Reset)][text "Reset"]         
         button [clazz "ui button"; onClick (fun _ -> ApplyFilters)][text "Filter"]         
       ]

       
       queryGui model               
       propertiesGui          
       Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) listOfFeatures //AList.empty  
     ]

  // GUI Standalone
  let myCss = [
    { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
    { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
    { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
  ]  

  let viewWrapped pos (model : MModel) =
    require Html.semui (
      body [style "width: 100%; height:100%; background: #252525; overflow-x: hidden; overflow-y: scroll"] [
        div [clazz "ui inverted segment"] [viewFeaturesGui model]
      ])

  // SG  
  let getSolBillboards (model : MModel) (view:IMod<CameraView>) (near:IMod<float>) =
    let lables =
      model.solLabels
        |> AMap.map(fun txt pos ->
           Drawing.text view near (Mod.constant 60.0) pos (Trafo3d.Translation(pos)) (Mod.constant txt) (model.featureProperties.textSize.value)
           ) 
        |> AMap.toASet  
        |> ASet.map(fun x -> snd x)
        |> Aardvark.UI.``F# Sg``.Sg.set
    lables
          
  let viewFeaturesSg (model : MModel) =
    let selectedPoints = Drawing.outlinePoints model.selectedSgFeatures model.featureProperties.pointSize.value
    let points = Drawing.drawIndexedGeometryPoints model.sgFeatures model.featureProperties.pointSize.value false
    let pickables = Drawing.getPickables model.sgFeatures model.featureProperties.pointSize.value model.picking
    Sg.ofList [points; selectedPoints; pickables] 
  
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
        update    = update (CameraView.lookAt V3d.Zero V3d.One V3d.OOI) (Frustum.perspective 90.0 0.001 100000.0 1.0)
        initial   = Initial.model mailbox
    }

    