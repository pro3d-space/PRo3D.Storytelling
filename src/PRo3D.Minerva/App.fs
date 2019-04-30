namespace PRo3D.Minerva

open Aardvark.UI
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
  //open Mutable.QueryFilterModule

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
   
   
  let getInstrumentQuery (filter:QueryFilter) =
    let mutable cqlStringInst = []
    if filter.checkMAHLI then cqlStringInst <- (cqlStringInst @ ["MAHLI"])
    //if filter.checkFrontHazcam then cqlStringInst <- (cqlStringInst @ [instrumentText Instrument.FrontHazcam])
    //if filter.checkMastcam then cqlStringInst <- (cqlStringInst @ [instrumentText Instrument.Mastcam])
    if filter.checkAPXS then cqlStringInst <- (cqlStringInst @ ["APXS"])
    if filter.checkFrontHazcamR then cqlStringInst <- (cqlStringInst @ ["FHAZ_RIGHT_B"])
    if filter.checkFrontHazcamL then cqlStringInst <- (cqlStringInst @ ["FHAZ_LEFT_B"])
    if filter.checkMastcamR then cqlStringInst <- (cqlStringInst @ ["MAST_RIGHT"])
    if filter.checkMastcamL then cqlStringInst <- (cqlStringInst @ ["MAST_LEFT"])
    if filter.checkChemLib then cqlStringInst <- (cqlStringInst @ ["CHEMCAM_LIBS"])
    if filter.checkChemRmi then cqlStringInst <- (cqlStringInst @ ["CHEMCAM_RMI"])
    let groupString = cqlStringInst |> String.concat("','")
    sprintf @"(instrumentId IN ('" + groupString + "'))"

    

  let getIdQuerySite (filter:QueryFilter) (ids:list<string>)  = 
    let groupString = ids |> String.concat("','")
    //let cql1 = [("cql", @"(identifier IN ('" + groupString + "'))")] |> HMap.ofList
    let cqlStringSol = sprintf @"(planetDayNumber >= %f AND planetDayNumber <= %f)"  filter.minSol.value filter.maxSol.value
    let cqlStringInst = getInstrumentQuery filter
    //let cqlStringAll = sprintf @"(" + cqlStringSol + " AND " + cqlStringInst + ")"
    let cql = [("cql", cqlStringSol)] |> HMap.ofList
    (@"https://minerva.eox.at/opensearch/collections/all/json/", cql)

  //let getDistFeatures' (features:alist<Feature>) (view:IMod<CameraView>) (dist:IMod<float>) =
  //  alist {
  //      let! distance = dist
  //      let! cam = view
  //      let fl = 
  //          features
  //          |> AList.map(fun f -> 
  //              f.geometry.positions
  //               |> List.choose(fun x -> if V3d.Distance(cam.Location, x) < distance then Some f else None)
  //               |> AList.ofList
  //          ) |> AList.concat
  //      yield fl
  //  } |> AList.concat

  let updateSolLables (model : Model) (features:plist<Feature>) =
    //let features = model.queryFilter.distFilteredFeatures
    let minS = model.queryFilter.minSol.value
    let maxS = model.queryFilter.maxSol.value
    let mutable lables = hmap.Empty
    for s in minS .. maxS do
        let solFeatures = features |> PList.filter (fun (f:Feature) -> f.sol = ((int)s)) |> PList.toList 
        if solFeatures.IsEmpty then
            lables |> ignore
        else
            let f = solFeatures.Head
            let pos = f.geometry.positions.Head
            let txt = sprintf "Sol %i" ((int)f.sol)
                
            lables <- lables.Add(txt, pos) //|> ignore
    lables

  let getDistFeatures (features:plist<Feature>) (view:CameraView) (dist:float) =
    let fl = 
        features
        |> PList.map(fun f -> 
            f.geometry.positions
                |> List.choose(fun x -> if V3d.Distance(view.Location, x) < dist then Some f else None)
                |> PList.ofList
        ) |> PList.concat
    fl
   

  let update (view:CameraView) (model : Model) (msg : MinervaAction) : Model =
      match msg with
      | Reset ->
        { model with data = Initial.data }
      | WriteScreenCoordsDebug -> model
      | PerformQueries -> 
        try
          //let data = model.queries |> MinervaGeoJSON.loadMultiple        
          Log.startTimed "[Minerva] Fetching full dataset from Server"
          let data = idTestList |> (getIdQuerySite model.queryFilter) |> fun (a,b) -> MinervaGeoJSON.loadPaged a b
          //let features = data.features |> PList.sortBy(fun x -> x.sol)    
          
          Log.stop()
          let m' = { model with data = data } //{ data with features = features } } //|> computeXYZFeatures}
          let solLables' = updateSolLables m' data.features
          { m' with queryFilter = {m'.queryFilter with distFilteredFeatures = data.features; solLables = solLables' }}
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
        Log.startTimed "[Minerva] loading products"
        let data = PRo3D.Base.Serialization.loadAs ".\data.minerva"
        Log.stop()
        Log.line "[Minerva] found %d entries" model.data.features.Count
        { model with data = data }
      | ConnectVisplore -> 
        if model.vplMessages.store.IsEmpty then

          //let evaluateCommand (cmd : PRo3D.Minerva.Communication.JsonNetworkCommand.CommandData) : string =
          //  match cmd.name with
          //  | "MinervaReplaceSelection" ->
          //     // Visplore sends Pro3D the selected ids --> do something with them (here: print them)
          //     cmd |> PRo3D.Minerva.Communication.JsonNetworkCommand.printCommand
          //     cmd.para.messageID
          //  | _ -> sprintf "dont know %A" cmd
          Log.line "[Minerva] connecting to visplore"
          let createWorker () =
             proclist {
               let bc = new HarriSchirchWrongBlockingCollection<_>()
               let comm = new Communicator.Communicator()
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

               comm.Close()
             }
          //comm.Start()
          //Thread.Sleep(60 * 100000)
          //comm.Close()        
          { model with vplMessages = ThreadPool.add "vpl server" (createWorker()) model.vplMessages }
        else
          model
      | FlyToProduct _ ->         
        model
      | SetMinSol ms ->
        let minsol = Numeric.update model.queryFilter.minSol ms
        { model with queryFilter = {model.queryFilter with minSol = minsol }}
      | SetMaxSol ms ->
        let maxsol = Numeric.update model.queryFilter.maxSol ms
        { model with queryFilter = {model.queryFilter with maxSol = maxsol }}
      | SetDistance d ->
        let dist = Numeric.update model.queryFilter.distance d
        let distfeatures = getDistFeatures model.data.features view dist.value
        let solLables' = updateSolLables model distfeatures
        { model with queryFilter = {model.queryFilter with distance = dist; distFilteredFeatures = distfeatures; solLables = solLables' }}
      | CheckMAHLI -> 
        { model with queryFilter = {model.queryFilter with checkMAHLI = not model.queryFilter.checkMAHLI }}
      | CheckFrontHazcam -> 
        { model with queryFilter = {model.queryFilter with checkFrontHazcam = not model.queryFilter.checkFrontHazcam }}
      | CheckMastcam -> 
        { model with queryFilter = {model.queryFilter with checkMastcam = not model.queryFilter.checkMastcam }}
      | CheckAPXS -> 
        { model with queryFilter = {model.queryFilter with checkAPXS = not model.queryFilter.checkAPXS }}
      | CheckFrontHazcamR -> 
        { model with queryFilter = {model.queryFilter with checkFrontHazcamR = not model.queryFilter.checkFrontHazcamR }}
      | CheckFrontHazcamL -> 
        { model with queryFilter = {model.queryFilter with checkFrontHazcamL = not model.queryFilter.checkFrontHazcamL }}
      | CheckMastcamR -> 
        { model with queryFilter = {model.queryFilter with checkMastcamR = not model.queryFilter.checkMastcamR }}
      | CheckMastcamL -> 
        { model with queryFilter = {model.queryFilter with checkMastcamL = not model.queryFilter.checkMastcamL }}
      | CheckChemLib -> 
        { model with queryFilter = {model.queryFilter with checkChemLib = not model.queryFilter.checkChemLib }}
      | CheckChemRmi -> 
        { model with queryFilter = {model.queryFilter with checkChemRmi = not model.queryFilter.checkChemRmi }}

  let iconToggle (dings : IMod<bool>) onIcon offIcon action =
    let toggleIcon = dings |> Mod.map(fun isOn -> if isOn then onIcon else offIcon)

    let attributes = 
        amap {
            let! icon = toggleIcon
            yield clazz icon
            yield onClick (fun _ -> action)
        } |> AttributeMap.ofAMap

    Incremental.i attributes AList.empty

  let iconCheckBox (dings : IMod<bool>) action =
    iconToggle dings "check square outline icon" "square icon" action

  
    
  let viewFeatures (pos : IMod<V3d>)(instr : Instrument) (features : list<Feature>) = 
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
  

  let viewQueryInstruments (model :MQueryFilter) =
    require Html.semui (
        Html.table [  
            Html.row (instrumentText Instrument.MAHLI)  [iconCheckBox model.checkMAHLI CheckMAHLI]
            //Html.row (instrumentText Instrument.FrontHazcam)  [iconCheckBox model.checkFrontHazcam CheckFrontHazcam]
            //Html.row (instrumentText Instrument.Mastcam)  [iconCheckBox model.checkMastcam CheckMastcam]
            Html.row (instrumentText Instrument.APXS)  [iconCheckBox model.checkAPXS CheckAPXS]
            Html.row (instrumentText Instrument.FrontHazcamR)  [iconCheckBox model.checkFrontHazcamR CheckFrontHazcamR]
            Html.row (instrumentText Instrument.FrontHazcamL)  [iconCheckBox model.checkFrontHazcamL CheckFrontHazcamL]
            Html.row (instrumentText Instrument.MastcamR)  [iconCheckBox model.checkMastcamR CheckMastcamR]
            Html.row (instrumentText Instrument.MastcamL)  [iconCheckBox model.checkMastcamL CheckMastcamL]
            Html.row (instrumentText Instrument.ChemLib)  [iconCheckBox model.checkChemLib CheckChemLib]
            Html.row (instrumentText Instrument.ChemRmi)  [iconCheckBox model.checkChemRmi CheckChemRmi]
        ]
    )

  let viewQueryFilters (model :MQueryFilter) =
    require Html.semui ( 
        Html.table [                
            Html.row "min sol:"  [Numeric.view' [NumericInputType.InputBox] model.minSol |> UI.map (fun x -> SetMinSol x)]
            Html.row "max sol:"  [Numeric.view' [NumericInputType.InputBox] model.maxSol |> UI.map (fun x -> SetMaxSol x)]
            Html.row "instruments:" [ ( viewQueryInstruments model )]
            Html.row "distance:" [Numeric.view' [NumericInputType.Slider] model.distance |> UI.map (fun x -> SetDistance x)] 
        ]
        
    ) 
  

  let viewFeaturesGui (pos : IMod<V3d>)(model :MModel) =
    
     let content =
        alist { 
          //let! features = model.data.features |> AList.toMod
          let! features = model.queryFilter.distFilteredFeatures |> AList.toMod
          let groupedFeatures = features |> PList.toList |> List.groupBy(fun x -> x.instrument)

          for (instr, group) in groupedFeatures do
            let header = sprintf "%s (%d)" (instr |> instrumentText) group.Length
            yield div [clazz "ui inverted item"][
                //i [clazz "ui folder inverted middle aligned icon"] []
            //    yield div [clazz "ui header small"] [header |> text]
                //div [clazz "ui description"] ["description" |> text]
                yield accordion header "Content" false [
                  div [clazz "ui list"] (viewFeatures pos instr group)
                ]
              ]
        }

     div [clazz "ui inverted segment"] [
       div [clazz "ui buttons"] [
         button [clazz "ui button"; onClick (fun _ -> PerformQueries)][text "Query DB"]
         button [clazz "ui button"; onClick (fun _ -> SaveFeatureCollection)][text "Save"]
         button [clazz "ui button"; onClick (fun _ -> LoadFeatureCollection)][text "Load"]
         button [clazz "ui button"; onClick (fun _ -> Reset)][text "Clear"]
         button [clazz "ui button"; onClick (fun _ -> WriteScreenCoordsDebug)][text "Export"]
         button [clazz "ui button"; onClick (fun _ -> ConnectVisplore)][text "Connect"]
       ]
       
       viewQueryFilters model.queryFilter                   
                     
       Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) content  
     ]

  let drawPoint color size (pos : IMod<V3d>) = 
    Sg.sphere 4 color size
        |> Sg.noEvents
        |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
        //|> Sg.uniform "PointSize" size
        //|> Sg.uniform "WorldPos" pos

  let mkDotISg color (size : IMod<float>) (pos : IMod<V3d>) = 
        Sg.sphere 2 color (Mod.constant 1.0)
            |> Sg.noEvents
            |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
            |> Sg.uniform "Size" size
            |> Sg.uniform "WorldPos" pos

  let dots (points : alist<V3d>) (size:IMod<float>) (color : IMod<C4b>) = //TODO TO refactor, put into base prj
    aset {
        for p in points |> ASet.ofAList do
           // let scale = computeScale view (Mod.constant p) 5.0                               
            yield mkDotISg color (size) (Mod.constant p)
    } 
    |> Sg.set
    |> Sg.effect [
        toEffect <| Shader.screenSpaceScale
        toEffect <| Shader.stableTrafo
        toEffect <| DefaultSurfaces.vertexColor          
    ]

  let indexedGeometryDots (points : alist<V3d>) (size:IMod<float>) (color : IMod<C4b>) =       
      let points' = points |> AList.toMod |> Mod.map(fun x -> x |> PList.toArray)
      let colors = points' |> Mod.map2(fun c x -> Array.create x.Length c) color
      
      Sg.draw IndexedGeometryMode.PointList
       |> Sg.vertexAttribute DefaultSemantic.Positions points'         
       |> Sg.vertexAttribute DefaultSemantic.Colors colors
       |> Sg.effect [
          toEffect Shader.stableTrafo
          toEffect DefaultSurfaces.vertexColor
          toEffect DefaultSurfaces.pointSprite
       ]
       |> Sg.uniform "PointSize" size
  
  //let computeInvariantScale (view : IMod<CameraView>) (near : IMod<float>) (p:IMod<V3d>) (size:IMod<float>) (hfov:IMod<float>) =
  //    adaptive {
  //        let! p = p
  //        let! v = view
  //        let! near = near
  //        let! size = size
  //        let! hfov = hfov
  //        let hfov_rad = Conversion.RadiansFromDegrees(hfov)
         
  //        let wz = Fun.Tan(hfov_rad / 2.0) * near * size
  //        let dist = V3d.Distance(p, v.Location)
      
  //        return ( wz / near ) * dist
  //    }
  let computeInvariantScale (view : IMod<CameraView>) (near : IMod<float>) (p:V3d) (size:IMod<float>) (hfov:IMod<float>) =
      adaptive {
          //let! p = p
          let! v = view
          let! near = near
          let! size = size
          let! hfov = hfov
          let hfov_rad = Conversion.RadiansFromDegrees(hfov)
         
          let wz = Fun.Tan(hfov_rad / 2.0) * near * size
          let dist = V3d.Distance(p, v.Location)
      
          return ( wz / near ) * dist
      }

  let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) =
        let right = up.Cross forw
        let rotTrafo = 
            new Trafo3d(
                new M44d(
                    right.X, up.X, forw.X, 0.0,
                    right.Y, up.Y, forw.Y, 0.0,
                    right.Z, up.Z, forw.Z, 0.0,
                    0.0,     0.0,  0.0,    1.0
                ),
                new M44d(
                    right.X, right.Y, right.Z, 0.0,
                    up.X,    up.Y,    up.Z,    0.0,
                    forw.X,  forw.Y,  forw.Z,  0.0,
                    0.0,     0.0,     0.0,     1.0
                )
        )
        rotTrafo * modelt
        

  let text (view : IMod<CameraView>) (near : IMod<float>) (hfov:IMod<float>) (pos:V3d) (modelTrafo:Trafo3d) (text:IMod<string>) (size:IMod<float>) =
         
      let invScaleTrafo = //Trafo3d.Scale(0.5)
          computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale //(Mod.constant 0.05)
      
      let billboardTrafo = 
          adaptive {
              let! v = view
              let modelt = modelTrafo
      
              return screenAligned v.Forward v.Up modelt
          }      
      
      Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
          |> Sg.noEvents
          |> Sg.shader {
            do! Shader.stableTrafo
          }            
          |> Sg.trafo invScaleTrafo //(Mod.constant invScaleTrafo)
          |> Sg.trafo billboardTrafo  


  let getSolBillboards (model : MModel) (view:IMod<CameraView>) (near:IMod<float>) =
    //alist {
        let lables =
            model.queryFilter.solLables
             |> AMap.map(fun txt pos ->
                text view near (Mod.constant 60.0) pos (Trafo3d.Translation(pos)) (Mod.constant txt) (Mod.constant 10.05)
                ) 
             |> AMap.toASet  
             |> ASet.map(fun x -> snd x)
             |> Sg.Set
        lables
        

        //let test =
        //  features
        //   |> AList.map( fun f ->
        //                let pos = f.geometry.positions.Head
        //                let nPosTrafo = Trafo3d.Translation(pos)
        //                let txt = sprintf "Sol %i" ((int)f.sol)
                    
        //                text view near (Mod.constant 60.0) (Mod.constant pos) (Mod.constant nPosTrafo) (Mod.constant txt) (Mod.constant 10.05)
        //                text view near (Mod.constant 60.0) pos nPosTrafo (Mod.constant txt) (Mod.constant 10.05)
        //        )
        

        //yield test     
    //}   |> Sg.set //|> AList.concat  |> AList.toASet

  let viewFeaturesSg (model : MModel) =
    //model.data.features 
    model.queryFilter.distFilteredFeatures 
        |> AList.map(fun f -> 
        dots (f.geometry.positions |> AList.ofList) (Mod.constant 5.0) (f.instrument |> Model.instrumentColor |> Mod.constant)
        //indexedGeometryDots (f.geometry.positions |> AList.ofList) (Mod.constant 5.0) (f.instrument |> Model.instrumentColor |> Mod.constant)
            //|> List.map(fun y -> 
            //  drawPoint (f.instrument |> Model.instrumentColor |> Mod.constant) (0.05 |> Mod.constant) (y |> Mod.constant)) |> Sg.ofList
        ) 
        |> AList.toASet 
        |> Sg.set
        |> Sg.effect [
        DefaultSurfaces.stableTrafo |> toEffect
        DefaultSurfaces.vertexColor |> toEffect
        //   DefaultSurfaces.pointSprite |> toEffect
        ]
    
    //Sg.ofList [sgFeatures; solBillboards]

  let myCss = [
    { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
    { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
    { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
  ]

  //let bodyAttributes = [style "background: #1B1C1E; height:100%; overflow-y:scroll; overflow-x:hidden;"]

  let viewWrapped pos (model : MModel) =
    require Html.semui (
      body [style "width: 100%; height:100%; background: #252525; overflow-x: hidden; overflow-y: scroll"] [
        div [clazz "ui inverted segment"] [viewFeaturesGui (pos)(model)]
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

    