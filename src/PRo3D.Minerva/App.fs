namespace PRo3D.Minerva

open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Events

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

open PRo3D.Base

type MinervaAction =
  | PerformQueries
  | ConnectVisplore
  | FlyToProduct of V3d

module App =
  open System.Net.Sockets
  open Aardvark.SceneGraph.Opc

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

  let update (model : Model) (msg : MinervaAction) : Model =
      match msg with
      | PerformQueries -> 
        let data = model.queries |> MinervaGeoJSON.loadMultiple
        let pos = "[-2486641.16564715, 2289265.84573656, -276244.7871095]" |> V3d.Parse
        
        let features = data.features |> PList.sortBy(fun x -> x.sol)
           // |> PList.filter(fun (x:Feature) -> V3d.Distance(pos, x.geometry.positions.Head) < 10000.0)
            //|> PList.map(fun x -> sprintf "instr: %A id: %A position: %A" x.instrument x.id x.geometry.positions.Head)
        //    |> PList.toList
        //    |> writeLinesToFile ".\strangeProducts.txt"

        { model with data = { data with features = features } } //|> computeXYZFeatures}
      | ConnectVisplore -> model
      | FlyToProduct _ -> model

  let instrumentText (instr : Instrument) =
    match instr with
    | Instrument.MAHLI       -> "MAHLI"
    | Instrument.FrontHazcam -> "FrontHazcam"
    | Instrument.Mastcam     -> "Mastcam"
    | Instrument.APXS        -> "APXS"
    | Instrument.FrontHazcamR -> "FrontHazcamR"
    | Instrument.FrontHazcamL -> "FrontHazcamL"
    | Instrument.MastcamR -> "MastcamR"    
    | Instrument.MastcamL -> "MastcamL"
    | _ -> instr |> sprintf "unknown instrument number %A" |> failwith
    

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

  let viewFeaturesGui (pos : IMod<V3d>)(model :MModel) =
    
     let content =
        alist {                    
          let! features = model.data.features |> AList.toMod
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
       button [clazz "ui button"; onClick (fun _ -> PerformQueries)][text "Load Minerva"]
       button [clazz "ui button"; onClick (fun _ -> ConnectVisplore)][text "Connect To Visplore"]
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

  let viewFeaturesSg (model : MModel) =
    model.data.features 
      |> AList.map(fun f -> 
        dots (f.geometry.positions |> AList.ofList) (Mod.constant 5.0) (f.instrument |> Model.instrumentColor |> Mod.constant)
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

  let start =
    App.start {
        unpersist = Unpersist.instance
        threads   = fun _ -> ThreadPool.empty
        view      = viewWrapped (Mod.constant V3d.Zero) //localhost
        update    = update
        initial   = Initial.model
    }

    