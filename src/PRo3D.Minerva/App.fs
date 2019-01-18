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
  | FlyToProduct of V3d

module App =

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

  let update (model : Model) (msg : MinervaAction) : Model =
      match msg with
      | PerformQueries -> 
        let data = model.queries |> MinervaGeoJSON.loadMultiple
        { model with data = data } //|> computeXYZFeatures}
      | FlyToProduct _ -> model

  let instrumentText (instr : Instrument) =
    match instr with
    | Instrument.MAHLI       -> "MAHLI"
    | Instrument.FrontHazcam -> "FrontHazcam"
    | Instrument.Mastcam     -> "Mastcam"
    | Instrument.APXS        -> "APXS"
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
                Incremental.text ( 
                  pos 
                    |> Mod.map(fun p -> 
                      let dist = V3d.Distance(p,f.geometry.positions.Head) 
                      if dist > 10000.0 then "far far away" else dist |> sprintf "%A m")
                      )
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
       Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) content  
     ]

  let drawPoint color size (pos : IMod<V3d>) = 
    Sg.sphere 4 color size
        |> Sg.noEvents
        |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
        //|> Sg.uniform "PointSize" size
        //|> Sg.uniform "WorldPos" pos

  let viewFeaturesSg (model : MModel) =
    model.data.features 
      |> AList.map(fun f -> 
        f.geometry.positions 
          |> List.map(fun y -> 
            drawPoint (f.instrument |> Model.instrumentColor |> Mod.constant) (0.05 |> Mod.constant) (y |> Mod.constant)) |> Sg.ofList
      ) 
      |> AList.toASet 
      |> Sg.set
      |> Sg.effect [
        DefaultSurfaces.stableTrafo |> toEffect
        DefaultSurfaces.vertexColor |> toEffect
     //   DefaultSurfaces.pointSprite |> toEffect
      ]

  let start =
    App.start {
        unpersist = Unpersist.instance
        threads   = fun _ -> ThreadPool.empty
        view      = viewFeaturesGui (Mod.constant V3d.Zero)//localhost
        update    = update
        initial   = Initial.model
    }

    