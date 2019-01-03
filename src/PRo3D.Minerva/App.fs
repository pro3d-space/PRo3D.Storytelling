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

        { model with data = data |> computeXYZFeatures}

  let instrumentText (instr : Instrument) =
    match instr with
    | Instrument.MAHLI       -> "MAHLI"
    | Instrument.FrontHazcam -> "FrontHazcam"
    | Instrument.Mastcam     -> "Mastcam"
    | Instrument.APXS        -> "APXS"
    | _ -> instr |> sprintf "unknown instrument number %A" |> failwith
    

  let viewFeatures (instr : Instrument) (features : list<Feature>) = 
    [
      for f in features do                                            
        let iconAttr = [clazz "ui map pin inverted middle aligned icon"]            
        let headerAttr : AttributeMap<_> = [clazz "ui header small"] |> AttributeMap.ofList
                        
        let item =             
          div [clazz "ui inverted item"][
            i iconAttr []
            div [clazz "ui content"] [
              Incremental.div (headerAttr) (AList.single (instr |> instrumentText |> text))
              div [clazz "ui description"] [text (f.id.ToString())] //[text (id.Replace('_',' '))]
            ]            
          ]
        yield item 
    ]

  let viewFeaturesGui (model :MModel) =
    
     let content =
        alist {                    
          let! features = model.data.features |> AList.toMod
          let groupedFeatures = features |> PList.toList |> List.groupBy(fun x -> x.instrument)

          for (instr, group) in groupedFeatures do
            let header = sprintf "%s (%d)" (instr |> instrumentText) group.Length
            yield div [clazz "ui inverted item"][
                //i [clazz "ui folder inverted middle aligned icon"] []
                div [clazz "ui header small"] [header |> text]
                //div [clazz "ui description"] ["description" |> text]
                div [clazz "ui list"] (viewFeatures instr group)
              ]
        }

     div [clazz "ui inverted segment"] [
       button [clazz "ui button"; onClick (fun _ -> PerformQueries)][text "Load Minerva"]
       Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) content  
     ]

  let drawPoint color size (pos : IMod<V3d>) = 
    Sg.sphere 2 color (Mod.constant 1.0)
        |> Sg.noEvents
        |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
        |> Sg.uniform "Size" size
        |> Sg.uniform "WorldPos" pos

  let viewFeaturesSg (model : MModel) =
    model.data.features 
      |> AList.map(fun x -> 
        x.geometry.positions 
          |> List.map(fun y -> 
            drawPoint (C4b.VRVisGreen |> Mod.constant) (1.5 |> Mod.constant) (y |> Mod.constant)) |> Sg.ofList
      ) 
      |> AList.toASet 
      |> Sg.set
      |> Sg.effect [
        DefaultSurfaces.stableTrafo |> toEffect
        DefaultSurfaces.vertexColor |> toEffect
      ]

    