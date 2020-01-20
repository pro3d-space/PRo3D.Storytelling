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

module QueryApp =
           
        
    let filterByDistance (view:CameraView) (dist:float) (features:plist<Feature>) =
      features        
        |> PList.filter(fun f -> V3d.Distance(view.Location, f.geometry.positions.Head) < dist)                          

    let filterBySol (minsol:float) (maxsol:float) (features:plist<Feature>) =
      let range = new Range1i(minsol |> int, maxsol |> int)
      features |> PList.filter (fun x -> range.Contains(x.sol))
       
    let filterByInstrument (model : QueryModel) (features:plist<Feature>) =
        let fl = 
            features
            |> PList.choose(fun f -> 
                match f.instrument with
                  | Instrument.MAHLI -> if model.checkMAHLI then Some f else None        
                  | Instrument.FrontHazcam -> if model.checkFrontHazcam then Some f else None     
                  | Instrument.Mastcam -> if model.checkMastcam then Some f else None         
                  | Instrument.APXS -> if model.checkAPXS then Some f else None             
                  | Instrument.FrontHazcamR -> if model.checkFrontHazcamR then Some f else None     
                  | Instrument.FrontHazcamL -> if model.checkFrontHazcamL then Some f else None   
                  | Instrument.MastcamR -> if model.checkMastcamR then Some f else None      
                  | Instrument.MastcamL -> if model.checkMastcamL then Some f else None        
                  | Instrument.ChemLib -> if model.checkChemLib then Some f else None         
                  | Instrument.ChemRmi -> if model.checkChemRmi then Some f else None  
                  | _ -> None
            )
        fl
        
            
    let applyFilterQueries (features:plist<Feature>) (qModel : QueryModel) (view:CameraView) : plist<Feature> =        
      let filtered = 
        features
          |> filterByDistance view qModel.distance.value
          |> filterBySol qModel.minSol.value qModel.maxSol.value
          |> filterByInstrument qModel
          //|> updateFeaturesForRendering qModel
      Log.line "[Minerva] filtered set %d products" filtered.Count

      filtered
    
    let update (model : QueryModel) (msg : QueryAction) : QueryModel =
      match msg with
      | SetMinSol ms ->
        let minsol = Numeric.update model.minSol ms
        { model with minSol = minsol }        
      | SetMaxSol ms ->
        let maxsol = Numeric.update model.maxSol ms
        { model with maxSol = maxsol }        
      | SetDistance d ->
        let dist = Numeric.update model.distance d
        { model with distance = dist }        
      | UpdateDistance -> msg |> sprintf "[Minerva] not implemented %A" |> failwith
      | CheckInstrumentType inst -> 
        match inst with
            | Instrument.MAHLI -> { model with checkMAHLI = not model.checkMAHLI }  
            | Instrument.FrontHazcam -> { model with checkFrontHazcam = not model.checkFrontHazcam }  
            | Instrument.Mastcam -> { model with checkMastcam = not model.checkMastcam }
            | Instrument.APXS -> { model with checkAPXS = not model.checkAPXS }
            | Instrument.FrontHazcamR -> { model with checkFrontHazcamR = not model.checkFrontHazcamR }
            | Instrument.FrontHazcamL -> { model with checkFrontHazcamL = not model.checkFrontHazcamL }
            | Instrument.MastcamR -> { model with checkMastcamR = not model.checkMastcamR } 
            | Instrument.MastcamL -> { model with checkMastcamL = not model.checkMastcamL }
            | Instrument.ChemRmi  -> { model with checkChemRmi = not model.checkChemRmi }
            | Instrument.ChemLib  -> { model with checkChemLib = not model.checkChemLib }
            |_ -> failwith "unknown instrument"

    let iconToggle (dings : IMod<bool>) onIcon offIcon action =
        let toggleIcon = dings |> Mod.map(fun isOn -> if isOn then onIcon else offIcon)

        let attributes = 
            amap {
                let! icon = toggleIcon
                yield clazz icon
                yield onClick (fun _ -> action)
            } |> AttributeMap.ofAMap

        Incremental.i attributes AList.empty

    let mkColAttributeMap icon instrument = //action
        amap {
            let! icon = icon
            yield style (sprintf "color: %s" (Html.ofC4b (instrument |> Model.instrumentColor)))
            yield clazz icon
            //yield onClick (fun _ -> action)
        } |> AttributeMap.ofAMap

    let gp instrument = Incremental.i (mkColAttributeMap (Mod.constant "circle icon") instrument) AList.empty //|> UI.wrapToolTip "Set active"

    let iconCheckBox (dings : IMod<bool>) action =
        iconToggle dings "check square outline icon" "square icon" action
        
    let viewQueryInstruments (model :MQueryModel) =
        require Html.semui (
            Html.table [  
                Html.row ("MAHLI")         [gp Instrument.MAHLI; iconCheckBox model.checkMAHLI (CheckInstrumentType Instrument.MAHLI)]                
                Html.row ("APXS")          [gp Instrument.APXS;iconCheckBox model.checkAPXS (CheckInstrumentType Instrument.APXS)]
                Html.row ("FrontHazcamR")  [gp Instrument.FrontHazcamR;iconCheckBox model.checkFrontHazcamR (CheckInstrumentType Instrument.FrontHazcamR)]
                Html.row ("FrontHazcamL")  [gp Instrument.FrontHazcamL;iconCheckBox model.checkFrontHazcamL (CheckInstrumentType Instrument.FrontHazcamL)]
                Html.row ("MastcamR")      [gp Instrument.MastcamR;iconCheckBox model.checkMastcamR (CheckInstrumentType Instrument.MastcamR)]
                Html.row ("MastcamL")      [gp Instrument.MastcamL;iconCheckBox model.checkMastcamL (CheckInstrumentType Instrument.MastcamL)]
                Html.row ("ChemLib")       [gp Instrument.ChemLib;iconCheckBox model.checkChemLib (CheckInstrumentType Instrument.ChemLib)]
                Html.row ("ChemRmi")       [gp Instrument.ChemRmi;iconCheckBox model.checkChemRmi (CheckInstrumentType Instrument.ChemRmi)]
            ]
        )
    
    let viewQueryFilters (model :MQueryModel) =
      require Html.semui ( 
        Html.table [                 
            Html.row "min sol:"  [Numeric.view' [NumericInputType.InputBox] model.minSol |> UI.map (fun x -> SetMinSol x)]
            Html.row "max sol:"  [Numeric.view' [NumericInputType.InputBox] model.maxSol |> UI.map (fun x -> SetMaxSol x)]
            Html.row "instruments:" [ (viewQueryInstruments model)]
            Html.row "distance:" [Numeric.view' [NumericInputType.Slider; NumericInputType.InputBox] model.distance |> UI.map (fun x -> SetDistance x)]
            Html.row "100 nearest" [button [clazz "ui button"; onClick (fun _ -> UpdateDistance)][text "ok"]]          
        ]        
      ) 