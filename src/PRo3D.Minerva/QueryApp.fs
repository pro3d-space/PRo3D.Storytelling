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
      | CheckMAHLI -> 
        { model with checkMAHLI = not model.checkMAHLI }        
      | CheckFrontHazcam -> 
        { model with checkFrontHazcam = not model.checkFrontHazcam }        
      | CheckMastcam -> 
        { model with checkMastcam = not model.checkMastcam }        
      | CheckAPXS -> 
        { model with checkAPXS = not model.checkAPXS }        
      | CheckFrontHazcamR -> 
        { model with checkFrontHazcamR = not model.checkFrontHazcamR }        
      | CheckFrontHazcamL -> 
        { model with checkFrontHazcamL = not model.checkFrontHazcamL }        
      | CheckMastcamR -> 
        { model with checkMastcamR = not model.checkMastcamR }        
      | CheckMastcamL -> 
        { model with checkMastcamL = not model.checkMastcamL }        
      | CheckChemLib -> 
        { model with checkChemLib = not model.checkChemLib }        
      | CheckChemRmi -> 
        { model with checkChemRmi = not model.checkChemRmi }        

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
        
    let viewQueryInstruments (model :MQueryModel) =
        require Html.semui (
            Html.table [  
                Html.row ("MAHLI")         [iconCheckBox model.checkMAHLI CheckMAHLI]                
                Html.row ("APXS")          [iconCheckBox model.checkAPXS CheckAPXS]
                Html.row ("FrontHazcamR")  [iconCheckBox model.checkFrontHazcamR CheckFrontHazcamR]
                Html.row ("FrontHazcamL")  [iconCheckBox model.checkFrontHazcamL CheckFrontHazcamL]
                Html.row ("MastcamR")      [iconCheckBox model.checkMastcamR CheckMastcamR]
                Html.row ("MastcamL")      [iconCheckBox model.checkMastcamL CheckMastcamL]
                Html.row ("ChemLib")       [iconCheckBox model.checkChemLib CheckChemLib]
                Html.row ("ChemRmi")       [iconCheckBox model.checkChemRmi CheckChemRmi]
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