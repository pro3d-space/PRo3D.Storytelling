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

    let updateSolLables (model : QueryModel) (features:plist<Feature>) =
        let minS = model.minSol.value
        let maxS = model.maxSol.value
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
        

    let updateSolLables2 (model : QueryModel) (features:plist<Feature>) =
      let range = new Range1i(model.minSol.value |> int, model.maxSol.value |> int)

      features 
        |> PList.toList 
        |> List.filter (fun x -> range.Contains(x.sol))
        |> List.map(fun x -> x.sol |> string, x.geometry.positions.Head) 
        |> HMap.ofList      
        

    let getDistFeatures (view:CameraView) (dist:float) (features:plist<Feature>) =
        features
            |> PList.map(fun f -> 
                f.geometry.positions
                    |> List.choose(fun x -> if V3d.Distance(view.Location, x) < dist then Some f else None)
                    |> PList.ofList
            ) |> PList.concat
        

    let solFilter (minsol:float) (maxsol:float) (features:plist<Feature>) =
        features
         |> PList.choose(fun f -> if f.sol >= int(minsol) && f.sol <= int(maxsol) then Some f else None)
       

    let instrumentFilter (model : QueryModel) (features:plist<Feature>) =
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

    let updateSgFeatures (features:plist<Feature>) =
        let positions' = 
            features
            |> PList.map(fun f -> f.geometry.positions |> PList.ofList )
            |> PList.concat
            |> PList.toArray

        let trafo' = 
            match positions' |> Array.tryHead with
             | Some p -> Trafo3d.Translation p
             | _ -> Trafo3d.Identity

        let colors' = [| for i in 0 .. (positions'.Length - 1) -> C4b.Red |]

        let c = 
            features
             |> PList.map( fun x -> x.instrument |> Model.instrumentColor )
             |> PList.toArray
             
            
        {
            positions = positions'
            colors = c
            trafo = trafo'
        }

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

    let updateFeaturesForRendering (model : QueryModel) (features:plist<Feature>) =
        Log.startTimed "[Minerva] updating sol labels"
        let solLabels = updateSolLables2 model features
        Log.stop()
        let sgfs = updateSgFeatures features
        { model with filteredFeatures = features; solLables = solLabels; sgFeatures = sgfs }
        
    
    let useQueriesForData (features:plist<Feature>) (qModel : QueryModel) (view:CameraView) =
        if qModel.useFileQueries then
            features
            |> getDistFeatures view qModel.distance.value
            |> solFilter qModel.minSol.value qModel.maxSol.value
            |> instrumentFilter qModel
            |> updateFeaturesForRendering qModel 
        else updateFeaturesForRendering qModel features
        
    
    let update (view:CameraView) (model : QueryModel) (features:plist<Feature>) (msg : QueryAction) : QueryModel =
      match msg with
      | SetMinSol ms ->
        let minsol = Numeric.update model.minSol ms
        let m = { model with minSol = minsol }
        useQueriesForData features m view
      | SetMaxSol ms ->
        let maxsol = Numeric.update model.maxSol ms
        let m = { model with maxSol = maxsol }
        useQueriesForData features m view
      | SetDistance d ->
        let dist = Numeric.update model.distance d
        let m = { model with distance = dist }
        useQueriesForData features m view
      | CheckMAHLI -> 
        let m = { model with checkMAHLI = not model.checkMAHLI }
        useQueriesForData features m view
      | CheckFrontHazcam -> 
        let m = { model with checkFrontHazcam = not model.checkFrontHazcam }
        useQueriesForData features m view
      | CheckMastcam -> 
        let m = { model with checkMastcam = not model.checkMastcam }
        useQueriesForData features m view
      | CheckAPXS -> 
        let m = { model with checkAPXS = not model.checkAPXS }
        useQueriesForData features m view
      | CheckFrontHazcamR -> 
        let m = { model with checkFrontHazcamR = not model.checkFrontHazcamR }
        useQueriesForData features m view
      | CheckFrontHazcamL -> 
        let m = { model with checkFrontHazcamL = not model.checkFrontHazcamL }
        useQueriesForData features m view
      | CheckMastcamR -> 
        let m = { model with checkMastcamR = not model.checkMastcamR }
        useQueriesForData features m view
      | CheckMastcamL -> 
        let m = { model with checkMastcamL = not model.checkMastcamL }
        useQueriesForData features m view
      | CheckChemLib -> 
        let m = { model with checkChemLib = not model.checkChemLib }
        useQueriesForData features m view
      | CheckChemRmi -> 
        let m = { model with checkChemRmi = not model.checkChemRmi }
        useQueriesForData features m view
      | SetPointSize s ->
        let size = Numeric.update model.pointSize s
        { model with pointSize = size }
      | UseQueriesForDataFile ->
        let m = { model with useFileQueries = not model.useFileQueries }
        useQueriesForData features m view

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
                Html.row ("MAHLI")  [iconCheckBox model.checkMAHLI CheckMAHLI]
                //Html.row (instrumentText Instrument.FrontHazcam)  [iconCheckBox model.checkFrontHazcam CheckFrontHazcam]
                //Html.row (instrumentText Instrument.Mastcam)  [iconCheckBox model.checkMastcam CheckMastcam]
                Html.row ("APXS")  [iconCheckBox model.checkAPXS CheckAPXS]
                Html.row ("FrontHazcamR")  [iconCheckBox model.checkFrontHazcamR CheckFrontHazcamR]
                Html.row ("FrontHazcamL")  [iconCheckBox model.checkFrontHazcamL CheckFrontHazcamL]
                Html.row ("MastcamR")  [iconCheckBox model.checkMastcamR CheckMastcamR]
                Html.row ("MastcamL")  [iconCheckBox model.checkMastcamL CheckMastcamL]
                Html.row ("ChemLib")  [iconCheckBox model.checkChemLib CheckChemLib]
                Html.row ("ChemRmi")  [iconCheckBox model.checkChemRmi CheckChemRmi]
            ]
        )

    let viewQueryFilters (model :MQueryModel) =
        require Html.semui ( 

            Html.table [ 
                Html.row ("use queries for data")  [iconCheckBox model.useFileQueries UseQueriesForDataFile]
                Html.row "min sol:"  [Numeric.view' [NumericInputType.InputBox] model.minSol |> UI.map (fun x -> SetMinSol x)]
                Html.row "max sol:"  [Numeric.view' [NumericInputType.InputBox] model.maxSol |> UI.map (fun x -> SetMaxSol x)]
                Html.row "instruments:" [ ( viewQueryInstruments model )]
                Html.row "distance:" [Numeric.view' [NumericInputType.Slider] model.distance |> UI.map (fun x -> SetDistance x)] 
                Html.row "point size:" [Numeric.view' [NumericInputType.Slider] model.pointSize |> UI.map (fun x -> SetPointSize x)] 
            ]
        
        ) 