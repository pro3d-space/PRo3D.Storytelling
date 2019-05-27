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

    let getDistFeatures (features:plist<Feature>) (view:CameraView) (dist:float) =
        let fl = 
            features
            |> PList.map(fun f -> 
                f.geometry.positions
                    |> List.choose(fun x -> if V3d.Distance(view.Location, x) < dist then Some f else None)
                    |> PList.ofList
            ) |> PList.concat
        fl

    let updateSgFeatures (features:plist<Feature>) =
        let positions' = 
            features
            |> PList.map(fun f -> f.geometry.positions |> PList.ofList )
            |> PList.concat
            |> PList.toArray

        let colors' = [| for i in 0 .. (positions'.Length - 1) -> C4b.Red |]
          
            
        {
            positions = positions'
            colors = colors'
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

    let update (view:CameraView) (model : QueryModel) (features:plist<Feature>) (msg : QueryAction) : QueryModel =
      match msg with
      | SetMinSol ms ->
        let minsol = Numeric.update model.minSol ms
        { model with minSol = minsol }
      | SetMaxSol ms ->
        let maxsol = Numeric.update model.maxSol ms
        { model with maxSol = maxsol }
      | SetDistance d ->
        let dist = Numeric.update model.distance d
        let distfeatures = getDistFeatures features view dist.value
        let solLables' = updateSolLables model distfeatures
        let sgfs = updateSgFeatures distfeatures
        { model with distance = dist; distFilteredFeatures = distfeatures; solLables = solLables' }
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
      | SetPointSize s ->
        let size = Numeric.update model.pointSize s
        { model with pointSize = size }

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
                Html.row "min sol:"  [Numeric.view' [NumericInputType.InputBox] model.minSol |> UI.map (fun x -> SetMinSol x)]
                Html.row "max sol:"  [Numeric.view' [NumericInputType.InputBox] model.maxSol |> UI.map (fun x -> SetMaxSol x)]
                Html.row "instruments:" [ ( viewQueryInstruments model )]
                Html.row "distance:" [Numeric.view' [NumericInputType.Slider] model.distance |> UI.map (fun x -> SetDistance x)] 
                Html.row "point size:" [Numeric.view' [NumericInputType.Slider] model.pointSize |> UI.map (fun x -> SetPointSize x)] 
            ]
        
        ) 