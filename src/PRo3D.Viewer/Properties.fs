namespace PRo3D

open System
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.AirState
open PRo3D.Surfaces





module ConfigProperties =
    open Aardvark.Base
    open Aardvark.Base.Rendering    

    type Action =
        | SetNearPlane              of Numeric.Action
        | SetFarPlane               of Numeric.Action
        | SetNavigationSensitivity  of Numeric.Action
        | SetArrowLength            of Numeric.Action
        | SetArrowThickness         of Numeric.Action
        | SetDnSPlaneSize           of Numeric.Action
        //| SetMinDipAngle            of Numeric.Action
        //| SetMaxDipAngle            of Numeric.Action
        | ToggleLodColors
        | ToggleOrientationCube     
        

    let update (model : ViewConfigModel) (act : Action) =
        match act with
            | SetNearPlane s ->
                { model with nearPlane = Numeric.update model.nearPlane s }
            | SetFarPlane s ->
                { model with farPlane = Numeric.update model.farPlane s }
            | SetNavigationSensitivity s ->
                { model with navigationSensitivity = Numeric.update model.navigationSensitivity s }
            | SetArrowLength al ->
                { model with arrowLength = Numeric.update model.arrowLength al }
            | SetArrowThickness at ->
                { model with arrowThickness = Numeric.update model.arrowThickness at }
            | SetDnSPlaneSize s ->
                { model with dnsPlaneSize = Numeric.update model.dnsPlaneSize s }
            //| SetMinDipAngle s ->
            //    { model with minDipAngle = Numeric.update model.minDipAngle s }
            //| SetMaxDipAngle s ->
            //    { model with maxDipAngle = Numeric.update model.maxDipAngle s }
            | ToggleLodColors ->
                { model with lodColoring = not model.lodColoring}
            | ToggleOrientationCube  -> {model with drawOrientationCube = not model.drawOrientationCube}
           

    let view (model : MViewConfigModel) =    
        require GuiEx.semui (
            Html.table [      
                Html.row "Near Plane:"              [Numeric.view' [InputBox] model.nearPlane             |> UI.map SetNearPlane ]               
                Html.row "Far Plane:"               [Numeric.view' [InputBox] model.farPlane              |> UI.map SetFarPlane ]    
                Html.row "Navigation Sensitivity:"  [Numeric.view' [Slider] model.navigationSensitivity |> UI.map SetNavigationSensitivity ]    
                Html.row "Arrow Length:"            [Numeric.view' [InputBox] model.arrowLength           |> UI.map SetArrowLength ] 
                Html.row "Arrow Thickness:"         [Numeric.view' [InputBox] model.arrowThickness        |> UI.map SetArrowThickness ]   
                Html.row "D+S Plane Size:"          [Numeric.view' [InputBox] model.dnsPlaneSize          |> UI.map SetDnSPlaneSize ]    
                //Html.row "Min Dipping Angle:"       [Numeric.view' [InputBox] model.minDipAngle           |> UI.map SetMinDipAngle ]
                //Html.row "Max Dipping Angle:"       [Numeric.view' [InputBox] model.maxDipAngle           |> UI.map SetMaxDipAngle ]
                Html.row "Lod colors:"              [Html.SemUi.toggleBox model.lodColoring ToggleLodColors]
                Html.row "Orientation Cube: "       [Html.SemUi.toggleBox model.drawOrientationCube ToggleOrientationCube]
            ]
        )

module CameraProperties =
    let view (model : MCameraControllerState) =    
        require GuiEx.semui (
            Html.table [      
                Html.row "Location:"    [Incremental.text (model.view |> Mod.map(fun x -> x.Location.ToString("0.00")))]
                Html.row "Forward:"     [Incremental.text (model.view |> Mod.map(fun x -> x.Forward.ToString("0.000")))]
                Html.row "Sky:"         [Incremental.text (model.view |> Mod.map(fun x -> x.Sky.ToString("0.000")))]
            ]
        )
    