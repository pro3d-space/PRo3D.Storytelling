namespace PRo3D

open System
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.AirState

module SurfaceProperties =
    open Aardvark.Base
    open Aardvark.Base.Rendering

    type Action =
        | SetFillMode   of FillMode
        | SetCullMode   of CullMode
        | SetName       of string
        | ToggleVisible 

    let update (model : Surface) (act : Action) =
        match act with
            | SetFillMode mode ->
                { model with fillMode = mode }
            | SetCullMode mode ->
                { model with cullMode = mode }
            | SetName s ->
                { model with name = s }
            | ToggleVisible ->
                { model with isVisible = not model.isVisible }

    let view (model : MSurface) =    
        require Html.semui (
            Html.table [                                            
               // Html.row "Path:"        [Incremental.text (model.importPath |> Mod.map (fun x -> sprintf "%A" x ))]                
                Html.row "Fillmode:"    [Html.SemUi.dropDown model.fillMode SetFillMode]
                Html.row "Cullmode:"    [Html.SemUi.dropDown model.cullMode SetCullMode]
                Html.row "Name:"        [Html.SemUi.textBox model.name SetName ]
                Html.row "Visible:"     [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
            ]
        )

module AnnotationProperties = 
    
    open Aardvark.Base
    
    type Action = 
        | SetGeometry     of Geometry
        | SetProjection   of Projection
        | SetSemantic     of Semantic
        | ChangeThickness of Numeric.Action
        | SetText         of string
        | ToggleVisible
        | ToggleShowDns

    let update (model : Annotation) (act : Action) =
        match act with
            | SetGeometry mode ->
                { model with geometry = mode }
            | SetSemantic mode ->
                { model with semantic = mode }
            | SetProjection mode ->
                { model with projection = mode }
            | ChangeThickness a ->
                { model with thickness = Numeric.update model.thickness a }
            | SetText t ->
                { model with text = t }
            | ToggleVisible ->
                { model with visible = (not model.visible) }
            | ToggleShowDns ->
                { model with showDns = (not model.showDns) }

    let view (model : MAnnotation) =        
        require Html.semui (
            Html.table [                            
                //Html.row "Geometry:"    [Html.SemUi.dropDown model.geometry SetGeometry]
                //Html.row "Projection:"  [Html.SemUi.dropDown model.projection SetProjection]      
                Html.row "Geometry:"    [Incremental.text (model.geometry |> Mod.map (fun x -> sprintf "%A" x ))]
                Html.row "Projection:"  [Incremental.text (model.projection |> Mod.map (fun x -> sprintf "%A" x ))]
                Html.row "Semantic:"    [Html.SemUi.dropDown model.semantic SetSemantic]      
                Html.row "Thickness:"   [Numeric.view' [InputBox] model.thickness |> UI.map ChangeThickness ]
                Html.row "Text:"        [Html.SemUi.textBox model.text SetText ]
                Html.row "Visible:"     [Html.SemUi.toggleBox model.visible ToggleVisible ]
                Html.row "Show DnS:"    [Html.SemUi.toggleBox model.showDns ToggleShowDns ]
            ]
        )

    let app = 
        {
            unpersist = Unpersist.instance
            threads = fun _ -> ThreadPool.empty
            initial = Annotation.initial
            update = update
            view = view
        }

    let start() = App.start app


module ConfigProperties =
    open Aardvark.Base
    open Aardvark.Base.Rendering

    type Action =
        | SetNearPlane              of Numeric.Action
        | SetFarPlane               of Numeric.Action
        | SetNavigationSensitivity  of Numeric.Action
        | SetArrowSize              of Numeric.Action
        | SetDnSPlaneSize           of Numeric.Action
        | SetMinDipAngle            of Numeric.Action
        | SetMaxDipAngle            of Numeric.Action

    let update (model : ViewConfigModel) (act : Action) =
        match act with
            | SetNearPlane s ->
                { model with nearPlane = Numeric.update model.nearPlane s }
            | SetFarPlane s ->
                { model with farPlane = Numeric.update model.farPlane s }
            | SetNavigationSensitivity s ->
                { model with navigationSensitivity = Numeric.update model.navigationSensitivity s }
            | SetArrowSize s ->
                { model with arrowSize = Numeric.update model.arrowSize s }
            | SetDnSPlaneSize s ->
                { model with dnsPlaneSize = Numeric.update model.dnsPlaneSize s }
            | SetMinDipAngle s ->
                { model with minDipAngle = Numeric.update model.minDipAngle s }
            | SetMaxDipAngle s ->
                { model with maxDipAngle = Numeric.update model.maxDipAngle s }

    let view (model : MViewConfigModel) =    
        require Html.semui (
            Html.table [      
                Html.row "Near Plane:"  [Numeric.view' [InputBox] model.nearPlane |> UI.map SetNearPlane ]               
                Html.row "Far Plane:"   [Numeric.view' [InputBox] model.farPlane |> UI.map SetFarPlane ]    
                Html.row "Navigation Sensitivity:"   [Numeric.view' [InputBox] model.navigationSensitivity |> UI.map SetNavigationSensitivity ]    
                Html.row "Arrow Size:"  [Numeric.view' [InputBox] model.arrowSize |> UI.map SetArrowSize ]               
                Html.row "D+S Plane Size:"   [Numeric.view' [InputBox] model.dnsPlaneSize |> UI.map SetDnSPlaneSize ]    
                Html.row "Min Dipping Angle:"   [Numeric.view' [InputBox] model.minDipAngle |> UI.map SetMinDipAngle ]
                Html.row "Max Dipping Angle:"   [Numeric.view' [InputBox] model.maxDipAngle |> UI.map SetMaxDipAngle ]
            ]
        )

module CameraProperties =
    let view (model : MCameraControllerState) =    
        require Html.semui (
            Html.table [      
                Html.row "Location:"    [Incremental.text (model.view |> Mod.map(fun x -> x.Location.ToString("0.00")))]
                Html.row "Forward:"     [Incremental.text (model.view |> Mod.map(fun x -> x.Forward.ToString("0.000")))]
                Html.row "Sky:"         [Incremental.text (model.view |> Mod.map(fun x -> x.Sky.ToString("0.000")))]
            ]
        )
    