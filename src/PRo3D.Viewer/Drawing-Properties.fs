namespace PRo3D

open Aardvark.Base
open System
open Aardvark.UI
open Aardvark.Base.Incremental
open PRo3D.Groups

module AnnotationProperties = 
    open Aardvark.UI.Static.Svg
            
    type Action = 
        | SetGeometry     of PRo3D.Geometry
        | SetProjection   of Projection
        | SetSemantic     of Semantic
        | ChangeThickness of Numeric.Action
        | ChangeColor     of ColorPicker.Action
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
            | ChangeColor a ->
                { model with color = ColorPicker.update model.color a }


    let view (model : MAnnotation) =   
        
        let height   = Mod.bindOption model.results Double.NaN (fun a -> a.height)
        let heightD  = Mod.bindOption model.results Double.NaN (fun a -> a.heightDelta)
        let length   = Mod.bindOption model.results Double.NaN (fun a -> a.length)
        let wLength  = Mod.bindOption model.results Double.NaN (fun a -> a.wayLength)

        require Html.semui (
            Html.table [                                            
                Html.row "Geometry:"    [Incremental.text (model.geometry |> Mod.map (fun x -> sprintf "%A" x ))]
                Html.row "Projection:"  [Incremental.text (model.projection |> Mod.map (fun x -> sprintf "%A" x ))]
                Html.row "Semantic:"    [Html.SemUi.dropDown model.semantic SetSemantic]      
                Html.row "Thickness:"   [Numeric.view' [InputBox] model.thickness |> UI.map ChangeThickness ]
                Html.row "Color:"       [ColorPicker.view model.color |> UI.map ChangeColor ]
                Html.row "Text:"        [Html.SemUi.textBox model.text SetText ]
                Html.row "Visible:"     [Html.SemUi.toggleBox model.visible ToggleVisible ]
                Html.row "Show DnS:"    [Html.SemUi.toggleBox model.showDns ToggleShowDns ]
                Html.row "Height:"      [Incremental.text (height   |> Mod.map  (fun d -> sprintf "%.4f" (d)))]
                Html.row "HeightDelta:" [Incremental.text (heightD  |> Mod.map  (fun d -> sprintf "%.4f" (d)))]
                Html.row "Length:"      [Incremental.text (length   |> Mod.map  (fun d -> sprintf "%.4f" (d)))]
                Html.row "WayLength:"   [Incremental.text (wLength  |> Mod.map  (fun d -> sprintf "%.4f" (d)))]
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

