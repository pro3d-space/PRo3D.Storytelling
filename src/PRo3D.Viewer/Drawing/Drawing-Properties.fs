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
        | SetTextSize     of Numeric.Action
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
             | SetTextSize s ->
                { model with textsize = Numeric.update model.textsize s }
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

        require GuiEx.semui (
            Html.table [                                            
                Html.row "Geometry:"    [Incremental.text (model.geometry |> Mod.map (fun x -> sprintf "%A" x ))]
                Html.row "Projection:"  [Incremental.text (model.projection |> Mod.map (fun x -> sprintf "%A" x ))]
                Html.row "Semantic:"    [Html.SemUi.dropDown model.semantic SetSemantic]      
                Html.row "Thickness:"   [Numeric.view' [InputBox] model.thickness |> UI.map ChangeThickness ]
                Html.row "Color:"       [ColorPicker.view model.color |> UI.map ChangeColor ]
                Html.row "Text:"        [Html.SemUi.textBox model.text SetText ]
                Html.row "TextSize:"    [Numeric.view' [InputBox] model.textsize |> UI.map SetTextSize ]
                Html.row "Visible:"     [GuiEx.iconCheckBox model.visible ToggleVisible ]
                Html.row "Show DnS:"    [GuiEx.iconCheckBox model.showDns ToggleShowDns ]
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

