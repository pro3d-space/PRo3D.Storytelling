namespace PRo3D

open System
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.AirState



module AnnotationProperties = 
    
    open Aardvark.Base
    
    type Action = 
        | SetGeometry     of Geometry
        | SetProjection   of Projection
        | SetSemantic     of Semantic
        | ChangeThickness of Numeric.Action
        | SetText         of string
        | ToggleVisible

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