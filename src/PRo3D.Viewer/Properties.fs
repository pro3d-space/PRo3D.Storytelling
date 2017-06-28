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
                //Html.row "Geometry:"    [Html.SemUi.dropDown model.geometry SetGeometry]
                //Html.row "Projection:"  [Html.SemUi.dropDown model.projection SetProjection]      
                Html.row "Path:"        [Incremental.text (model.importPath |> Mod.map (fun x -> sprintf "%A" x ))]                
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