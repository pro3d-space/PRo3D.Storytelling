namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
open Aardvark.VRVis
open Aardvark.SceneGraph

open PRo3D

module SurfaceProperties =
    
    type Action =
        | SetFillMode   of FillMode
        | SetCullMode   of CullMode
        | SetName       of string
        | ToggleVisible 
        | SetQuality    of Numeric.Action
        | SetPriority    of Numeric.Action

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
            | SetQuality a ->
                { model with quality = Numeric.update model.quality a }
            | SetPriority a ->
                { model with priority = Numeric.update model.priority a }

    let view (model : MSurface) =
        require Html.semui (
            Html.table [                                            
               // Html.row "Path:"        [Incremental.text (model.importPath |> Mod.map (fun x -> sprintf "%A" x ))]                
                Html.row "Fillmode:"    [Html.SemUi.dropDown model.fillMode SetFillMode]
                Html.row "Cullmode:"    [Html.SemUi.dropDown model.cullMode SetCullMode]
                Html.row "Name:"        [Html.SemUi.textBox model.name SetName ]
                Html.row "Visible:"     [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
                Html.row "Quality:"     [Numeric.view' [NumericInputType.Slider]   model.quality  |> UI.map SetQuality ]
                Html.row "Priority:"    [Numeric.view' [NumericInputType.InputBox] model.priority |> UI.map SetPriority ]
            ]
        )