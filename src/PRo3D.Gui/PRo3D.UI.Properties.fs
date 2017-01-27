namespace PRo3DGui

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open DomainModel

open GuiExperiments

module ScenePropertiesApp =
    type Model = DomainModel.Scene

    type Action =
        | Set_Name of Text.Action

    let update (env: Env<Action>) (model : Model) (action : Action) =
        match action with
            | Set_Name n -> { model with Name = Text.update model.Name n }

    let view (model : Model) : DomNode<Action> =
        div [clazz "Segment"] [Text.view model.Name |> Html.map Set_Name]

module SurfacePropertiesApp =
    
    let column c = div [clazz "column"] c
    let row c = div [clazz "row"] c

    type Model = DomainModel.Surface
    
    type Action =
        | Set_ShadingMode       of Choice.Action
        | Set_Texture           of Choice.Action
        | Set_SetMap            of Choice.Action
        | Set_RenderMode        of Choice.Action
        | Set_IsVisible         of Toggle.Action
        | Set_IsActive          of Toggle.Action
        | Set_TriangleThreshold of Numeric.Action
        | Set_Trafo             of Transformation.Action
    
    let update (env: Env<Action>) (model : Model) (action : Action) =
        match action with
            | Set_ShadingMode       a -> { model with ShadingMode = Choice.update model.ShadingMode a }
            | Set_Texture           a -> { model with Texture = Choice.update model.Texture a }
            | Set_SetMap            a -> { model with Map = Choice.update model.Map a }
            | Set_RenderMode        a -> { model with RenderMode = Choice.update model.RenderMode a }
            | Set_IsVisible         a -> { model with IsVisible = Toggle.update model.IsVisible a }
            | Set_IsActive          a -> { model with IsActive = Toggle.update model.IsActive a }
            | Set_TriangleThreshold a -> { model with TriangleThreshold = Numeric.update env model.TriangleThreshold a }   
            | Set_Trafo             a -> { model with Trafo = Transformation.update env model.Trafo a }   
    
    let collapsing = "collapsing"
    let view (model : Model) : DomNode<Action> =
        div [] [
            accordion "Surface Properties" "options" true [
                table [clazz "ui celled striped table unstackable"] [
                    tbody [] [
                        tr [] [
                            td [clazz collapsing] [text "Name:"]; 
                            td [clazz "right aligned"] [text model.Name]
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Visiblity:"]; 
                            td [clazz "right aligned"] [Toggle.view model.IsVisible |> Html.map Set_IsVisible]
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Active State:"]; 
                            td [clazz "right aligned"] [Toggle.view model.IsActive |> Html.map Set_IsActive]
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Shading Mode:"]; 
                            td [clazz "right aligned"] [Choice.view model.ShadingMode |> Html.map Set_ShadingMode] 
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Texture:"]; 
                            td [clazz "right aligned"] [Choice.view model.Texture |> Html.map Set_Texture] 
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Map:"];
                            td [clazz "right aligned"] [Choice.view model.Map |> Html.map Set_SetMap] 
                        ]
                        tr [] [
                            td [clazz collapsing] [text "Render Mode:"];
                            td [clazz "right aligned"] [Choice.view model.RenderMode |> Html.map Set_RenderMode] 
                        ]                    
                        tr [] [
                            td [clazz collapsing] [text "TriangleThreshold:"];
                            td [clazz "right aligned"] [Numeric.view model.TriangleThreshold |> Html.map Set_TriangleThreshold]
                        ]                                 
                    ]
                ]
            ]        

            accordion "Transformation" "maximize" false [
                table [clazz "ui celled striped table unstackable"] [
                        tbody [] [
                            tr [] [
                                td [clazz collapsing] [text "Trafo:"];
                                td [clazz "right aligned"] [Transformation.view model.Trafo |> Html.map Set_Trafo]
                            ]    
                        ]
                    ]
                ]
        ]

module PropertiesApp = 
    type Model = Properties

    type Action =
        | SetSurfaceProperties of SurfacePropertiesApp.Action
        | SetSceneProperties   of ScenePropertiesApp.Action

    let update (env: Env<Action>) (model : Model) (action : Action) =
        match (action, model ) with
            | (SetSurfaceProperties a, SurfaceProperties p) -> 
                SurfacePropertiesApp.update (env |> Env.map SetSurfaceProperties) p a |> SurfaceProperties
            | (SetSceneProperties a, SceneProperties p) -> 
                ScenePropertiesApp.update (env |> Env.map SetSceneProperties) p a |> SceneProperties
            | (_ , NoProperties) -> model
            | _ -> failwith "encountered invalid property"

    let view (model : Model) : DomNode<Action> =
        let properties = 
            match model with
                | SurfaceProperties s -> [ SurfacePropertiesApp.view s |> Html.map SetSurfaceProperties]
                | SceneProperties   s -> [ ScenePropertiesApp.view s |> Html.map SetSceneProperties]
                | NoProperties          -> []

        div [Style [ "border", "1px solid black"; "height", "55vh"; "overflowY", "auto"]] properties