namespace PRo3DGui

open Aardvark.Base

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

open DomainModel

open Aether
open Aether.Operators

open GuiExperiments

module Html =
    let ofC4b (c : C4b) = sprintf "rgb(%i,%i,%i)" c.R c.G c.B

    let table rows = table [clazz "ui celled striped table unstackable"] [ tbody [] rows ]

    let row k v =  tr [] [ td [clazz "collapsing"] [text k]; td [clazz "right aligned"] v ]

module ScenePropertiesApp =
    type Model = DomainModel.Scene

    type Action = 
        | Set_Name of Text.Action
        | Set_Desc of Text.Action

    let update (env: Env<Action>) (model : Model) (action : Action) =
        match action with
            | Set_Name n ->{ model with Name = Text.update env model.Name n }
            | Set_Desc n ->{ model with Description = Text.update env model.Description n }

    let view (model : Model) : DomNode<Action> =
        div[] [
            div [clazz "Segment"] [Text.view model.Name |> Html.map Set_Name]
            div [clazz "Segment"] [Text.view model.Description |> Html.map Set_Desc]
        ]

module SurfacePropertiesApp =
    
    let column c = div [clazz "column"] c
    let row c = div [clazz "row"] c

    type Model = DomainModel.Surface
    
    type Action =
        | Set_Name              of Text.Action
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
            | Set_Name              a -> { model with Name = Text.update env model.Name a }
            | Set_ShadingMode       a -> { model with ShadingMode = Choice.update env model.ShadingMode a }
            | Set_Texture           a -> { model with Texture = Choice.update env model.Texture a }
            | Set_SetMap            a -> { model with Map = Choice.update env model.Map a }
            | Set_RenderMode        a -> { model with RenderMode = Choice.update env model.RenderMode a }
            | Set_IsVisible         a -> { model with IsVisible = Toggle.update env model.IsVisible a }
            | Set_IsActive          a -> { model with IsActive = Toggle.update env model.IsActive a }
            | Set_TriangleThreshold a -> { model with TriangleThreshold = Numeric.update env model.TriangleThreshold a }   
            | Set_Trafo             a -> { model with Trafo = Transformation.update env model.Trafo a }   
    
    let collapsing = "collapsing"
    let view (model : Model) : DomNode<Action> =
        div [] [
            accordion "Surface Properties" "options" true [
                Html.table [                    
                    Html.row "Id:"                  [Text (sprintf "%d" model.Id)]
                    Html.row "Name:"                [Text.view model.Name                   |> Html.map Set_Name]
                    Html.row "Visiblity:"           [Toggle.view model.IsVisible            |> Html.map Set_IsVisible]
                    Html.row "Active State:"        [Toggle.view model.IsActive             |> Html.map Set_IsActive]
                    Html.row "Shading Mode:"        [Choice.view model.ShadingMode          |> Html.map Set_ShadingMode]
                    Html.row "Texture:"             [Choice.view model.Texture              |> Html.map Set_Texture]
                    Html.row "Map:"                 [Choice.view model.Map                  |> Html.map Set_SetMap]
                    Html.row "Texture:"             [Choice.view model.Texture              |> Html.map Set_Texture]
                    Html.row "Render Mode:"         [Choice.view model.RenderMode           |> Html.map Set_RenderMode]
                    Html.row "Triangle Threshold:"  [Numeric.view model.TriangleThreshold   |> Html.map Set_TriangleThreshold]                                                                   
                ]                
            ]        

            accordion "Transformation" "maximize" false [
                Html.table [        
                    Html.row "Trafo:" [Transformation.view model.Trafo |> Html.map Set_Trafo]                    
                ]
            ]                
        ]

module List =
    let rec updateIf (p : 'a -> bool) (f : 'a -> 'a) (xs : list<'a>) = 
        match xs with
            | x :: xs ->
                if(p x) then (f x) :: updateIf p f xs
                else x :: updateIf p f xs
            | [] -> []
    
module PropertiesApp = 
    type Model = DomainModel.DomainModel

    type Action =
        | SetSurfaceProperties of SurfacePropertiesApp.Action
        | SetSceneProperties   of ScenePropertiesApp.Action

    let inline (<==) (m, l) v = Optic.set l v m

    let update (env: Env<Action>) (model : Model) (action : Action) =
        let m = Optic.get Model.select_ model
        match (m, action) with            
                | (SurfaceProperties p, SetSurfaceProperties a) ->
                        let surf'     = SurfacePropertiesApp.update (env |> Env.map SetSurfaceProperties) p a
                        let surfaces' = List.updateIf (fun (x : Surface) -> x.Id = surf'.Id) (fun x -> surf') model.Scene.Surfaces.Surfaces.AsList
                                                
                        let model = (model, Model.select_) <== SurfaceProperties surf'
                        { model with Scene = { model.Scene with Surfaces = { model.Scene.Surfaces with Surfaces = PSet.ofList surfaces' }}}
                | (SceneProperties p, SetSceneProperties a) -> 
                        let scene = ScenePropertiesApp.update (env |> Env.map SetSceneProperties) p a
                        let model = (model, Model.select_) <== SceneProperties scene
                        { model with Scene = scene }
                        
                | (NoProperties, _) -> Optic.set Model.select_ m model
                | _ -> failwith "invalid property action combination"

    let view (model : Model) : DomNode<Action> =
        let m = Optic.get Model.select_ model
        let properties = 
            match m with
                | SurfaceProperties s -> [ SurfacePropertiesApp.view s |> Html.map SetSurfaceProperties]
                | SceneProperties   s -> [ ScenePropertiesApp.view s |> Html.map SetSceneProperties]
                | NoProperties        -> []

        div [Style [ "border", "1px solid black"; "height", "55vh"; "overflowY", "auto"]] properties