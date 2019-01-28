namespace PRo3D

module TranslantionApp =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering    

    open Aardvark.Base.Rendering   
    
    open Aardvark.SceneGraph.Opc
    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
    open Aardvark.VRVis.Opc

    open PRo3D.ReferenceSystem
    open PRo3D.Surfaces
    open Aardvark.Base.MultimethodTest

    //open Aardvark.UI.ChoiceModule
   
    type Action =
        | SetTranslation   of Vector3d.Action
        | ToggleVisible
    

    let update<'a> (model : Transformations) (act : Action) =
        match act with
            | SetTranslation t ->    
                let t' = Vector3d.update model.translation t
                { model with translation =  t'; trafo = Trafo3d.Translation t'.value}
            | ToggleVisible   -> 
                { model with useTranslationArrows = not model.useTranslationArrows}

    module Sg =
        let view<'ma> (position:IMod<V3d>) (mbigConfig : 'ma) (minnerConfig : ReferenceSystemApp.MInnerConfig<'ma>) (refsys:MReferenceSystem) (model:MTransformations) (cam:IMod<CameraView>)  : ISg<Action> =
                       
            let length    = minnerConfig.getArrowLength    mbigConfig
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig

            let east = Mod.map2(fun (l:V3d) (r:V3d) -> r.Cross(l) ) refsys.up.value refsys.northO // model.north.value

            let size = refsys.selectedScale |> Mod.map ReferenceSystemApp.Sg.scaleToSize

            let styleUp : ReferenceSystemApp.Sg.MarkerStyle = {
                position  = position
                direction = refsys.up.value
                color     = Mod.constant C4b.Blue
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant false
                text      = Mod.constant None
                fix       = Mod.constant false
            }

            let styleNorth : ReferenceSystemApp.Sg.MarkerStyle = {
                position  = position
                direction = refsys.northO //model.north.value
                color     = Mod.constant C4b.Red
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant true
                text      = Mod.constant (Some "N")
                fix       = Mod.constant false
            }

            let styleEast : ReferenceSystemApp.Sg.MarkerStyle = {
                position  = position
                direction = east
                color     = Mod.constant C4b.Green
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant false
                text      = Mod.constant None
                fix       = Mod.constant false
            }
            
            let sizeText = Sg.text cam near (Mod.constant 60.0) position (position |> Mod.map Trafo3d.Translation) refsys.selectedScale (Mod.constant 0.05)

            Sg.ofList [
                ReferenceSystemApp.Sg.point position (Mod.constant C4b.Red) cam
                sizeText
                styleUp    |> ReferenceSystemApp.Sg.directionMarker near cam  
                styleNorth |> ReferenceSystemApp.Sg.directionMarker near cam
                styleEast  |> ReferenceSystemApp.Sg.directionMarker near cam                     
            ]   |> Sg.onOff(model.useTranslationArrows)  
                

    module UI =
        open Aardvark.UI.Primitives.Mutable

        let viewV3dInput (model : MV3dInput) =  
            Html.table [                            
                Html.row "north" [Numeric.view' [InputBox] model.x |> UI.map Vector3d.Action.SetX]
                Html.row "east"  [Numeric.view' [InputBox] model.y |> UI.map Vector3d.Action.SetY]
                Html.row "up"    [Numeric.view' [InputBox] model.z |> UI.map Vector3d.Action.SetZ]
            ]       

        let view (model:MTransformations) =
            
            require GuiEx.semui (
                Html.table [  
                    Html.row "Visible:" [GuiEx.iconCheckBox model.useTranslationArrows ToggleVisible ]
                    Html.row "Translation:"      [viewV3dInput model.translation |> UI.map SetTranslation ]
                ]
            )
