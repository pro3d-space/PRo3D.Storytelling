namespace PRo3D

module ReferenceSystemApp =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open PRo3D.Drawing

    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
   
    type Action =
        | InferCoordSystem of V3d
        | SetOrigin        of V3d
        | SetUp            of Vector3d.Action       
        | SetNorth         of V3d
        | ToggleVisible
        | SetViewConfig    of ViewConfigModel
 
    
    let upVector (point:V3d) = point.Normalized
    
    let northVector (up:V3d) =
        let east = V3d.OOI.Cross(up)
        up.Cross(east)
    
    let update (model : ReferenceSystem) (act : Action) =
        match act with
            | InferCoordSystem p ->
                let up = upVector p
                let n = northVector up
                { model with origin = p; north = n; up = ReferenceSystem.setV3d up }
            | SetOrigin p     -> { model with origin = p }
            | SetUp     up    ->    
                let up = Vector3d.update model.up up
                let up' = ReferenceSystem.setV3d up.value.Normalized
                { model with up =  up'}
            | SetNorth  n     -> { model with north = n }
            | ToggleVisible   -> { model with isVisible = not model.isVisible}     
            | SetViewConfig c -> { model with config = c }

    module Sg =

        let coneISg color radius size trafo =  
            Sg.cone 30 color radius size
                    |> Sg.noEvents
                    |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
                        do! Aardvark.Opc.Shader.stableTrafo
                        do! DefaultSurfaces.vertexColor
                        do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.trafo(trafo)

        let directionMarker (pos:IMod<V3d>) (dir:IMod<V3d>) (color:IMod<C4b>) (conf:MViewConfigModel) (length:IMod<float>) (cam:IMod<CameraView>) =
            aset{
               // let! pos = pos
                let! dirs = dir
                //let! length = length
                let lineLengthScale = DrawingApp.Sg.computeInvariantScale cam conf pos conf.arrowLength.value (Mod.constant 60.0)
                let scaledLength = 
                    Mod.map2(fun (str:V3d) s -> str.Normalized * s) dir lineLengthScale

                //let scaledLength = dir * length
                let al = 
                    alist {
                        let! p = pos
                        let! length = scaledLength
                        yield p
                        yield p + length
                    }

                let posTrafo =
                            pos |> Mod.map(fun d -> Trafo3d.Translation(d))

                let coneTrafo = 
                            Mod.map2(fun p s -> Trafo3d.RotateInto(V3d.ZAxis, dirs.Normalized) * Trafo3d.Translation(p + s)) pos scaledLength

                //let radius = length * 0.1
                //let scale =  DrawingApp.Sg.computeInvariantScale cam p 5.0
                let radius = lineLengthScale |> Mod.map(fun d -> d * 0.05)
                let coneSize = lineLengthScale |> Mod.map(fun d -> d * 0.3)

                yield DrawingApp.Sg.lines al color conf.arrowThickness.value posTrafo (Mod.constant Guid.Empty)
                yield coneISg color radius coneSize coneTrafo //(Mod.constant (Trafo3d.RotateInto(V3d.OOI, dir.Normalized) * Trafo3d.Translation(pos + scaledLength))) // 
            } |> Sg.set 
                
        
        let point (pos:IMod<V3d>) (color:IMod<C4b>) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                yield DrawingApp.Sg.dots (alist{yield pos}) color cam //there is also a method for drawing a single dot.
            } |> Sg.set


        let view (model:MReferenceSystem) (cam:IMod<CameraView>) =
                // order is irrelevant for rendering. change list to set,
                // since set provides more degrees of freedom for the compiler
            
                Sg.ofList [
                    point model.origin (Mod.constant C4b.Red) cam
                    directionMarker model.origin model.up.value (Mod.constant C4b.Blue) model.config model.config.arrowLength.value cam
                    directionMarker model.origin model.north (Mod.constant C4b.Red)  model.config model.config.arrowLength.value cam
                ] |> Sg.onOff(model.isVisible)
    module UI =
        let view (model:MReferenceSystem) =
            require Html.semui (
                Html.table [                                                
                    Html.row "Pos:"     [Incremental.text (model.origin     |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Up:"      [Vector3d.view model.up |> UI.map SetUp ]
                    Html.row "North:"   [Incremental.text (model.north      |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Bearing:" [text "not implemented"] // compute azimuth with view dir, north vector and up vector
                    Html.row "Pitch:"   [text "not implemented"] // same for pitch which relates to dip angle
                    Html.row "Height:"  [text "not implemented"]
                    Html.row "Visible:" [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
                    
                ]
            )
