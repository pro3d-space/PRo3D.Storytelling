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
        | SetUp            of V3d       
        | SetNorth         of V3d
        | ToggleVisible
        | SetArrowScale    of NumericInput
    
    let upVector (point:V3d) = point.Normalized
    
    let northVector (up:V3d) =
        let east = V3d.OOI.Cross(up)
        up.Cross(east)
    
    let update (model : ReferenceSystem) (act : Action) =
        match act with
            | InferCoordSystem p ->
                let up = upVector p
                let n = northVector up
                { model with origin = p; north = n; up = up }
            | SetOrigin p     -> { model with origin = p }
            | SetUp     up    -> { model with up = up }
            | SetNorth  n     -> { model with north = n }
            | ToggleVisible   -> { model with isVisible = not model.isVisible}     
            | SetArrowScale s -> { model with arrowScale = s }

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

        let directionMarker (pos:IMod<V3d>) (dir:IMod<V3d>) (color:IMod<C4b>) (scale:IMod<float>) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                let! dir = dir
                let! s = scale
                let length = dir * s
                let al = 
                    alist {
                        yield pos
                        yield pos + length
                    }
                let radius = s * 0.1
                //let scale =  DrawingApp.Sg.computeScale cam (Mod.constant pos) 5.0
                yield DrawingApp.Sg.lines al color (Mod.constant 1.0) (Mod.constant (Trafo3d.Translation(pos))) (Mod.constant Guid.Empty)
                yield coneISg color (Mod.constant radius) (Mod.constant s) (Mod.constant (Trafo3d.RotateInto(V3d.OOI, dir.Normalized) * Trafo3d.Translation(pos + length))) // 
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
                    directionMarker model.origin model.up (Mod.constant C4b.Blue) model.arrowScale.value cam
                    directionMarker model.origin model.north (Mod.constant C4b.Red) model.arrowScale.value cam
                ] |> Sg.onOff(model.isVisible)
    module UI =
        let view (model:MReferenceSystem) =
            require Html.semui (
                Html.table [                                                
                    Html.row "Pos:"     [Incremental.text (model.origin |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Up:"      [Incremental.text (model.up     |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "North:"   [Incremental.text (model.north  |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Bearing:" [text "not implemented"] // compute azimuth with view dir, north vector and up vector
                    Html.row "Pitch:"   [text "not implemented"] // same for pitch which relates to dip angle
                    Html.row "Height:"  [text "not implemented"]
                    Html.row "Visible:" [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
                ]
            )
