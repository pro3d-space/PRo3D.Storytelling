namespace PRo3D

module OrientationDisplay =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open PRo3D.Drawing

    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
    //open Mutable.CoordinateSystemModelModule.Lens
    open MeasurementsImporter

    type Action =
        | SetCenter     of V3d
    
    let upVector (point:V3d) = point.Normalized

    let northVector (point:V3d) = 
        let up = upVector point
        up.Cross(V3d.OOI.Cross(up))
    
    let update (model : OrientationDisplayModel) (act : Action) =
        match act with
            | SetCenter p ->  { model with center = Some p; upVector = Some (upVector p); northVector = Some (northVector p) }
              

    module Sg =
        //open CoordinateSystem.Sg

        let coneISg color size trafo =         
            Sg.cone 30 color size (Mod.constant 0.2)
                    |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.vertexColor
                        do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.noEvents
                    |> Sg.trafo(trafo) 


        let directionMarker (pos:IMod<Option<V3d>>) (dir:IMod<Option<V3d>>) (color:IMod<C4b>) =
            aset{
                let! pos = pos
                let! dir = dir
                let dir = match dir with    
                            | Some x -> x.Normalized
                            | None -> V3d.NaN

                let length = dir * 0.3

                let pos = match pos with 
                            | Some x -> x
                            | None -> V3d.NaN
                let al = alist {
                                yield pos
                                yield pos + length
                                }
                yield DrawingApp.Sg.lines al color (Mod.constant 1.0) (Mod.constant (Trafo3d.Translation(pos))) (Mod.constant Guid.Empty)
                yield coneISg color (Mod.constant 0.02) (Mod.constant (Trafo3d.RotateInto(V3d.ZAxis, dir) * Trafo3d.Translation(pos+length))) 
                } |> Sg.set 
                
        
        let point (pos:IMod<Option<V3d>>) (color:IMod<C4b>) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                match pos with
                    | Some x -> yield DrawingApp.Sg.dots (alist{yield x}) (Mod.constant C4b.Magenta) cam
                    | None -> ()
                } |> Sg.set


        let view (model:MOrientationDisplayModel) (cam:IMod<CameraView>) =
                // order is irrelevant for rendering. change list to set,
                // since set provides more degrees of freedom for the compiler
            
                //let upLine = Sg.line model.center model.upVector (Mod.constant C4b.Magenta)
                //let northLine = Sg.line model.center model.northVector (Mod.constant C4b.Cyan)
                                
                Sg.ofList [
                    point model.center (Mod.constant C4b.Magenta) cam
                    directionMarker model.center model.upVector (Mod.constant C4b.Magenta)
                    directionMarker model.center model.northVector (Mod.constant C4b.Cyan)
                ]
