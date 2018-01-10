namespace PRo3D

module ReferenceSystemApp =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering    

    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
    open PRo3D.ReferenceSystem
    //open Aardvark.UI.ChoiceModule
   
    type Action =
        | InferCoordSystem of V3d
        | SetOrigin        of V3d
        | SetUp            of Vector3d.Action       
        | SetNorth         of V3d
        | ToggleVisible
        | SetArrowSize     of double
    //    | SetViewConfig    of ViewConfigModel

    type InnerConfig<'a> =
        {
            arrowLength     : Lens<'a,float>
            arrowThickness  : Lens<'a,float>            
        } 

    type MInnerConfig<'ma> =
        {
            getArrowLength    : 'ma -> IMod<float>
            getArrowThickness : 'ma -> IMod<float>
            getNearDistance   : 'ma -> IMod<float>
        }
    
    let upVector (point:V3d) = point.Normalized
    
    let northVector (up:V3d) =
        let east = V3d.OOI.Cross(up)
        up.Cross(east)
    
    let update<'a> (bigConfig : 'a) (config : ReferenceSystemConfig<'a>) (model : ReferenceSystem) (act : Action) =
        match act with
            | InferCoordSystem p ->
                let up = upVector p
                let n = northVector up
                { model with origin = p; north = n; up = ReferenceSystem.setV3d up }, bigConfig
            | SetOrigin p -> 
                { model with origin = p }, bigConfig
            | SetUp up ->    
                let up = Vector3d.update model.up up
                let up' = ReferenceSystem.setV3d up.value.Normalized
                { model with up =  up'}, bigConfig
            | SetNorth  n     -> 
                { model with north = n }, bigConfig
            | ToggleVisible   -> 
                { model with isVisible = not model.isVisible}, bigConfig
            | SetArrowSize d  ->
                let big' = config.arrowLength.Set(bigConfig, d) 
                model, big'           

    module Sg =

        let coneISg color radius size trafo =  
            Sg.cone 30 color radius size
                    |> Sg.noEvents
                    |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
                        do! Aardvark.Opc.Shader.stableTrafo
                        do! DefaultSurfaces.vertexColor
                       // do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.trafo(trafo)

        let directionMarker (pos:IMod<V3d>) (dir:IMod<V3d>) (color:IMod<C4b>) (near:IMod<float>) (size:IMod<float>) (arrowLength:IMod<float>) (arrowThickness:IMod<float>) (cam:IMod<CameraView>) =
            aset{
               
                let! dirs = dir
                
                let lineLengthScale = size
                    //Sg.computeInvariantScale cam near pos arrowLength (Mod.constant 60.0)
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
                
                let sizeText = 
                    size 
                      |> Mod.map(fun x -> sprintf "%Am" x) 
                      |> Sg.text cam near (Mod.constant 60.0) pos posTrafo

                let coneTrafo = 
                    Mod.map2(fun p s -> Trafo3d.RotateInto(V3d.ZAxis, dirs.Normalized) * Trafo3d.Translation(p + s)) pos scaledLength

                //let radius = length * 0.1
                //let scale =  DrawingApp.Sg.computeInvariantScale cam p 5.0
                let radius = lineLengthScale |> Mod.map(fun d -> d * 0.04)
                let coneSize = lineLengthScale |> Mod.map(fun d -> d * 0.3)

                let nLabelPos = Mod.map2(fun l r -> l + r) pos scaledLength
                let nPosTrafo =
                    nLabelPos |> Mod.map(fun d -> Trafo3d.Translation(d))
                let nLabel = Sg.text cam near (Mod.constant 60.0) nLabelPos nPosTrafo (Mod.constant "N")

                yield PRo3D.Sg.lines al color (arrowThickness |> Mod.map(fun x -> x*2.0)) posTrafo (Mod.constant Guid.Empty)
               // yield coneISg color radius coneSize coneTrafo //(Mod.constant (Trafo3d.RotateInto(V3d.OOI, dir.Normalized) * Trafo3d.Translation(pos + scaledLength))) // 
                yield sizeText
                yield nLabel
            } |> Sg.set 
                        
        let point (pos:IMod<V3d>) (color:IMod<C4b>) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                yield Sg.dots (AList.single pos) color cam //there is also a method for drawing a single dot.
            } |> Sg.set

        let view<'ma> (mbigConfig : 'ma) (minnerConfig : MInnerConfig<'ma>) (model:MReferenceSystem) (cam:IMod<CameraView>) : ISg<Action> =
                       
            let length    = minnerConfig.getArrowLength    mbigConfig
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig

            let east = Mod.map2(fun (l:V3d) (r:V3d) -> r.Cross(l) ) model.up.value model.north

            let size = Mod.constant 100.0

            Sg.ofList [
                point model.origin (Mod.constant C4b.Red) cam
                directionMarker model.origin model.up.value (Mod.constant C4b.Blue)  near size length thickness cam
                directionMarker model.origin model.north    (Mod.constant C4b.Red)   near size length thickness cam
                directionMarker model.origin east           (Mod.constant C4b.Green) near size length thickness cam                
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
