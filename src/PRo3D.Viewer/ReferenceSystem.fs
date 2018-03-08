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
        | SetNorth         of Vector3d.Action
        | ToggleVisible
        | SetScale         of string
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
                { model with origin = p; north = ReferenceSystem.setV3d n; up = ReferenceSystem.setV3d up }, bigConfig
            | SetOrigin p -> 
                { model with origin = p }, bigConfig
            | SetUp up ->    
                let up = Vector3d.update model.up up
                let up = ReferenceSystem.setV3d up.value.Normalized
                { model with up =  up}, bigConfig
            | SetNorth  n     -> 
                let n = Vector3d.update model.north n
                let n = ReferenceSystem.setV3d n.value.Normalized
                
                { model with north = n }, bigConfig
            | ToggleVisible   -> 
                { model with isVisible = not model.isVisible}, bigConfig
            | SetArrowSize d  ->
                let big' = config.arrowLength.Set(bigConfig, d) 
                model, big'          
            | SetScale s ->
                { model with selectedScale = s }, bigConfig

    module Sg =

        type MarkerStyle = {
                position  : IMod<V3d>
                direction : IMod<V3d>
                color     : IMod<C4b>
                size      : IMod<float>
                thickness : IMod<float>
                hasArrow  : IMod<bool>
                text      : IMod<option<string>>
                fix       : IMod<bool>
            }

        let coneISg color radius size trafo =  
            Sg.cone 30 color radius size
                    |> Sg.noEvents
                    |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
                        do! Aardvark.Opc.Shader.stableTrafo
                        do! DefaultSurfaces.vertexColor
                       // do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.trafo(trafo)

        let directionMarker (near:IMod<float>) (cam:IMod<CameraView>) (style : MarkerStyle) =
            aset{
               
                let! dirs = style.direction
                
                let lineLengthScale = style.size                    
                let scaledLength = 
                    Mod.map2(fun (str:V3d) s -> str.Normalized * s) style.direction lineLengthScale

                //let scaledLength = dir * length
                let al = 
                    alist {
                        let! p = style.position
                        let! length = scaledLength
                        yield p
                        yield p + length
                    }

                let posTrafo =
                    style.position |> Mod.map(fun d -> Trafo3d.Translation(d))
                
                

                let coneTrafo = 
                    Mod.map2(fun p s -> Trafo3d.RotateInto(V3d.ZAxis, dirs.Normalized) * Trafo3d.Translation(p + s)) style.position scaledLength

                //let radius = length * 0.1
                //let scale =  DrawingApp.Sg.computeInvariantScale cam p 5.0
                let radius = lineLengthScale |> Mod.map(fun d -> d * 0.04)
                let coneSize = lineLengthScale |> Mod.map(fun d -> d * 0.3)

                let nLabelPos = Mod.map2(fun l r -> l + r) style.position scaledLength
                let nPosTrafo =
                    nLabelPos |> Mod.map(fun d -> Trafo3d.Translation(d))

                
                let label = 
                    style.text 
                      |> Mod.map(fun x ->
                        match x with 
                          | Some text -> Sg.text cam near (Mod.constant 60.0) nLabelPos nPosTrafo (Mod.constant text) (Mod.constant 0.05)
                          | None -> Sg.empty)


                let pT = 
                    adaptive {
                        let! v = cam
                        let! modelt = posTrafo
                        let! pos = style.position

                        return PRo3D.Sg.fixPosition v.ViewTrafo v.Location v.Forward v.Up modelt
                    }        
                
                yield PRo3D.Sg.lines al style.color (style.thickness |> Mod.map(fun x -> x*2.0)) posTrafo (Mod.constant Guid.Empty) 
                yield label |> Sg.dynamic
                    
                //yield coneISg style.color radius coneSize coneTrafo //(Mod.constant (Trafo3d.RotateInto(V3d.OOI, dir.Normalized) * Trafo3d.Translation(pos + scaledLength))) //                 
                 
            } |> Sg.set 

        let directionMarkerFix (near:IMod<float>) (cam:IMod<CameraView>) (style : MarkerStyle) (exp:IMod<V3d>) =
            aset{
               
                let! dirs = style.direction
                
                let lineLengthScale = style.size                    
                let scaledLength = 
                    Mod.map2(fun (str:V3d) s -> str.Normalized * s) style.direction lineLengthScale
                
                let! cam1 = cam
                //let scaledLength = dir * length
                let al = 
                    alist {
                        let! p = style.position
                        let! length = scaledLength
                        yield p
                        yield p + length
                    }

                let posTrafo =
                    //exp |> Mod.map(fun d -> Trafo3d.Translation(d))
                    style.position |> Mod.map(fun d -> Trafo3d.Translation(d))
                let camTrafo = Trafo3d.Translation(cam1.Location).Inverse
                let newTrafo = posTrafo |> Mod.map( fun d -> camTrafo * d )
                let newTrafoInv = newTrafo |> Mod.map(fun d -> d.Inverse)

                let posTrafoInv =
                    exp |> Mod.map(fun d -> Trafo3d.Translation(d).Inverse)
                
                let nLabelPos = Mod.map2(fun l r -> l + r) style.position scaledLength
                let nPosTrafo =
                    nLabelPos |> Mod.map(fun d -> Trafo3d.Translation(d))

                
                let label = 
                    style.text 
                      |> Mod.map(fun x ->
                        match x with 
                          | Some text -> Sg.text cam near (Mod.constant 60.0) nLabelPos nPosTrafo (Mod.constant text) (Mod.constant 0.05)
                          | None -> Sg.empty)


                let pT = 
                    adaptive {
                        let! v = cam
                        let! modelt = posTrafo
                        let! pos = style.position

                        return PRo3D.Sg.fixPosition v.ViewTrafo v.Location v.Forward v.Up modelt
                    }        
                
                yield PRo3D.Sg.fixLines al style.color (style.thickness |> Mod.map(fun x -> x*2.0)) posTrafo (Mod.constant Guid.Empty) 
                           //|> Sg.trafo posTrafoInv  
                yield label |> Sg.dynamic
                    
                //yield coneISg style.color radius coneSize coneTrafo //(Mod.constant (Trafo3d.RotateInto(V3d.OOI, dir.Normalized) * Trafo3d.Translation(pos + scaledLength))) //                 
                 
            } |> Sg.set 

           
        let point (pos:IMod<V3d>) (color:IMod<C4b>) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                yield Sg.dots (AList.single pos) color cam //there is also a method for drawing a single dot.
            } |> Sg.set

        let pointFix (pos:IMod<V3d>) (style:MarkerStyle) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                
                yield Sg.fixDots (AList.single pos) style.color cam //there is also a method for drawing a single dot.
            } |> Sg.set


//["1km";"100m";"10m";"1m";"10cm";"1cm";"1mm";"0.1mm"] 
        let scaleToSize (a:string) =
            match a with
              | "1km" -> 1000.0
              | "100m"-> 100.0
              | "10m" -> 10.0
              | "2m"  -> 2.0
              | "1m"  -> 1.0
              | "10cm"-> 0.1
              | "1cm" -> 0.01
              | "1mm" -> 0.001
              | "0.1mm" -> 0.0001
              | _     -> 1.0

        let view<'ma> (mbigConfig : 'ma) (minnerConfig : MInnerConfig<'ma>) (model:MReferenceSystem) (cam:IMod<CameraView>) : ISg<Action> =
                       
            let length    = minnerConfig.getArrowLength    mbigConfig
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig

            let east = Mod.map2(fun (l:V3d) (r:V3d) -> r.Cross(l) ) model.up.value model.north.value

            let size = model.selectedScale |> Mod.map scaleToSize

            let styleUp : MarkerStyle = {
                position  = model.origin
                direction = model.up.value
                color     = Mod.constant C4b.Blue
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant false
                text      = Mod.constant None
                fix       = Mod.constant false
            }

            let styleNorth : MarkerStyle = {
                position  = model.origin
                direction = model.north.value
                color     = Mod.constant C4b.Red
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant true
                text      = Mod.constant (Some "N")
                fix       = Mod.constant false
            }

            let styleEast : MarkerStyle = {
                position  = model.origin
                direction = east
                color     = Mod.constant C4b.Green
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant false
                text      = Mod.constant None
                fix       = Mod.constant false
            }

            
            let sizeText = Sg.text cam near (Mod.constant 60.0) model.origin (model.origin |> Mod.map Trafo3d.Translation) model.selectedScale (Mod.constant 0.05)

            Sg.ofList [
                point model.origin (Mod.constant C4b.Red) cam
                sizeText
                styleUp    |> directionMarker near cam  
                styleNorth |> directionMarker near cam
                styleEast  |> directionMarker near cam                     
            ]   |> Sg.onOff(model.isVisible)

        let cview<'ma> (mbigConfig : 'ma) (minnerConfig : MInnerConfig<'ma>) (model:MReferenceSystem) (cam:IMod<CameraView>) (explorerCenter:IMod<V3d>) : ISg<Action> =
            
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig
            
            let size = model.selectedScale |> Mod.map scaleToSize

            let styleUp : MarkerStyle = {
                position  = model.origin
                direction = model.up.value
                color     = Mod.constant C4b.Blue
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant false
                text      = Mod.constant None
                fix       = Mod.constant true
            }

            let styleNorth : MarkerStyle = {
                position  = model.origin
                direction = model.north.value
                color     = Mod.constant C4b.Red
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant true
                text      = Mod.constant (Some "N")
                fix       = Mod.constant true
            }

            let pT = 
                adaptive {
                    let! v = cam
                    let! modelt = (model.origin |> Mod.map Trafo3d.Translation)
                    let! pos = model.origin

                    return PRo3D.Sg.fixPosition v.ViewTrafo v.Location v.Forward v.Up modelt
                    //return PRo3D.Sg.screenAligned v.Forward v.Up modelt
                }        

            let sizeText = Sg.text cam near (Mod.constant 60.0) model.origin (model.origin |> Mod.map Trafo3d.Translation) model.selectedScale (Mod.constant 0.05)
                              
            Sg.ofList [
                point model.origin (Mod.constant C4b.Red) cam
                sizeText
                directionMarkerFix near cam styleUp explorerCenter
                directionMarkerFix near cam styleNorth explorerCenter              
            ]  |> Sg.onOff(model.isVisible)
                

    module UI =
        open Aardvark.UI.Primitives.Mutable

        let view (model:MReferenceSystem) (camera:MCameraControllerState) =
            let bearing = 
                adaptive {
                    let! up = model.up.value
                    let! north = model.north.value 
                    let! view = camera.view
                    return DipAndStrike.bearing up north view.Forward
                    }

            let pitch = 
                adaptive {
                    let! up = model.up.value
                    let! view = camera.view
                    return DipAndStrike.pitch up view.Forward
                    }

            require Html.semui (
                Html.table [                                                
                    Html.row "Pos:"     [Incremental.text (model.origin     |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Up:"      [Vector3d.view model.up |> UI.map SetUp ]
                    Html.row "North:"   [Vector3d.view model.north |> UI.map SetNorth ]
                    Html.row "Bearing:" [Incremental.text (bearing |> Mod.map (fun x -> x.ToString("0.00")))] // compute azimuth with view dir, north vector and up vector
                    Html.row "Pitch:"   [Incremental.text (pitch |> Mod.map (fun x -> x.ToString("0.00")))]  // same for pitch which relates to dip angle
                    Html.row "Height:"  [text "not implemented"]
                    Html.row "Visible:" [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
                    
                ]
            )
