namespace PRo3D

module ReferenceSystemApp =
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

    //open Aardvark.UI.ChoiceModule
   
    type Action =
        | InferCoordSystem of V3d
        | SetOrigin        of V3d
        | SetUp            of Vector3d.Action
        | SetNorth         of Vector3d.Action
        | SetNOffset       of Numeric.Action
        | ToggleVisible
        | SetScale         of string
        | SetArrowSize     of double
    //    | SetViewConfig    of ViewConfigModel
        | SetPlanet        of Planet

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

    let updateVectorInDegree (up:V3d) (point:V3d) (origin:V3d) (theta:float) =
        //https://sites.google.com/site/glennmurray/Home/rotation-matrices-and-formulas
        
        let xyPlane = new Plane3d(up.Normalized, origin) 

        let u = xyPlane.Normal.X //up.X
        let v = xyPlane.Normal.Y //up.Y
        let w = xyPlane.Normal.Z //up.Z
        let a = xyPlane.Point.X //pos.X
        let b = xyPlane.Point.Y //pos.Y
        let c = xyPlane.Point.Z //pos.Z
        let x = point.X
        let y = point.Y
        let z = point.Z
        
        let t = theta.RadiansFromDegrees()

        let u2 = u*u
        let v2 = v*v
        let w2 = w*w
        let cosT = Math.Cos(t)
        let oneMinusCosT = 1.0-cosT
        let sinT = Math.Sin(t)
        
        // Build the matrix entries element by element.
        let m11 = u2 + (v2 + w2) * cosT
        let m12 = u*v * oneMinusCosT - w*sinT
        let m13 = u*w * oneMinusCosT + v*sinT
        let m14 = (a*(v2 + w2) - u*(b*v + c*w))*oneMinusCosT
                + (b*w - c*v)*sinT

        let m21 = u*v * oneMinusCosT + w*sinT
        let m22 = v2 + (u2 + w2) * cosT
        let m23 = v*w * oneMinusCosT - u*sinT
        let m24 = (b*(u2 + w2) - v*(a*u + c*w))*oneMinusCosT
                + (c*u - a*w)*sinT

        let m31 = u*w * oneMinusCosT - v*sinT
        let m32 = v*w * oneMinusCosT + u*sinT
        let m33 = w2 + (u2 + v2) * cosT
        let m34 = (c*(u2 + v2) - w*(a*u + b*v))*oneMinusCosT
                + (a*v - b*u)*sinT
                
        let px = m11*x + m12*y + m13*z + m14
        let py = m21*x + m22*y + m23*z + m24
        let pz = m31*x + m32*y + m33*z + m34

        V3d(px, py, pz)
    
   
    let upVector (point:V3d) (planet) = CooTransformation.getUpVector point planet //point.Normalized
    
    let northVector (up:V3d) =
        let east = V3d.OOI.Cross(up)
        up.Cross(east)

   
    let update<'a> (bigConfig : 'a) (config : ReferenceSystemConfig<'a>) (model : ReferenceSystem) (act : Action) =
        match act with
            | InferCoordSystem p ->
                let up = upVector p (model.planet)
                let n = northVector up 
                let no = updateVectorInDegree up n model.origin model.noffset.value 
                { model with origin = p; north = ReferenceSystem.setV3d n; up = ReferenceSystem.setV3d up; northO = no }, bigConfig
            | SetOrigin p -> 
                { model with origin = p }, bigConfig
            | SetUp up ->    
                let up = Vector3d.update model.up up
                let up = ReferenceSystem.setV3d up.value.Normalized
                { model with up =  up}, bigConfig
            | SetNorth  n     -> 
                let n = Vector3d.update model.north n
                let n = ReferenceSystem.setV3d n.value//.Normalized
                
                { model with north = n }, bigConfig
            | SetNOffset o ->
                let noffset = Numeric.update model.noffset o
                let no = updateVectorInDegree model.up.value model.north.value model.origin model.noffset.value 
                { model with noffset = noffset; northO = no }, bigConfig 
            | ToggleVisible   -> 
                { model with isVisible = not model.isVisible}, bigConfig
            | SetArrowSize d  ->
                let big' = config.arrowLength.Set(bigConfig, d) 
                model, big'          
            | SetScale s ->
                { model with selectedScale = s }, bigConfig
            | SetPlanet p ->                              
              let up = upVector model.origin p                                                
              let n = northVector up        
              let no = updateVectorInDegree up n model.origin model.noffset.value
              { model with  north = ReferenceSystem.setV3d n; up = ReferenceSystem.setV3d up; planet = p; northO = no }, bigConfig

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
                        do! Shader.stableTrafo
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

                yield PRo3D.Sg.lines al style.color style.thickness posTrafo (Mod.constant Guid.Empty)
                yield label |> Sg.dynamic                                   
                 
            } |> Sg.set 

           
        let point (pos:IMod<V3d>) (color:IMod<C4b>) (cam:IMod<CameraView>) =
            aset{
                let! pos = pos
                yield Sg.dots (AList.single pos) color //there is also a method for drawing a single dot.
            } |> Sg.set

//["1km";"100m";"10m";"1m";"10cm";"1cm";"1mm";"0.1mm"] 
        let scaleToSize (a:string) =
            match a with
              | "100km" -> 100000.0
              | "10km"  ->  10000.0
              | "1km"   ->   1000.0
              | "100m"  ->    100.0
              | "10m"   ->     10.0
              | "2m"    ->      2.0
              | "1m"    ->      1.0
              | "10cm"  ->      0.1
              | "1cm"   ->      0.01
              | "1mm"   ->      0.001
              | "0.1mm" ->      0.0001
              | _       ->      1.0

        let getOrientationSystem (mbigConfig : 'ma) (minnerConfig : MInnerConfig<'ma>) (model:MReferenceSystem) (cam:IMod<CameraView>) =
            let thickness = Mod.constant 2.0
            let near      = minnerConfig.getNearDistance   mbigConfig

            let east = Mod.map2(fun (l:V3d) (r:V3d) -> r.Cross(l) ) model.up.value model.north.value

            let size = Mod.constant 2.0
            let posTrafo = Mod.constant (Trafo3d.Translation(V3d.OOO))
            let position = Mod.constant V3d.OIO

            let styleUp : MarkerStyle = {
                position  = position
                direction = model.up.value
                color     = Mod.constant C4b.Magenta
                size      = size
                thickness = thickness
                hasArrow  = Mod.constant false
                text      = Mod.constant None
                fix       = Mod.constant false
            }


            let upV = 
                alist {
                    let! udir = model.up.value
                    let! position = position
                    yield position
                    yield position + udir.Normalized
                    }
                    
            let northV = 
                alist {
                    let! ndir = model.north.value
                    let! position = position
                    yield position
                    yield position + ndir.Normalized
                    }

            let eastV = 
                alist {
                    let! edir = east
                    let! position = position
                    yield position
                    yield position + edir.Normalized
                    }

            let nLabelPos = Mod.map2(fun l r -> l + r) position model.north.value
            let nPosTrafo =nLabelPos |> Mod.map(fun d -> Trafo3d.Translation(d))
            let label = Sg.text cam near (Mod.constant 60.0) nLabelPos nPosTrafo (Mod.constant "N") (Mod.constant 0.05)

            Sg.ofList [
                point model.origin (Mod.constant C4b.Red) cam
                //sizeText
                PRo3D.Sg.lines upV (Mod.constant C4b.Blue) thickness posTrafo (Mod.constant Guid.Empty)  
                styleUp    |> directionMarker near cam  
                PRo3D.Sg.lines northV (Mod.constant C4b.Red) thickness posTrafo (Mod.constant Guid.Empty) 
                PRo3D.Sg.lines eastV (Mod.constant C4b.Green) thickness posTrafo (Mod.constant Guid.Empty) 
                label                     
            ]   |> Sg.onOff(model.isVisible)       

        let view<'ma> (mbigConfig : 'ma) (minnerConfig : MInnerConfig<'ma>) (model:MReferenceSystem) (cam:IMod<CameraView>)  : ISg<Action> =
                       
            let length    = minnerConfig.getArrowLength    mbigConfig
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig

            let east = Mod.map2(fun (l:V3d) (r:V3d) -> r.Cross(l) ) model.up.value model.northO // model.north.value

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
                direction = model.northO //model.north.value
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

            //test
            //let n90 =
            //    adaptive {
            //        let! up = model.up.value
            //        let! n = model.north.value
            //        let! o = model.origin
            //        return updateVectorInDegree up n o 90.0
            //    }
                     
            //let styleNorth90 : MarkerStyle = {
            //    position  = model.origin
            //    direction =  n90 //Mod.constant(V3d(19417.0692,323595.2722,-323129.0951))//
            //    color     = Mod.constant C4b.Yellow
            //    size      = size
            //    thickness = thickness
            //    hasArrow  = Mod.constant true
            //    text      = Mod.constant (Some "90°")
            //    fix       = Mod.constant false
            //}

            
            let sizeText = Sg.text cam near (Mod.constant 60.0) model.origin (model.origin |> Mod.map Trafo3d.Translation) model.selectedScale (Mod.constant 0.05)

            Sg.ofList [
                point model.origin (Mod.constant C4b.Red) cam
                sizeText
                styleUp    |> directionMarker near cam  
                styleNorth |> directionMarker near cam
                //styleNorth90 |> directionMarker near cam
                //styleNorth120 |> directionMarker near cam
                //styleNorth180 |> directionMarker near cam
                //styleNorth260 |> directionMarker near cam
                styleEast  |> directionMarker near cam                     
            ]   |> Sg.onOff(model.isVisible)  
                //|> Sg.trafo (model.noffset.value |> Mod.map ( fun o -> (Trafo3d.RotationXInDegrees(o))))

    module UI =
        open Aardvark.UI.Primitives.Mutable

        let view (model:MReferenceSystem) (camera:MCameraControllerState) =
            let bearing = 
                adaptive {
                    let! up = model.up.value
                    let! north = model.northO //model.north.value 
                    let! view = camera.view
                    return DipAndStrike.bearing up north view.Forward
                    }

            let pitch = 
                adaptive {
                    let! up = model.up.value
                    let! view = camera.view
                    return DipAndStrike.pitch up view.Forward
                    }

            let height = 
                adaptive {
                        let! pos = model.origin
                        let! planet = model.planet
                        return CooTransformation.getHeight pos planet
                    }

            require GuiEx.semui (
                Html.table [                                                
                    Html.row "Pos:"     [Incremental.text (model.origin     |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Up:"      [Vector3d.view model.up |> UI.map SetUp ]
                    Html.row "North:"   [Incremental.text (model.north.value |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "NorthO:"  [Incremental.text (model.northO |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "N-Offset:"[Numeric.view' [InputBox] model.noffset |> UI.map SetNOffset ] 
                    Html.row "Bearing:" [Incremental.text (bearing |> Mod.map (fun x -> x.ToString("0.00")))] // compute azimuth with view dir, north vector and up vector
                    Html.row "Pitch:"   [Incremental.text (pitch |> Mod.map (fun x -> x.ToString("0.00")))]  // same for pitch which relates to dip angle
                    Html.row "Height:"  [Incremental.text (height |> Mod.map (fun x -> x.ToString("0.00")))]
                    Html.row "Visible:" [GuiEx.iconCheckBox model.isVisible ToggleVisible ]
                      
                ]
            )
