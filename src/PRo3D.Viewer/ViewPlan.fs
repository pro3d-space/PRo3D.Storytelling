namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.VRVis
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Base.Rendering

open System
open System.IO

open PRo3d
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Groups
open PRo3D.Navigation2
open Aardvark.UI.Primitives
open CSharpUtils

module ViewPlan = 
    open ViewPlanner
    open Aardvark.Base.MultimethodTest
    open Aardvark.Geometry
    

    type Action =
        | AddPoint         of V3d*ReferenceSystem*hmap<string, ConcreteKdIntersectionTree>*SurfaceModel 
        | SelectViewPlan   of Guid
        | FlyToViewPlan    of Guid
        | IsVisible        of Guid
        | RemoveViewPlan   of Guid
        | SelectInstrument of option<Instrument>
        | SelectAxis       of option<Axis>
        | ChangeAngle      of string * PRo3DNumeric.Action
        | ChangeFocal      of string * Numeric.Action
        | SetVPName        of string
             
    let loadRoverData (model : ViewPlanModel) (path : Option<string>) =      
      match path with
        | Some _ ->
            let directory = @".\InstrumentStuff"
            let errorCode = IPWrapper.Init(directory, directory)
            printfn "%A" errorCode

            let names = RoverProvider.platformNames()    
            printfn "%A" names

            let roverData =
                names 
                  |> Array.map RoverProvider.initRover 
                  |> List.ofArray 

            let rovs = roverData|> List.map(fun (r,_) -> r.id, r) 
            let rovsHm = rovs |> HMap.ofList
            let plats = roverData|> List.map(fun (r,p) -> r.id, p) |> HMap.ofList

            printfn "found rover(s): %d" (rovsHm |> HMap.count)

            let selected = 
                match rovs.IsEmptyOrNull() with
                    | true -> None
                    | _-> let srid, sr = rovs.[0]
                          Some sr
      
            { model with roverModel = { model.roverModel with rovers = rovsHm; platforms = plats; selectedRover = selected } }
        | _ -> model

    let updateViewPlans (vp:ViewPlan) (vps:hset<ViewPlan>) =
        vps
        |> HSet.map (fun x -> if x.id = vp.id then vp else x)
    
    //TODO LF refactor formatting indentation ... whole file. Maximum column width 120.
    let updateViewPlanFromRover (roverModel:RoverModel) (vp:ViewPlanModel) =
        // update rover model
        let m = {vp with roverModel = roverModel}
        // update selected view plan and viewplans
        match vp.selectedViewPlan with
            | Some v -> match roverModel.selectedRover with
                            | Some r -> { m with selectedViewPlan = Some { v with rover = r };  viewPlans = updateViewPlans v vp.viewPlans } //{ v with rover = r }
                            | None -> { m with viewPlans = updateViewPlans v vp.viewPlans } 
            | None -> m 
       
    let trafoFromTranslatedBase (position:V3d) (tilt:V3d) (forward:V3d) (right:V3d) : Trafo3d =  
        let rotTrafo =  //TODO LF refactor ... compare with Trafo fromOrthoNormalBase function
            new Trafo3d(
                new M44d(
                    forward.X, right.X, tilt.X, 0.0,
                    forward.Y, right.Y, tilt.Y, 0.0,
                    forward.Z, right.Z, tilt.Z, 0.0,
                    0.0,     0.0,  0.0,    1.0
                ),
                new M44d(
                    forward.X, forward.Y, forward.Z, 0.0,
                    right.X, right.Y, right.Z, 0.0,
                    tilt.X, tilt.Y, tilt.Z, 0.0,
                    0.0,     0.0,     0.0,     1.0
                )
        )
        (rotTrafo * Trafo3d.Translation(position))

    //TODO LF refactor ... use Math.sign. Checksign is also very ambigious function naming
    let checkSign (v1:V3d) (v2:V3d) =
        let sign = V3d.Dot(v2, v1)
        (sign <= 0.0)

    let hitF (p : V3d) (dir:V3d) (refSystem : ReferenceSystem.ReferenceSystem) (cache : hmap<string, ConcreteKdIntersectionTree>)(surfaceModel:SurfaceModel) = 
        let mutable cache = cache
        let ray = FastRay3d(p, dir)  
                 
        match SurfaceApp.doKdTreeIntersection surfaceModel refSystem ray cache with
            | Some (t,surf), c -> ray.Ray.GetPointOnRay(t) |> Some
            | None, c -> None
       
    let initialPlacementTrafo (position:V3d) (lookAt:V3d) (up:V3d) : Trafo3d =
        let forward = (lookAt - position).Normalized
        let n = V3d.Cross(forward, up.Normalized).Normalized
        let tilt = V3d.Cross(n, forward).Normalized
        let right = V3d.Cross(tilt, forward).Normalized

        trafoFromTranslatedBase position tilt forward right

        
    let calculateSurfaceContactPoint (roverWheel:V3d) (refSystem : ReferenceSystem.ReferenceSystem) (cache : hmap<string, ConcreteKdIntersectionTree>) (trafo:Trafo3d) (surfaceModel:SurfaceModel) =
        let tilt = trafo.Forward.UpperLeftM33().C2;
        let wheelT = trafo.Forward.TransformPos(roverWheel)
        let origin = wheelT + 5000.0 * tilt //move up ray origin
        hitF origin (-tilt) refSystem cache surfaceModel
        //Some origin
       
    let nearestPoint (x:V3d) (plane:Plane3d) =
        let p = plane.Point;
        (x - plane.Normal.Dot(x - p) * plane.Normal);
    
    
    let calculateRoverPlacement (working:list<V3d>) (wheels:list<V3d>) (up:V3d) : Trafo3d = 
        let forward = (working.[1] - working.[0]).Normalized
        // projects intersection points along the lookAt vector onto a plane
        let plane1 = new Plane3d(forward, working.[1]);
        let planeIntersectionPoints = wheels |> List.map(fun x -> nearestPoint x plane1) |> List.toArray

        // line fitting with the projected points to get right vector
        let rightVecLine = PlaneFitting.Line planeIntersectionPoints //fitLineLeastSquares planeIntersectionPoints

        let newRightVec = rightVecLine.Direction.Normalized
        let newTiltVec = V3d.Cross(forward, newRightVec).Normalized

        match checkSign up newTiltVec with
        | true -> let newTilt = newTiltVec.Negated
                  let newRight = newRightVec.Negated
                  trafoFromTranslatedBase working.[0] newTilt forward newRight
        | false -> trafoFromTranslatedBase working.[0] newTiltVec forward newRightVec
        

    let createViewPlan (working:list<V3d>) (rover:Rover) (ref:ReferenceSystem) (camState:CameraControllerState) (kdTree:hmap<string, ConcreteKdIntersectionTree>) (surfaceModel:SurfaceModel) =
        let initTrafo = initialPlacementTrafo working.[0] working.[1] ref.up.value
        
        let surfaceIntersectionPoints = // ray casting for surface intersection with rover wheels
          rover.wheelPositions 
            |> List.choose(fun x -> calculateSurfaceContactPoint x ref kdTree initTrafo surfaceModel)                 

        let placementTrafo = //initTrafo
          if (surfaceIntersectionPoints.Length > 2) then
            calculateRoverPlacement working surfaceIntersectionPoints ref.up.value
          else
            initTrafo
        
        let angle = 
            {
                value = 0.0
                min =  -90.0
                max = 90.0
                step = 0.1
                format = "{0:0.0}"
            }

        let newViewPlan = {
            id = Guid.NewGuid()
            name = rover.id
            position = working.[0]
            lookAt = working.[1]
            viewerState = camState
            vectorsVisible = true
            rover = rover
            roverTrafo = placementTrafo
            isVisible = true
            selectedInstrument = None
            selectedAxis = None
            currentAngle = angle
            }
        newViewPlan

    let createViewPlanFromFile (data:hmap<string,string>) (model:ViewPlanModel) (rover:Rover) (ref:ReferenceSystem) (camState:CameraControllerState) =
        let pos = V3d((data |> HMap.find "X").ToFloat(), (data |> HMap.find "Y").ToFloat(), (data |> HMap.find "Z").ToFloat())
        
        let posQ = V4d((data |> HMap.find "qX").ToFloat(), (data |> HMap.find "qY").ToFloat(), (data |> HMap.find "qZ").ToFloat(), (data |> HMap.find "qW").ToFloat())
        let rot = Rot3d(posQ.W,posQ.X, posQ.Y,posQ.Z)
        let forward = rot.TransformDir(ref.north.value) //ref.north.value
        let forward' = pos + forward
        let trafo = initialPlacementTrafo pos forward' ref.up.value
        let angle = 
            {
                value = 0.0
                min =  -90.0
                max = 90.0
                step = 0.1
                format = "{0:0.0}"
            }

        let newViewPlan = {
            id = Guid.NewGuid()
            name = rover.id
            position = pos
            lookAt = forward
            viewerState = camState
            vectorsVisible = true
            rover = rover
            roverTrafo = trafo
            isVisible = true
            selectedInstrument = None
            selectedAxis = None
            currentAngle = angle
            }
        { model with viewPlans = HSet.add newViewPlan model.viewPlans; working = List.Empty; selectedViewPlan = Some newViewPlan }

    let removeViewPlan (vps:hset<ViewPlan>) (id:Guid) = 
        vps |> HSet.filter (fun x -> x.id <> id)   
    
    let transformExtrinsics (vp:ViewPlan) (ex:Extrinsics) =
        { ex with position = vp.roverTrafo.Forward.TransformPos ex.position;
                  camLookAt = vp.roverTrafo.Forward.TransformDir ex.camLookAt;
                  camUp = vp.roverTrafo.Forward.TransformDir ex.camUp
                   }

    let updateInstrumentCam (vp:ViewPlan) (model:ViewPlanModel) =
        match vp.selectedInstrument with
            | Some i -> let ex = transformExtrinsics vp i.extrinsics
                        let frust = model.instrumentFrustum
                        let hfov = i.intrinsics.horizontalFieldOfView
                        let ifrustum = Frustum.perspective (hfov.DegreesFromGons()) frust.near frust.far 1.0
                        {model with instrumentCam = {model.instrumentCam with view = CameraView.lookAt ex.position (ex.position + ex.camLookAt) ex.camUp };
                                    instrumentFrustum = ifrustum} //CameraController.initial
                        //{ model with instrumentCam = cam } 
            | _-> {model with instrumentCam = {model.instrumentCam with view = CameraView.lookAt V3d.Zero V3d.One V3d.OOI };
                                    instrumentFrustum = Frustum.perspective 60.0 0.1 10000.0 1.0 }
        
    let updateRovers (model:ViewPlanModel) (roverModel:RoverModel) (vp:ViewPlan)=
        let r = roverModel.rovers  |> HMap.find vp.rover.id
        let i = match vp.selectedInstrument with
                    | Some i -> Some (r.instruments |> HMap.find i.id)
                    |_-> None
        let vp' = { vp with rover = r; selectedInstrument = i }  
        let m'= { model with selectedViewPlan = Some vp'; viewPlans = model.viewPlans |> HSet.map(fun x -> if x.id = vp.id then vp' else x) }
        let m'' = updateInstrumentCam vp' m'
        { m'' with roverModel = roverModel }

    //let update (model : ViewPlanModel) (camState:CameraControllerState) (action : Action) =
    let update (model : ViewPlanModel) (action : Action) (navigation : Lens<'a,NavigationModel>) (outerModel:'a) : ('a*ViewPlanModel) = 
        printfn "action: %A" action
        match action with
            | AddPoint (p,ref,kdTree,surfaceModel) ->
                match model.roverModel.selectedRover with
                    | Some r -> match model.working.IsEmpty with
                                    | true -> outerModel, {model with working = [p]} // first point (position)
                                    | false -> // second point (lookAt)
                                        let w = List.append model.working [p]
                                        let navigation = (navigation.Get outerModel)
                                        let wp = createViewPlan w r ref navigation.camera kdTree surfaceModel
                                        outerModel, { model with viewPlans = HSet.add wp model.viewPlans; working = List.Empty; selectedViewPlan = Some wp }
                    | None -> outerModel, model
            | SelectViewPlan id ->
                let vp = model.viewPlans |> Seq.tryFind(fun x -> x.id = id)
                let vp', m =
                    match vp, model.selectedViewPlan with
                        | Some a, Some b -> if a.id = b.id then None, model else Some a, (updateInstrumentCam a model)
                        | Some a, None  -> Some a, (updateInstrumentCam a model)
                        | None, _        -> None, model
                
                outerModel, { m with selectedViewPlan = vp' }
            | FlyToViewPlan id -> 
                let vp = model.viewPlans |> Seq.tryFind(fun x -> x.id = id)
                match vp with
                    | Some v-> let nav = (navigation.Get outerModel)
                               let nav' = { nav with camera = v.viewerState }
                               let newOuterModel = navigation.Set(outerModel, nav')
                               (newOuterModel, model)
                    | _ -> (outerModel,model) 
            | IsVisible id -> 
                outerModel, { model with viewPlans = model.viewPlans 
                                                        |> HSet.map(fun x -> if x.id = id then 
                                                                                { x with isVisible = not x.isVisible } 
                                                                                else x) }
            | RemoveViewPlan id ->
                let vp' = match  model.selectedViewPlan with
                            | Some v -> if v.id = id then None else Some v
                            | _ -> None
                let vps = removeViewPlan model.viewPlans id
                outerModel, { model with viewPlans = vps; selectedViewPlan = vp' }
            | SelectInstrument i -> match model.selectedViewPlan with
                                        | Some vp -> let newVp = { vp with selectedInstrument = i }
                                                     let m' = updateInstrumentCam newVp model
                                                     outerModel, { m' with selectedViewPlan = Some newVp; 
                                                                        viewPlans = model.viewPlans 
                                                                                        |> HSet.map(fun x -> if x.id = vp.id then 
                                                                                                                newVp 
                                                                                                                else x) }
                                        | None -> outerModel, model                                                     
            | SelectAxis a       -> match model.selectedViewPlan with
                                        | Some vp -> let newVp = { vp with selectedAxis = a }
                                                     outerModel, { model with selectedViewPlan = Some newVp; 
                                                                            viewPlans = model.viewPlans 
                                                                                            |> HSet.map(fun x -> if x.id = vp.id then
                                                                                                                    newVp 
                                                                                                                    else x) }
                                        | None -> outerModel, model    
            | ChangeAngle (id,a) ->  match model.selectedViewPlan with
                                        | Some vp -> match vp.rover.axes.TryFind id with
                                                        | Some ax -> 
                                                            let angle = PRo3DNumeric.update ax.angle a
                                                            let ax' = { ax with angle = angle } 
                                                            let vp' = { vp with rover = {vp.rover with axes = (vp.rover.axes 
                                                                                                                |> HMap.update id (fun _ -> ax')) };
                                                                                                                currentAngle = angle }
                                                            let roverModel' = RoverApp.updateAnglePlatform { roverId = vp'.rover.id ;
                                                                                                                axisId = ax'.id ; 
                                                                                                                angle = ax'.angle.value } 
                                                                                                                model.roverModel
                                 
                                                            let m' = updateRovers model roverModel' vp'
                                                            
                                                            outerModel, m'
                                                        | None -> outerModel, model
                                      
                                        | None -> outerModel, model        
            | ChangeFocal (id, f) ->
                match model.selectedViewPlan with
                    | Some vp -> 
                        match vp.selectedInstrument with
                        | Some inst ->   
                            let focal = Numeric.update inst.focal f
                            let inst' =  { inst with focal = focal }
                            let instruments' = 
                                vp.rover.instruments |> HMap.update id (fun x -> 
                                    match x with
                                        | Some _ -> inst'
                                        | None   -> failwith "instrument not found")
                            let vp' = {vp with rover = {vp.rover with instruments = instruments'}; selectedInstrument = Some inst'}
                            let roverModel' = RoverApp.updateFocusPlatform { roverId = vp'.rover.id ; 
                                                                             instrumentId = inst'.id ; 
                                                                             focal = inst'.focal.value } 
                                                                             model.roverModel
                            outerModel, updateRovers model roverModel' vp'
                        | None -> outerModel, model                                       
                    | None -> outerModel, model 
            | SetVPName t -> match model.selectedViewPlan with
                                | Some vp -> let vp' = {vp with name = t}
                                             outerModel, {model with selectedViewPlan = Some vp';
                                                                     viewPlans = model.viewPlans 
                                                                                    |> HSet.map(fun x -> if x.id = vp.id then
                                                                                                            vp' 
                                                                                                            else x) }              
                                | _ -> outerModel, model
                
            //| _ -> model

    module Sg =
                        
        //TODO LF refactor: try without matching bools, without aset expressions, without direct index access
        let drawWorking (model:MViewPlanModel) =
            let point0 =
                aset{
                let! w = model.working
                match w.IsEmpty with
                    | false -> yield PRo3D.Sg.dots (AList.single w.[0]) (Mod.constant 3.0) (Mod.constant C4b.Green)
                    | true -> ()
                } |> Sg.set

            let point1 =
                aset{
                let! w = model.working
                match w.IsEmpty with
                    | false -> 
                      match w.Length > 1 with
                        | true -> yield PRo3D.Sg.dots (AList.single w.[1]) (Mod.constant 3.0) (Mod.constant C4b.Yellow)
                        | false -> ()
                    | true -> ()
                } |> Sg.set

            Sg.ofList [point0;point1]
        
        let drawInitPositions (viewPlan : MViewPlan) (cam:IMod<CameraView>)=
            Sg.ofList [
                    ReferenceSystemApp.Sg.point viewPlan.position (Mod.constant C4b.Green) cam // position
                    ReferenceSystemApp.Sg.point viewPlan.lookAt (Mod.constant C4b.Yellow) cam // lookAt pos
                    ]

        let drawVectors (viewPlan : MViewPlan)(near:IMod<float>) (length:IMod<float>) (thickness:IMod<float>) (cam:IMod<CameraView>) =

            let lookAtVec = 
                adaptive {
                    let! trafo = viewPlan.roverTrafo
                    return (trafo.Forward.UpperLeftM33().C0)
                }
            let rightVec = 
                adaptive {
                    let! trafo = viewPlan.roverTrafo
                    return (trafo.Forward.UpperLeftM33().C1)
                }
            let tiltVec = 
                adaptive {
                    let! trafo = viewPlan.roverTrafo
                    return (trafo.Forward.UpperLeftM33().C2)
                }

            //let size = Sg.computeInvariantScale cam near viewPlan.position length (Mod.constant 60.0)

            let marker : ReferenceSystemApp.Sg.MarkerStyle = {
                position  = viewPlan.position
                direction = Mod.constant V3d.NaN
                color     = Mod.constant C4b.Black
                size      = length //size
                thickness = thickness
                hasArrow  = Mod.constant true
                text      = Mod.constant None
                fix       = Mod.constant false
            }

            let lookAt = { marker with direction = lookAtVec; color = (Mod.constant C4b.Yellow)}
            let right  = { marker with direction = rightVec;  color = (Mod.constant C4b.Red)}
            let tilt   = { marker with direction = tiltVec;   color = (Mod.constant C4b.Red)}

            Sg.ofList [
                    lookAt |> ReferenceSystemApp.Sg.directionMarker near cam
                    right  |> ReferenceSystemApp.Sg.directionMarker near cam
                    tilt   |> ReferenceSystemApp.Sg.directionMarker near cam                    
                ] |> Sg.onOff viewPlan.isVisible
        
        let drawAxis (axis:MAxis) (cam:IMod<CameraView>) (thickness:IMod<float>) (trafo:IMod<Trafo3d>) =
            let start = 
                alist{
                    let! s = axis.startPoint
                    let! trafo = trafo
                    yield trafo.Forward.TransformPos s
                    }
            let endp = 
                alist{
                    let! e = axis.endPoint
                    let! trafo = trafo
                    yield trafo.Forward.TransformPos e
                    }
            let axisLine = 
                alist {
                    let! s = axis.startPoint
                    let! e = axis.endPoint
                    let! trafo = trafo
                    yield trafo.Forward.TransformPos e
                    yield trafo.Forward.TransformPos s
                } 
            let color = Mod.constant C4b.Red
            Sg.ofList [
                    Sg.dots start (Mod.constant 3.0) color  
                    Sg.dots endp  (Mod.constant 3.0) color
                    PRo3D.Sg.lines axisLine color thickness trafo
                ] 
        
        let drawInstruments (instruments:alist<MInstrument>) (viewPlan:MViewPlan) (near:IMod<float>) (length:IMod<float>) (thickness:IMod<float>) (cam:IMod<CameraView>) =
            alist {
                let! trafo = viewPlan.roverTrafo
                for i in instruments do
                    let camPosTrans = 
                        adaptive{ //TODO LF refactor: don't need adaptive block if there is only 1 value. 1 value -> mod.map, 2 values -> mod.map2, more values -> adaptive
                            let! p = i.extrinsics.position
                            return trafo.Forward.TransformPos p
                            }
                    let camLookAtTrans = 
                        adaptive{
                            let! p = i.extrinsics.camLookAt
                            return trafo.Forward.TransformDir p
                            }
                    let camUpTrans = 
                        adaptive{
                            let! p = i.extrinsics.camUp
                            return trafo.Forward.TransformDir p
                            }
                    let! selInst = viewPlan.selectedInstrument
                    match selInst with
                        | Some s -> 
                            let! sid = s.id
                            let! id = i.id
                            //let size = Sg.computeInvariantScale cam near viewPlan.position length (Mod.constant 60.0)

                            let marker : ReferenceSystemApp.Sg.MarkerStyle = {
                                position  = camPosTrans
                                direction = Mod.constant V3d.NaN
                                color     = Mod.constant C4b.Black
                                size      = length //size
                                thickness = thickness
                                hasArrow  = Mod.constant true
                                text      = Mod.constant None
                                fix       = Mod.constant false
                            }

                            let up = { marker with direction = camUpTrans; color = (Mod.constant C4b.Red)}

                            match sid = id with
                            | true ->
                              let lookAt = { marker with direction = camLookAtTrans; color = (Mod.constant C4b.Cyan)}
                              yield Sg.ofList [
                                        ReferenceSystemApp.Sg.point camPosTrans (Mod.constant C4b.Cyan) cam // position
                                        lookAt |> ReferenceSystemApp.Sg.directionMarker near cam 
                                        up     |> ReferenceSystemApp.Sg.directionMarker near cam
                                    ] 
                            | false -> 
                              let lookAt = { marker with direction = camLookAtTrans; color = (Mod.constant C4b.Blue)}                              
                              yield Sg.ofList [
                                        lookAt |> ReferenceSystemApp.Sg.directionMarker near cam 
                                        up     |> ReferenceSystemApp.Sg.directionMarker near cam
                                    ] 
                        | None -> yield Sg.ofList []
                }
                          
        let drawWheels (vp:MViewPlan) (cam:IMod<CameraView>) =
            alist {
                let! wheels = vp.rover.wheelPositions
                let! trafo = vp.roverTrafo
                for w in wheels do
                    let wheelPos = 
                        adaptive{
                            return trafo.Forward.TransformPos w
                            }
                    yield Sg.ofList [
                            ReferenceSystemApp.Sg.point wheelPos (Mod.constant C4b.White) cam ]   
                }
                
        let drawSelectionGeometry (vp:MViewPlan) (near:IMod<float>) (length:IMod<float>) (thickness:IMod<float>) (cam:IMod<CameraView>) (roverM:MRoverModel)= 
            let wheels = drawWheels vp cam
                            |> ASet.ofAList 
                            |> Sg.set
            let axes = vp.rover.axes |> RoverApp.mapTolist
            let sgAxes = 
                alist {
                    for a in axes do
                        yield drawAxis a cam thickness vp.roverTrafo
                    } |> ASet.ofAList |> Sg.set
            let instruments = vp.rover.instruments |> RoverApp.mapTolist
            let sgInstruments = 
                 drawInstruments instruments vp near length thickness cam
                 |> ASet.ofAList 
                 |> Sg.set
            Sg.ofList [
                wheels
                sgAxes
                sgInstruments
            ]         

        let view<'ma> (mbigConfig : 'ma) (minnerConfig : ReferenceSystemApp.MInnerConfig<'ma>) (model:MViewPlanModel) (cam:IMod<CameraView>) : ISg<Action> =
                       
            let length    = minnerConfig.getArrowLength    mbigConfig
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig

            let viewPlans =
              aset {
                for vp in model.viewPlans do 
                 yield drawInitPositions vp cam
                 let! showVectors = vp.isVisible
                 if showVectors then
                   yield drawVectors vp near length thickness cam
                   let! selected = model.selectedViewPlan
                   let! id = vp.id
                   let! selDrawing = 
                     match selected with
                       | Some s -> s.id |> Mod.map (fun g -> if g = id then (drawSelectionGeometry vp near length thickness cam model.roverModel) else Sg.ofList [])
                       | None -> Mod.constant (Sg.ofList [])
                   yield selDrawing
                 else
                   yield Sg.ofList []
              } |> Sg.set
                
            Sg.ofList [
                viewPlans
                drawWorking model
            ]

    

    module UI =

        let viewHeader (m:MViewPlan) (id:Guid) toggleMap = 
            [
                Incremental.text m.name; text " "

                i [clazz "home icon";                                                
                    onClick (fun _ -> FlyToViewPlan id)
                ][] |> UI.wrapToolTip "FlyTo"                                                                                

                Incremental.i toggleMap AList.empty |> UI.wrapToolTip "Toggle Arrows"                                                                     

                i [clazz "Remove icon red";                                             
                    onClick (fun _ -> RemoveViewPlan id)
                ][] |> UI.wrapToolTip "Remove"                                         
            ]    

        let viewViewPlans (m:MViewPlanModel) = 
            let itemAttributes =
                amap {
                    yield clazz "ui divided list inverted segment"
                    yield style "overflow-y : visible"
                } |> AttributeMap.ofAMap
            Incremental.div itemAttributes (
                alist { 
                    yield Incremental.i itemAttributes AList.empty
                    let! selected = m.selectedViewPlan
                    let viewPlans = m.viewPlans |> ASet.toAList
                    for vp in viewPlans do
                        let! vpid = vp.id
                        let! color =
                            match selected with
                              | Some sel -> 
                                sel.id |> Mod.map (fun g -> if g = vpid then C4b.VRVisGreen else C4b.White)
                              | None -> Mod.constant C4b.White
                                                                 
                        let bgc = color |> Html.ofC4b |> sprintf "color: %s"

                        let toggleIcon =
                            adaptive {
                                let! toggle = vp.isVisible
                                return if toggle then "unhide icon" else "hide icon"
                            }

                        let toggleMap = 
                            amap {
                                let! toggleIcon = toggleIcon
                                yield clazz toggleIcon
                                yield onClick (fun _ -> IsVisible vpid)
                            } |> AttributeMap.ofAMap  


                        yield div [clazz "item"] [
                            Incremental.i itemAttributes AList.empty
                            i [
                                clazz "large cube middle aligned icon"; 
                                style bgc
                                onClick (fun _ -> SelectViewPlan vpid)
                            ][]
                            div [clazz "content"] [
                                Incremental.i itemAttributes AList.empty
                                div [clazz "header"] (
                                    viewHeader vp vpid toggleMap
                                )      
                            ]
                        ]
                }
            )
        
        let focalGui (i : MInstrument) =
            let nodes =
                alist {
                    let! id = i.id
                    let! focals = i.calibratedFocalLengths
                    if focals.Length > 1 then                    
                        yield Numeric.view' [NumericInputType.Slider; NumericInputType.InputBox] i.focal |> UI.map (fun x -> ChangeFocal (id,x))
                    else
                        yield Incremental.text (i.focal.value |> Mod.map string)
                }
            Incremental.div AttributeMap.Empty nodes

        let instrumentProperties(i : MInstrument) =
            let sensor = i.intrinsics |> Mod.map(fun x -> sprintf "%d X %d" x.horizontalResolution x.verticalResolution)

            require GuiEx.semui (
                Html.table [                
                    Html.row "Sensor (px):"  [ Incremental.text sensor ]
                    Html.row "Focal (mm):"   [ focalGui i ]                
                ]
            )

        let viewInstrumentProperties (m : MViewPlan) = 
            m.selectedInstrument 
                |> Mod.map(fun x ->
                    match x with 
                      | Some i -> instrumentProperties i
                      | None ->   div[][])

        //let instrumentsDd (r:MRover) (m : MViewPlan) = 
        //    UI.dropDown'' (r.instruments |> RoverApp.mapTolist) m.selectedInstrument (fun x -> SelectInstrument (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) )   
        
        let instrumentsDd (r:MRover) (m : MViewPlan) = 
            UI.dropDown'' (r.instruments |> RoverApp.mapTolist) m.selectedInstrument (fun x -> SelectInstrument (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) )   

        let viewAxesList (r : MRover) (m : MViewPlan) =
            alist {
                let! selectedI = m.selectedInstrument
                match selectedI with
                    | Some i ->
                        for axis in (r.axes |> RoverApp.mapTolist) do
                            yield div[][Incremental.text axis.id; text "(deg)"]
                            //yield Numeric.view' [NumericInputType.Slider; NumericInputType.InputBox] axis.angle |> UI.map (fun x -> ChangeAngle (axis.id |> Mod.force,x))
                            let! id = axis.id
                            let! value = axis.angle.value
                            let! max = axis.angle.max
                            let! min = axis.angle.min
                            //yield Numeric.view' [InputBox] axis.angle |> UI.map (fun x -> ChangeAngle (id,x))
                            yield PRo3DNumeric.view' [NumericInputType.Slider; NumericInputType.InputBox] axis.angle |> UI.map (fun x -> ChangeAngle (id,x))
                            //if (value <= max) && (value >= min) then                    
                            //    yield Numeric.view' [NumericInputType.Slider; NumericInputType.InputBox] axis.angle |> UI.map (fun x -> ChangeAngle (id,x))
                            //else
                            //    yield Incremental.text (axis.angle.value |> Mod.map string)
                            //yield br[]
                            //yield Incremental.text (axis.startPoint |> Mod.map (sprintf "%A"))
                            yield div[][text "["; Incremental.text (axis.angle.min |> Mod.map string); text ";"; Incremental.text (axis.angle.max |> Mod.map string); text "]"]
                            yield br[]
                    | None -> yield div[][]

            }

        let viewRoverProperties' (r : MRover) (m : MViewPlan) =
            require GuiEx.semui (
                Html.table [
                     Html.row "Change VPName:"[ Html.SemUi.textBox m.name SetVPName ]
                     Html.row "Name:"       [ Incremental.text r.id ]
                     Html.row "Instrument:" [ 
                        instrumentsDd r m 
                        Incremental.div AttributeMap.empty (AList.ofModSingle (viewInstrumentProperties m))                    
                     ]
                     Html.row "Axes:" [   
                        Incremental.div AttributeMap.empty (viewAxesList r m)
                     ]
                ]
            )

        let viewSelectRover (m : MRoverModel) : DomNode<RoverApp.Action> =
            Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large Rocket icon"][] ]
                Html.Layout.boxH [ UI.dropDown'' (RoverApp.roversList m)  m.selectedRover (fun x -> RoverApp.Action.SelectRover (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) ) ]                
            ]


        let viewRoverProperties lifter (model : MViewPlanModel) = 
            model.selectedViewPlan 
                |> Mod.map(fun x ->
                    match x with 
                      | Some x -> viewRoverProperties' x.rover x |> UI.map lifter
                      | None ->   div[][] |> UI.map lifter)

        let viewViewPlansUI (model:MViewPlanModel) = 
             //let listAttr = AttributeMap.ofList [style "overflow-y : visible"]
             require GuiEx.semui (
                viewViewPlans model
            ) 

       