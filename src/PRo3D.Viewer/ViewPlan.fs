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
open OpenCvSharp

open PRo3d
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Groups
open Aardvark.UI.Primitives


module ViewPlan = 
    open ViewPlanner

    type Action =
        | AddPoint  of V3d*ReferenceSystem*hmap<string, ConcreteKdIntersectionTree>*SurfaceModel 
        | SelectViewPlan of Guid
        | FlyToViewPlan of Guid
        | IsVisible of Guid
        | RemoveViewPlan of Guid
    
    let loadRoverData (model : ViewPlanModel) (path : Option<string>) =
        match path with
            | Some p ->
                let directory = @".\InstrumentStuff" 
                let errorCode = IPWrapper.Init(directory, directory)
                printfn "%A" errorCode

                let names = RoverProvider.platformNames()    
                printfn "%A" names

                let roverData =
                    names 
                      |> Array.map RoverProvider.initRover 
                      |> List.ofArray 

                let rovs = roverData|> List.map(fun (r,_) -> r.id, r) |> HMap.ofList
                let plats = roverData|> List.map(fun (r,p) -> r.id, p) |> HMap.ofList

                printfn "found rover(s): %d" (rovs |> HMap.count)
        
                { model with roverModel = { model.roverModel with rovers = rovs; platforms = plats } }
            | _ -> model

    let trafoFromTranslatedBase (position:V3d) (tilt:V3d) (forward:V3d) (right:V3d) : Trafo3d =  
        let rotTrafo =
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

    let hitF (p : V3d) (dir:V3d) (cache : hmap<string, ConcreteKdIntersectionTree>)(surfaceModel:SurfaceModel) : V3d option = 
        let mutable cache = cache
        let ray = FastRay3d(p, dir)  
                 
        match SurfaceApp.doKdTreeIntersection surfaceModel Trafo3d.Identity ray cache with
            | Some (t,surf), c ->                             
                ray.Ray.GetPointOnRay t |> Some
            | None, c ->
                None
        
    let initialPlacementTrafo (position:V3d) (lookAt:V3d) (up:V3d) : Trafo3d =
        let forward = (lookAt - position).Normalized
        let n = V3d.Cross(forward, up.Normalized).Normalized
        let tilt = V3d.Cross(n, forward).Normalized
        let right = V3d.Cross(tilt, forward).Normalized

        trafoFromTranslatedBase position tilt forward right

    let calculateSurfaceContactPoints (roverWheel:V3d) (cache : hmap<string, ConcreteKdIntersectionTree>) (trafo:Trafo3d) (surfaceModel:SurfaceModel) : V3d option =
        let tilt = trafo.Forward.UpperLeftM33().C2;
        let wheelT = trafo.Forward.TransposedTransformPos(roverWheel)
        let origin = wheelT + 5000.0 * tilt //move up ray origin
        hitF origin (-tilt) cache surfaceModel

    let nearestPoint (x:V3d) (plane:Plane3d) =
        let p = plane.Point;
        (x - plane.Normal.Dot(x - p) * plane.Normal);
       
    let fitLineLeastSquares(points:list<V3d>) =
        let centroid = points.ComputeCentroid()
        let normalizedPositions = points |> List.map(fun p -> p - centroid)
        let xx = normalizedPositions |> List.map(fun p -> p.X * p.X) |> List.sum
        let xy = normalizedPositions |> List.map(fun p -> p.X * p.Y) |> List.sum
        let xz = normalizedPositions |> List.map(fun p -> p.X * p.Z) |> List.sum
        let yy = normalizedPositions |> List.map(fun p -> p.Y * p.Y) |> List.sum
        let yz = normalizedPositions |> List.map(fun p -> p.Y * p.Z) |> List.sum
        let zz = normalizedPositions |> List.map(fun p -> p.Z * p.Z) |> List.sum

        //let covariance = new Matrix<double>([xx; xy; xz; xy; yy; yz; xz, yz, zz], 3, 3)

        //OpenCvSharp.C

        points

        //let covariance = new Matrix<double>({xx, xy, xz, xy, yy, yz, xz, yz, zz}, 3, 3);
        //let c = covariance.get
        //    Matrix<double> eigenVectors;
        //    Vector<double> eigenValues;
        //    covariance.ComputeEigenValues(out eigenVectors, out eigenValues);

        //    //eigenvector with largest eigenvalue is best fit line
        //    var n = new V3d(eigenVectors.Data, 0);

        //    var result = new Line3d(centroid - n, centroid + n);
        //    return result;
        
    let calculateRoverPlacement (working:list<V3d>) (wheels:list<V3d>) : Trafo3d = 
        let forward = (working.[1] - working.[0]).Normalized
        // projects intersection points along the lookAt vector onto a plane
        let plane1 = new Plane3d(forward, working.[1]);
        let planeIntersectionPoints = wheels |> List.map(fun x -> nearestPoint x plane1)

        let w = wheels.ComputeCentroid()
        // line fitting with the projected points to get right vector
        let rightVecLine = fitLineLeastSquares planeIntersectionPoints

        //let newRightVec = rightVecLine..Normalized
        //let newTiltVec = V3d.Cross(forward, newRightVec).Normalized
        //let sign = V3d.Dot(newTiltVec, up)

        Trafo3d.Identity
                        //if (sign <= 0)
                        //{
                        //    newTiltVec = newTiltVec.Negated;
                        //    newRightVec = newRightVec.Negated;
                        //}

    let createViewPlan (working:list<V3d>) (rover:Rover) (ref:ReferenceSystem) (camState:CameraControllerState) (kdTree:hmap<string, ConcreteKdIntersectionTree>) (surfaceModel:SurfaceModel) =
        // initial rover placement trafo
        let initTrafo = initialPlacementTrafo working.[0] working.[1] ref.up.value
        // ray casting for surface intersection with rover wheels
        //let surfaceIntersectionPoints = 
        //    rover.wheelPositions 
        //        |> List.map(fun x -> calculateSurfaceContactPoints x kdTree initTrafo) 
        //        |> List.choose(fun y -> y)

        // do rover placement stuff...
        //let placementTrafo =
        //    match (surfaceIntersectionPoints.Length > 3) with
        //        | true -> calculateRoverPlacement working surfaceIntersectionPoints
        //        | _ -> initTrafo
        // ...
        let newViewPlan = {
            id = Guid.NewGuid()
            position = working.[0]
            lookAt = working.[1]
            viewerState = camState
            vectorsVisible = true
            rover = rover
            roverTrafo = initTrafo
            isVisible = true
            }
        newViewPlan

    let removeViewPlan (vps:hset<ViewPlan>) (id:Guid) = 
        vps |> HSet.filter (fun x -> x.id <> id)   

    let update (model : ViewPlanModel) (camState:CameraControllerState) (action : Action) =
        printfn "action: %A" action
        match action with
            | AddPoint (p,ref,kdTree,surfaceModel) ->
                match model.roverModel.selectedRover with
                    | Some r -> match model.working.IsEmpty with
                                    | true -> {model with working = [p]} // first point (position)
                                    | false -> // second point (lookAt)
                                        let w = List.append model.working [p]
                                        let wp = createViewPlan w r ref camState kdTree surfaceModel
                                        { model with viewPlans = HSet.add wp model.viewPlans; working = List.Empty }
                    | None -> model
            | SelectViewPlan id ->
                let vp = model.viewPlans |> Seq.tryFind(fun x -> x.id = id)
                let vp =
                    match vp, model.selectedViewPlan with
                        | Some a, Some b -> if a.id = b.id then None else Some a
                        | Some a, None   -> Some a
                        | None, _        -> None

                { model with selectedViewPlan = vp }
            | FlyToViewPlan id -> model
            | IsVisible id -> 
                { model with viewPlans = model.viewPlans |> HSet.map(fun x -> if x.id = id then { x with isVisible   = not x.isVisible } else x) }
            | RemoveViewPlan id ->
                { model with viewPlans = model.viewPlans |> HSet.filter (fun x -> x.id <> id) }
            //| _ -> model

    module Sg =

        //let coneISg color radius size trafo =  
        //    Sg.cone 30 color radius size
        //            |> Sg.noEvents
        //            |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
        //                do! Aardvark.Opc.Shader.stableTrafo
        //                do! DefaultSurfaces.vertexColor
        //                do! DefaultSurfaces.simpleLighting
        //            }
        //            |> Sg.trafo(trafo)
                        
        let drawWorking (model:MViewPlanModel) (cam:IMod<CameraView>) =
           
            let point0 =
                aset{
                let! w = model.working
                match w.IsEmpty with
                    | false -> yield PRo3D.Sg.dots (AList.single w.[0]) (Mod.constant C4b.Green) cam
                    | true -> ()
                } |> Sg.set

            let point1 =
                aset{
                let! w = model.working
                match w.IsEmpty with
                    | false -> match w.Length > 1 with
                                | true -> yield PRo3D.Sg.dots (AList.single w.[1]) (Mod.constant C4b.Yellow) cam
                                | false -> ()
                    | true -> ()
                } |> Sg.set

            Sg.ofList [point0;point1]
            

        let drawViewPlan (viewPlan : MViewPlan)(near:IMod<float>) (length:IMod<float>) (thickness:IMod<float>) (cam:IMod<CameraView>) =

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

            Sg.ofList [
                    ReferenceSystemApp.Sg.point viewPlan.position (Mod.constant C4b.Green) cam // position
                    ReferenceSystemApp.Sg.point viewPlan.lookAt (Mod.constant C4b.Yellow) cam // lookAt pos
                    ReferenceSystemApp.Sg.directionMarker viewPlan.position lookAtVec (Mod.constant C4b.Yellow) near length thickness cam // lookAt vec
                    ReferenceSystemApp.Sg.directionMarker viewPlan.position rightVec (Mod.constant C4b.Blue) near length thickness cam // right vec
                    ReferenceSystemApp.Sg.directionMarker viewPlan.position tiltVec (Mod.constant C4b.Red) near length thickness cam // tilt vec
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
                    yield e
                    yield s
                } 
            let color = Mod.constant C4b.Red
            Sg.ofList [
                    Sg.dots start color cam 
                    Sg.dots endp color cam 
                    PRo3D.Sg.lines axisLine color thickness trafo (Mod.constant Guid.Empty)
                ] 

        let drawSelectionGeometry (vp:MViewPlan) (thickness:IMod<float>) (cam:IMod<CameraView>) = 
            let wheels = vp.rover.wheelPositions
                            |> Mod.force
                            |> AList.ofList
            let axes = vp.rover.axes |> RoverApp.mapTolist
            let sgAxes = 
                alist {
                    for a in axes do
                        yield drawAxis a cam thickness vp.roverTrafo
                    } |> ASet.ofAList |> Sg.set
            Sg.ofList [
                Sg.dots wheels (Mod.constant C4b.White) cam
                sgAxes
            ]         

        let view<'ma> (mbigConfig : 'ma) (minnerConfig : ReferenceSystemApp.MInnerConfig<'ma>) (model:MViewPlanModel) (cam:IMod<CameraView>) : ISg<Action> =
                       
            let length    = minnerConfig.getArrowLength    mbigConfig
            let thickness = minnerConfig.getArrowThickness mbigConfig
            let near      = minnerConfig.getNearDistance   mbigConfig

            let viewPlans =
                aset {
                    for vp in model.viewPlans do             
                       yield drawViewPlan vp near length thickness cam
                       let selected = model.selectedViewPlan
                       let id = vp.id
                       yield match (selected |> Mod.force) with
                                    | Some s -> if ((s.id |> Mod.force) = (id |> Mod.force)) then 
                                                    drawSelectionGeometry s thickness cam
                                                else Sg.ofList []
                                    | None -> Sg.ofList []
                } |> Sg.set
                
            Sg.ofList [
                viewPlans
                drawWorking model cam
            ]

    

    module UI =

        let viewHeader (m:MViewPlan) toggleMap =     
                [
                    Incremental.text m.rover.id; text " "

                    i [clazz "home icon";                                                
                        onClick (fun _ -> FlyToViewPlan (m.id |> Mod.force))
                    ][] |> UI.wrapToolTip "FlyTo"                                                                                

                    Incremental.i toggleMap AList.empty |> UI.wrapToolTip "Toggle Arrows"                                                                     

                    i [clazz "Remove icon red";                                             
                        onClick (fun _ -> RemoveViewPlan (m.id |> Mod.force))
                    ][] |> UI.wrapToolTip "Remove"                                         
                ]    

        let viewViewPlans (m:MViewPlanModel) = 
            let listAttr = AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : visible"] //;max-height: 30%"
            Incremental.div listAttr (
                alist { 
                    let! selected = m.selectedViewPlan
                    let viewPlans = m.viewPlans |> ASet.toAList
                    for vp in viewPlans do

                        let! color =
                            match selected with
                              | Some sel -> 
                                sel.id |> Mod.map (fun g -> if g = (vp.id |> Mod.force) then C4b.VRVisGreen else C4b.White)
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
                                yield onClick (fun _ -> IsVisible (vp.id |> Mod.force))
                            } |> AttributeMap.ofAMap  


                        yield div [clazz "item"] [
                            i [
                                clazz "large cube middle aligned icon"; 
                                style bgc
                                onClick (fun _ -> SelectViewPlan (vp.id |> Mod.force))
                            ][]
                            div [clazz "content"] [
                                div [clazz "header"] (
                                    viewHeader vp toggleMap
                                )      
                            ]
                        ]
                }
            )

        let viewSelectRover (m : MRoverModel) : DomNode<RoverApp.Action> =
            Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large Rocket icon"][] ]
                Html.Layout.boxH [ UI.dropDown'' (RoverApp.roversList m)  m.selectedRover (fun x -> RoverApp.Action.SelectRover (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) ) ]                
            //Static.div[][
            //    RoverApp.viewRovers m
            //    br[]
            //    UI.dropDown'' (RoverApp.roversList m)  m.selectedRover (fun x -> RoverApp.Action.SelectRover (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) )
            ]

        let viewRoverProperties lifter (model : MViewPlanModel) = 
            model.selectedViewPlan 
                |> Mod.map(fun x ->
                    match x with 
                      | Some x -> RoverApp.viewRoverProperties' x.rover model.roverModel |> UI.map lifter
                      | None ->   div[][] |> UI.map lifter)

        let viewViewPlansUI (model:MViewPlanModel) = 
             //let listAttr = AttributeMap.ofList [style "overflow-y : visible"]
             require Html.semui (
                viewViewPlans model
            )  

        //let viewPlanUI (model:MViewPlanModel) =
        //    div[][                            
        //        yield GuiEx.accordion "ViewPlans" "Cubes" true [ 
        //            viewViewPlans |> UI.map GroupsMessage
        //            viewSurfacesGroups model ]
        //        yield GuiEx.accordion "Properties" "Content" true [
        //                    Incremental.div AttributeMap.empty (alist{ let! item = item2
        //                                                               let! i = item
        //                                                               yield i})
        //        ]
        //    ]