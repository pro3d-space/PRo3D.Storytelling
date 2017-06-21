namespace PRo3D

module DrawingApp =
    open Newtonsoft.Json    
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.Application
    open Aardvark.SceneGraph
    open Aardvark.Rendering.Text
    open Aardvark.VRVis

    open Aardvark.UI
    open Aardvark.UI.Primitives

    open PRo3D.Drawing
    open System.Windows.Forms

    module DipAndStrike = 

        let projectOntoPlane (x:V3d) (n:V3d) = (x - (x * n)).Normalized

        let computeAzimuth (dir:V3d) (north:V3d) (planeNormal:V3d) =
            let east = north.Cross(planeNormal)
            let eastWestPlane = new Plane3d(east, 0.0) //separates west (-) from east  (+)
            let northSouthPlane = new Plane3d(north, 0.0) //separates sout (-) from north  (+)

            let coords = new V2d(eastWestPlane.Height(dir), northSouthPlane.Height(dir))
            let angle = Fun.Atan(coords.Y / coords.X).DegreesFromRadians().Abs()

            //let nangle = match coords.X >= 0.0 with
            //                | true -> match coords.Y >= 0.0 with
            //                            | true -> 90.0 - angle
            //                            | false -> angle + 90.0
            //                | false -> match coords.Y > 0.0 with
            //                            | true -> 270.0 + angle
            //                            | false -> 270.0 - angle

            match coords.X.Sign() with
                    | -1 -> match coords.Y.Sign() with
                                | 1 -> 270.0 + angle
                                | _ -> 270.0 - angle 
                    | _ -> match coords.Y.Sign() with
                                | -1 -> angle + 90.0
                                | _ -> 90.0 - angle

        let calculateDipAndStrikeResults (points:plist<V3d>) =
            let up = V3d.OOI
            let north = V3d.IOO
            let v3dArray = points.AsList |> List.toArray // points.toArray not possible because of: int -> V3d[]
            let plane = PlaneFitting.FitPlane3dLS(v3dArray);
            let bb = new Box3d(points);

            let horP = new Plane3d(up, V3d.Zero);

             //correct plane orientation - check if normals point in same direction
            let height = horP.Height(plane.Normal);
            let planeNormal = match height.Sign() with
                                | -1 -> plane.Normal.Negated
                                | _ -> plane.Normal

            //strike
            let strike = up.Cross(planeNormal).Normalized;

            //dip vector 
            let dip = strike.Cross(planeNormal).Normalized;

            //dip plane incline .. maximum dip angle
            let v = strike.Cross(up).Normalized;

            let dns = {
                plane = plane
                dipAngle = Math.Acos(v.Dot(dip)).DegreesFromRadians()
                dipDirection = dip
                strikeDirection = strike
                dipAzimuth = computeAzimuth v north up
                strikeAzimuth = computeAzimuth strike north up
                centerOfMass = bb.Center
            }
            dns

        let dipColor (dipAngle:IMod<float>) (min:IMod<float>) (max:IMod<float>) =
            adaptive {
                let! dipAngle = dipAngle
                let! min = min
                let! max = max

                let range = new Range1d(min, max)
                let hue = (dipAngle - range.Min) / range.Size
                let hsv = HSVf((1.0 - hue) * 0.625, 1.0, 1.0)

                return hsv.ToC3f().ToC4b()
            }

    let finishAndAppend (model : DrawingModel) = 
        //if there are at least 3 points do dns calculation (debug, dev). 
        //might take long for large number of points so created on demand?

        let anns = match model.working with
                            | Some w ->               
                                let dns = 
                                    match w.points.Count with 
                                        | x when x > 2 -> Some (DipAndStrike.calculateDipAndStrikeResults w.points) //more acc. by using segments as well?
                                        | _ -> None 
                                model.annotations 
                                    |> PList.prepend { w with dnsResults = dns }
                                        
                            | None -> model.annotations
        { model with working = None; annotations = anns; pendingIntersections = ThreadPool.empty }

    let addPoint samplePoint p model =
        let working, newSegment = 
            match model.working with
                | Some w ->     
                    let annotation = { w with points = w.points |> PList.append p }
                    Log.line "working contains %d points" annotation.points.Count

                    //fetch current drawing segment (projected, polyline or polygon)
                    let result = 
                        match w.projection with
                            | Projection.Viewpoint | Projection.Sky ->                     
                                match PList.tryAt (PList.count w.points-1) w.points with
                                    | None -> 
                                        annotation, None
                                    | Some a -> 
                                        let segmentIndex = PList.count annotation.segments
                                        let newSegment = { startPoint = a; endPoint = p; points = PList.ofList [a;p] }

                                        if PRo3D.Config.useAsyncIntersections then
                                            { annotation with segments = PList.append newSegment annotation.segments }, Some (newSegment,segmentIndex)
                                        else
                                            let dir = newSegment.endPoint - newSegment.startPoint
                                            let points = 
                                                [ for s in 0 .. PRo3D.Config.sampleCount do
                                                    let p = newSegment.startPoint + dir * (float s / float PRo3D.Config.sampleCount) // world space
                                                    match samplePoint p with
                                                        | None -> ()
                                                        | Some projectedPoint ->
                                                            yield projectedPoint
                                                ]
                                            let newSegment = { startPoint = a; endPoint = p; points = PList.ofList points }
                                            { annotation with segments = PList.append newSegment annotation.segments }, None
                            | Projection.Linear ->
                                annotation, None
                            | _ -> failwith "case does not exist"
                    
                    result
                    
                        
                | None ->  //no working state, start new working annotation
                    { 
                        //annotation states should be immutable after creation
                        (Annotation.make model.projection model.geometry model.semantic)  
                            with points = PList.ofList [p]; modelTrafo = Trafo3d.Translation p
                    }, None

        let model = { model with working = Some working }

        match (working.geometry, (working.points |> PList.count)) with
            | Geometry.Point, 1 -> 
                model |> finishAndAppend, None
            | Geometry.Line, 2 -> 
                model |> finishAndAppend, None
            | _ -> 
                model, newSegment // returns current segment for async computations outside

    let addNewSegment samplePoint model (newSegment : Segment, segmentIndex : int) =
        let dir = newSegment.endPoint - newSegment.startPoint
        let id = Guid.NewGuid() |> string

        let computation = 
            proclist {
                let mutable r = []
                let result = MVar.empty()
                let task = 
                    async {
                        do! Async.SwitchToNewThread()
                        let r = 
                            [ for s in 0 .. PRo3D.Config.sampleCount do
                                let p = newSegment.startPoint + dir * (float s / float PRo3D.Config.sampleCount) // world space
                                match samplePoint p with
                                    | None -> ()
                                    | Some projectedPoint -> // projected point in world space
                                        r <- r @ [projectedPoint]
                                        MVar.put result (Choice1Of2 r)
                                        yield projectedPoint
                            ]
                        MVar.put result (Choice2Of2 ())
                    } |> Async.Start

                let rec doIt () =
                    proclist {
                         let! r = Proc.Await (MVar.takeAsync result)
                         match r with
                            | Choice1Of2 r -> 
                                printfn "mked it: %A" r
                                let segment = { newSegment with points = PList.ofList r}
                                yield SetSegment(segmentIndex,segment)
                                yield! doIt()
                            | Choice2Of2 _ -> ()
                    }

                yield! doIt()
            } 
        
        let pool = 
            if model.pendingIntersections.store.ContainsKey id then 
                ThreadPool.remove id model.pendingIntersections
            else 
                model.pendingIntersections
        { model with pendingIntersections = ThreadPool.add id computation pool }

    let update (w : Form) (model : DrawingModel) (act : Action) =
        match (act, model.draw) with
            | StartDrawing, _ ->                     
                    { model with draw = true }
            | StopDrawing, _ -> 
                    {model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPoint(p,samplePoint), true ->                     
                    addPoint samplePoint p model |> fst               
            | AddPointAdv (point, samplePoint), true ->
                    let model, newSegment = addPoint samplePoint point model
                    
                    match newSegment with
                        | None         -> model
                        | Some segment -> addNewSegment samplePoint model segment

            | SetSegment(segmentIndex,segment), _ ->
                match model.working with
                    | None -> model
                    | Some w ->                         
                        { model with working = Some { w with segments = PList.setAt segmentIndex segment w.segments } }

            //| KeyDown Aardvark.Application.Keys.Enter, _ -> 
            //        model |> finishAndAppend
            | Finish, _ -> 
                    model |> finishAndAppend
            | Exit, _ -> 
                    { model with hoverPosition = None }
            | SetSemantic mode, _ ->
                    let model =
                        match mode with
                            | Semantic.GrainSize -> { model with geometry = Geometry.Line }
                            | _ -> model

                    {model with semantic = mode }
            | SetGeometry mode, _ ->
                    { model with geometry = mode }
            | SetProjection mode, _ ->
                    { model with projection = mode }                  
            | SetExportPath s, _ ->
                    { model with exportPath = s }
            | Export, _ ->
                    //let path = Path.combine([model.exportPath; "drawing.json"])
                    //printf "Writing %i annotations to %s" (model.annotations |> PList.count) path
                    //let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> JsonConvert.SerializeObject
                    //Serialization.writeToFile path json 
                    
                    model
            | Save,_->
                match Dialogs.saveDialog w with
                    | None -> model
                    | Some p -> 
                        Serialization.save "./lastMeasure" model.annotations |> ignore
                        { model with annotations = Serialization.save p model.annotations }
            | Load,_ ->                
                match Dialogs.openDialog w with
                    | None -> model
                    | Some path -> 
                        let annotations = Serialization.loadAs<plist<PRo3D.Annotation>> path
                        { model with annotations = annotations }        
            | QuickLoad,_ ->                
                let annotations = Serialization.loadAs<plist<PRo3D.Annotation>> "./lastMeasure"
                { model with annotations = annotations }        
            | Clear,_ ->
                { model with annotations = PList.empty }
            | Select id, false ->
                let selection = 
                    if (not model.selected.IsEmpty) then
                        if HSet.contains id model.selected 
                        then HSet.remove id model.selected 
                        else HSet.add id HSet.empty
                    else
                        HSet.add id model.selected                                        

                let ann = DrawingModel.tryGet model id
                        
                { model with selected = selection; selectedAnnotation = ann }  
            | Nop, _ -> model
            | _ -> model

    module UI =
        open Aardvark.Base.Incremental

        let viewAnnotationToolsHorizontal (model:MDrawingModel) =
            Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large Write icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.geometry SetGeometry ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.projection SetProjection ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.semantic SetSemantic ]
                td [][] 
            ]

        let viewAnnotationToolsSlim (model:MDrawingModel) =
            Html.SemUi.stuffStack[                                            
                Html.SemUi.dropDown model.geometry SetGeometry
                Html.SemUi.dropDown model.projection SetProjection
                Html.SemUi.dropDown model.semantic SetSemantic
            ]  

        let viewAnnotationTools (model:MDrawingModel)=  
            Html.table [                            
             //   Html.row "Text:" [Html.SemUi.textBox model.exportPath SetExportPath ]
                Html.row "Geometry:" [Html.SemUi.dropDown model.geometry SetGeometry]
                Html.row "Projections:" [Html.SemUi.dropDown model.projection SetProjection]
                Html.row "Semantic:" [Html.SemUi.dropDown model.semantic SetSemantic]
            ]    

        let viewAnnotationToolsAcc (model:MDrawingModel)=  
            Html.SemUi.accordion "Annotation Tools" "Write" true [
                viewAnnotationTools model            
            ]
        
        let mkColor (model : MDrawingModel) (a : MAnnotation) =
            let id = a.key |> Mod.force

            let color =  
                model.selected 
                    |> ASet.contains id 
                    |> Mod.bind (function x -> if x then Mod.constant C4b.Red else a.color)

            color

        let viewAnnotationImportExport =
            div [clazz "ui buttons inverted"] [
                button [clazz "ui icon button"; onMouseClick (fun _ -> Save)] [
                        i [clazz "save icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> Load)] [
                        i [clazz "folder outline icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> Clear)] [
                        i [clazz "file outline icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> QuickLoad)] [
                        i [clazz "folder icon"] [] ]
            ]

        let viewAnnotations (model:MDrawingModel) = 
                            
            Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto;max-height: 30%"    ]) (
                            
                        alist {                                                                     
                            for a in model.annotations do 
                                                                        
                                let! sem = a.semantic
                                let! c = mkColor model a

                                let bgc = sprintf "color: %s" (Html.ofC4b c)
                                    
                                yield div [clazz "item"; onClick(fun _ -> Select (a.key |> Mod.force))] [
                                        i [clazz "large Sticky Note middle aligned icon"; style bgc][]
                                        div [clazz "content"] [
                                            let header = Mod.map2 (fun x y -> sprintf "%A | %A" x y) a.geometry a.semantic
                                            let desc = Mod.map2 (fun x y -> sprintf "#Points: %A | Proj: %A" x y) (a.points |> AList.toMod |> Mod.map (fun d -> d.Count)) a.projection
                                            yield div [clazz "header"] [Incremental.text header]                                       
                                            yield div [clazz "description"] [Incremental.text desc]      
                                        ]
                                    ]
                        }     
                )
            
        let viewAnnotationsAcc (model:MDrawingModel) = 
            Html.SemUi.accordion "Annotations" "File Outline" true [
                viewAnnotations model 
            ]
 
    let threads (m : DrawingModel) = m.pendingIntersections

    module Sg =        

        let discISg color size trafo =
            Sg.cylinder 5 color size (Mod.constant 2.1)
                        |> Sg.shader {
                            do! Aardvark.Opc.Shader.stableTrafo
                            do! DefaultSurfaces.vertexColor
                            do! DefaultSurfaces.simpleLighting
                        }
                        |> Sg.noEvents
                        |> Sg.trafo(trafo) 

        let coneISg color size trafo =
            Sg.cone 5 color size (Mod.constant 0.5)
                        |> Sg.shader {
                            do! Aardvark.Opc.Shader.stableTrafo
                            do! DefaultSurfaces.vertexColor
                            do! DefaultSurfaces.simpleLighting
                        }
                        |> Sg.noEvents
                        |> Sg.trafo(trafo) 

        let computeScale (view : IMod<CameraView>)(p:IMod<V3d>)(size:float) =
            adaptive {
                let! p = p
                let! v = view
                let distV = p - v.Location
                let distF = V3d.Dot(v.Forward, distV)
                return distF * size / 800.0 //needs hfov at this point
            }

        let mkDotISg color size trafo = 
            Sg.sphere 5 color size 
               |> Sg.noEvents
               |> Sg.trafo(trafo) 

        let edgeLines (close : bool) (points : alist<V3d>) (trafo:IMod<Trafo3d>) =
            points
                |> AList.map(fun d -> trafo.GetValue().Backward.TransformPos d)
                |> AList.toMod |> Mod.map (fun l ->
                    let list = PList.toList l
                    let head = list |> List.tryHead
                    
                    match head with
                        | Some h -> if close then list @ [h] else list
                                        |> List.pairwise
                                        |> List.map (fun (a,b) -> new Line3d(a,b))
                                        |> List.toArray
                        | None -> [||]                         
            )
            
        let brush (hovered : IMod<Trafo3d option>) = 
            let trafo =
                hovered |> Mod.map (fun o -> 
                    match o with 
                        | Some t-> t
                        | None -> Trafo3d.Scale(V3d.Zero)
                )

            mkDotISg (Mod.constant C4b.Red) (Mod.constant 0.05) trafo
            |> Sg.effect [
                toEffect <| Aardvark.Opc.Shader.stableTrafo
                toEffect <| DefaultSurfaces.vertexColor
                toEffect <| DefaultSurfaces.simpleLighting
            ]
       
        let dots (points : alist<V3d>) (color : IMod<C4b>) (view : IMod<CameraView>) = 
            
            aset {
                for p in points |> ASet.ofAList do
                    let scale = computeScale view (Mod.constant p) 5.0
                    //let scale = Mod.constant 0.6
                    yield mkDotISg color scale (Mod.constant (Trafo3d.Translation(p)))
            } 
            |> Sg.set
            |> Sg.effect [
                    toEffect <| Aardvark.Opc.Shader.stableTrafo
                    toEffect <| DefaultSurfaces.vertexColor
                    toEffect <| DefaultSurfaces.simpleLighting
                ]
           
        let linePass = RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main

        let lines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>) (id : IMod<Guid>) = 
            let box = points |> AList.toMod |> Mod.map (fun d -> if d.Count > 2 then Box3d d else Box3d.Unit)

            let edges = edgeLines false points trafo

            // Cylinder3d(trafo.Forward.TransformPos center,trafo.Forward.TransformPos (center+dir*height),radius)

            //edge.P0 v.Normalized v.Length (r/2.5)                        

            
            //let cylinders = 
            //    edges 
            //        |> Mod.map (fun e ->
            //                [
            //                    for edge in e do
            //                        //yield Sg.cylinder' 3 C4b.White 
            //                        let v = edge.P1 - edge.P0
            //                        let c = Cylinder3d(edge.P0,v.Normalized, v.Length, 0.25)
            //                        yield Sg.box' C4b.White Box3d.Unit 
            //                            |> Sg.shader {
            //                                do! Aardvark.Opc.Shader.stableTrafo
            //                                do! DefaultSurfaces.vertexColor                                            
            //                            }  
            //                            |> Sg.noEvents                                         
            //                           // |> Sg.trafo trafo
            //                            |> Sg.pickable (PickShape.Cylinder c)
            //                            |> Sg.withEvents [
            //                                Sg.onMouseDownEvt (fun sceneHit -> Select (id |> Mod.force))]
            //                ] |> Sg.ofList
                              
            //                )
            
            
            //let cylinders = cylinders |> Sg.dynamic 
                    
            edges
                |> Sg.lines color
                |> Sg.shader {
                    do! Aardvark.Opc.Shader.stableTrafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.thickLine    
                }               
                |> Sg.noEvents
                |> Sg.trafo trafo
                |> Sg.uniform "LineWidth" width
                |> Sg.pass linePass
                |> Sg.depthTest (Mod.constant DepthTestMode.None)
              //  |> Sg.andAlso cylinders
                //|> Sg.pickable (PickShape.Box (box |> Mod.force))
                //|> Sg.withEvents [
                //            Sg.onMouseDownEvt (fun sceneHit -> Select (id |> Mod.force))
                //        ]
                //|> Sg.andAlso (
                //    Sg.wireBox' C4b.VRVisGreen Box3d.Invalid
                //        |> Sg.shader {
                //            do! Aardvark.Opc.Shader.stableTrafo
                //            do! DefaultSurfaces.vertexColor 
                //        }
                //        |> Sg.noEvents
                //        |> Sg.pickable (PickShape.Box (box |> Mod.force))
                //        ) |> Sg.withEvents [
                //            Sg.onMouseDownEvt (fun sceneHit -> Select (id |> Mod.force))
                //        ]
        
        let drawDns (anno : MAnnotation) =
            //let color = Mod.bindOption anno.dnsResults C4b.White (fun x -> (DipAndStrike.dipColor x.dipAngle 0.0 90.0))
            //let trafo 

            aset {
                //let! c = color
                let! dns = anno.dnsResults                
                match dns with
                        | Some x -> 
                            let color = DipAndStrike.dipColor x.dipAngle (Mod.constant 0.0) (Mod.constant 90.0)
                            let middlePoint = anno.points |> AList.toMod |> Mod.map PList.count //get item at index ?
                            yield discISg color (Mod.constant 1.0) (anno.modelTrafo)
                        | None -> ()                            
            } |> Sg.set
            
        let getPolyline (a : MAnnotation) =
            alist {                
                let! hasSegments = a.segments |> AList.toMod |> Mod.map (fun d -> d |> PList.count > 0)
                if hasSegments then
                    for segment in a.segments do
                        let! startPoint = segment.startPoint
                        let! endPoint = segment.endPoint
                        yield  startPoint
                        yield! segment.points
                        yield  endPoint
                else
                    yield! a.points
            }

        let workingAnnotation (anno : IMod<Option<MAnnotation>>)(view : IMod<CameraView>) = 

            let polyPoints =
                alist {
                    let! anno = anno
                    match anno with
                        | Some a -> yield! getPolyline a
                        | None -> ()
                }

            let points = 
                alist {
                    let! anno = anno
                    match anno with
                        | Some a -> yield! a.points
                        | None -> ()
                }
                
            let color     = Mod.bindOption anno C4b.VRVisGreen   (fun a -> a.color) 
            let thickness = Mod.bindOption anno 1.0              (fun a -> a.thickness.value) 
            let key       = Mod.bindOption anno Guid.Empty       (fun a -> a.key) 
            let trafo     = Mod.bindOption anno Trafo3d.Identity (fun a -> a.modelTrafo)          
                                   
            Sg.ofList [
                lines polyPoints color thickness trafo key //polyline
                dots points color view                  //support points
            ]         

        let finishedAnnotation (anno : MAnnotation)(view : IMod<CameraView>)(c : IMod<C4b>) =
            let points = getPolyline anno
            
           // let dns = 
            Sg.ofList [
                lines points c anno.thickness.value anno.modelTrafo anno.key;
             //   dots anno.points c view
               // drawDns anno
            ]

        let view (model:MDrawingModel) (cam:IMod<CameraView>) =
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler
            let annoSet = ASet.ofAList model.annotations 

            let annotations =
                aset {
                    for a in annoSet do                       
                       let c = UI.mkColor model a
                       yield finishedAnnotation a cam c
                } |> Sg.set
                                
            Sg.ofList [
                brush model.hoverPosition; 
                annotations
                workingAnnotation model.working cam
            ]
            
