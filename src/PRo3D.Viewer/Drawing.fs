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

    let finishAndAppend (model : DrawingModel) = 
        let anns = match model.working with
                            | Some w -> model.annotations |> PList.prepend w
                            | None -> model.annotations
        { model with working = None; annotations = anns; pendingIntersections = ThreadPool.empty }

    let addPoint p model =
        let working, newSegment = 
            match model.working with
                    | Some w ->     
                        let newModel = { w with points = w.points |> PList.append p }
                        Log.line "working contains %d points" newModel.points.Count
                        match PList.tryAt (PList.count w.points - 1) w.points with
                            | None -> 
                                newModel, None
                            | Some a -> 
                                let segmentIndex = PList.count newModel.segments
                                { newModel with segments = PList.append (PList.ofList [a;p]) newModel.segments }, Some (a,p,segmentIndex)
                        
                    | None ->                                     
                        { 
                            (Annotation.make model.projection model.geometry model.semantic)  //add annotation states
                                with points = PList.ofList [p]; modelTrafo = Trafo3d.Translation p
                        }, None 

        let model = { model with working = Some working }

        //let model = match (working.geometry, (working.points |> PList.count)) with
        //                | Geometry.Point, 1 -> model |> finishAndAppend
        //                | Geometry.Line, 2 -> model |> finishAndAppend
        //                | _ -> model

        model, newSegment

    let addNewSegment samplePoint model (p0:V3d,p1:V3d,segmentIndex : int) =
        let sampleCount = 20
        let dir = p1-p0
        let samples = 
            [ for s in 0 .. 20 do
                let p = p0 + dir * (float s / float sampleCount) // world space
                yield async {
                    do! Async.SwitchToNewThread()
                    match samplePoint p with
                        | None -> return None
                        | Some projectedPoint -> // projected point in world space
                            return Some projectedPoint
                } |> Async.StartAsTask
            ]

        let mutable result = []

        let computation = 
            proclist {
                for s in samples do
                    let! a = Proc.Await s
                    match a with
                        | None -> ()
                        | Some s -> 
                            result <-  result @ [s]
                            yield SetSegment(segmentIndex, result)
            } 
        
        let threadPool = ThreadPool.add (Guid.NewGuid().ToString()) computation model.pendingIntersections
        { model with pendingIntersections = threadPool }


    let update (model : DrawingModel) (act : Action) =
        match (act, model.draw) with
            | KeyDown Keys.LeftCtrl, _ ->                     
                    { model with draw = true }
            | KeyUp Keys.LeftCtrl, _ -> 
                    {model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPoint p, true ->                     
                    addPoint p model |> fst               
            | AddPointAdv (point, samplePoint), true ->
                    let model, newSegment = addPoint point model                    
                    
                    match newSegment with
                        | None         -> model
                        | Some segment -> addNewSegment samplePoint model segment

            | SetSegment(segmentIndex,samples), true ->
                match model.working with
                    | None -> model
                    | Some w -> 
                        let newSegment = PList.ofList samples
                        { model with working = Some { w with segments = PList.setAt segmentIndex newSegment w.segments } }

            | KeyDown Aardvark.Application.Keys.Enter, _ -> 
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
            | KeyDown Aardvark.Application.Keys.D0, _ -> 
                    {model with semantic = Semantic.Horizon0 }               
            | KeyDown Aardvark.Application.Keys.D1, _ -> 
                    {model with semantic = Semantic.Horizon1 }               
            | KeyDown Aardvark.Application.Keys.D2, _ -> 
                    {model with semantic = Semantic.Horizon2 }               
            | KeyDown Aardvark.Application.Keys.D3, _ -> 
                    {model with semantic = Semantic.Horizon3 }               
            | KeyDown Aardvark.Application.Keys.D4, _ -> 
                    {model with semantic = Semantic.Horizon4 }               
            | SetExportPath s, _ ->
                    { model with exportPath = s }
            | Export, _ ->
                    //let path = Path.combine([model.exportPath; "drawing.json"])
                    //printf "Writing %i annotations to %s" (model.annotations |> PList.count) path
                    //let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> JsonConvert.SerializeObject
                    //Serialization.writeToFile path json 
                    
                    model
            | Save,_->
                Serialization.save "./lastMeasure" model.annotations |> ignore
                model
            | Load,_ ->
                let annotations = 
                    Serialization.loadAs<plist<PRo3D.Annotation>> "./lastMeasure"
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

            let azimuth = match coords.X.Sign() with
                            | -1 -> match coords.Y.Sign() with
                                        | 1 -> 270.0 + angle
                                        | _ -> 270.0 - angle 
                            | _ -> match coords.Y.Sign() with
                                        | -1 -> angle + 90.0
                                        | _ -> 90.0 - angle
                                        
            azimuth

            
  
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

    let threads (m : DrawingModel) = m.pendingIntersections

    module Sg =        

        let discISg color size trafo =         
            Sg.cone 5 color size (Mod.constant 2.1)
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
                    yield mkDotISg color (computeScale view (Mod.constant p) 5.0) (Mod.constant (Trafo3d.Translation(p)))
            } 
            |> Sg.set
            |> Sg.effect [
                    toEffect <| Aardvark.Opc.Shader.stableTrafo
                    toEffect <| DefaultSurfaces.vertexColor
                    toEffect <| DefaultSurfaces.simpleLighting
                ]
           
        let lines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>)= 
           let lines = edgeLines false points trafo           
           lines
                |> Sg.lines color                
                |> Sg.shader {
                    do! Aardvark.Opc.Shader.stableTrafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.thickLine    
                }              
                |> Sg.noEvents                
                |> Sg.trafo trafo
                |> Sg.uniform "LineWidth" width
                |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
                |> Sg.depthTest (Mod.constant DepthTestMode.None)
   
        let workingAnnotations (anno : IMod<Option<MAnnotation>>)(view : IMod<CameraView>) = 

            let points =
                alist {
                    let! anno = anno
                    match anno with
                        | Some a -> 
                            yield! AList.concat a.segments
                        | None -> ()
                }
                
            let color     = Mod.bindOption anno C4b.VRVisGreen   (fun a -> a.color) 
            let thickness = Mod.bindOption anno 1.0              (fun a -> a.thickness.value) 
            let key       = Mod.bindOption anno Guid.Empty       (fun a -> a.key) 
            let trafo     = Mod.bindOption anno Trafo3d.Identity (fun a -> a.modelTrafo) 
            
            let polyline = lines points color thickness trafo                                

            [polyline; dots points color view]         

        let finishedAnnotations (anno : MAnnotation)(view : IMod<CameraView>)(c : IMod<C4b>) =                         
            let points = 
                if (anno.segments |> AList.toList |> List.isEmpty) then
                    anno.points
                else
                    anno.segments |> AList.concat

            [lines points c anno.thickness.value anno.modelTrafo;
             dots points c view] 
            |> ASet.ofList

        let view (model:MDrawingModel) (cam:IMod<CameraView>) =        
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler
            let annoSet = ASet.ofAList model.annotations 

           // let trafo = model.offset |> Mod.map (fun d -> match d with | Some p -> Trafo3d.Translation p | None -> Trafo3d.Identity )

            let annotations =
                aset {
                    for a in annoSet do
                       // let a = { a with color = UI.mkColor a }
                       let c = UI.mkColor model a
                       yield! finishedAnnotations a cam c
                } |> Sg.set
                                
            //canvas
            [brush model.hoverPosition; annotations] @ workingAnnotations model.working cam
            |> Sg.ofList
            
