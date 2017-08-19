namespace PRo3D

module DrawingApp =
   // open Newtonsoft.Json
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
    open System.Text
    open System.Net.WebSockets
    open System.Threading
    open PRo3D
    open System.Collections.Concurrent    

    open MBrace.FsPickler.Json   

    

    let getFinishedAnnotation (model : DrawingModel) =
        match model.working with
            | Some w ->               
                let dns = 
                    match w.points.Count with 
                        | x when x > 2 -> Some (DipAndStrike.calculateDipAndStrikeResults w.points model.coordinates.up.value) //more acc. by using segments as well?
                        | _ -> None 
                let results = Annotation.calculateAnnotationResults w model.coordinates.up.value
                Some { w with dnsResults = dns ; results = Some results }
            | None -> None

    let finishAndAppendAndSend (model : DrawingModel) (bc : BlockingCollection<string>) = 
        //if there are at least 3 points do dns calculation (debug, dev). 
        //might take long for large number of points so created on demand?

        let anns = 
            match getFinishedAnnotation model with
                | Some a -> 
                    let json = a |> JsonTypes.ofAnnotation |> Aardvark.UI.Pickler.jsonToString
                    Log.line "sending %A" a.semantic
                    bc.Add json

                    model.annotations |> PList.prepend a
                | None -> model.annotations
        
        { model with working = None; annotations = anns; pendingIntersections = ThreadPool.empty }

    let addPoint samplePoint p model surfaceName bc =
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
                        (Annotation.make model.projection model.geometry model.semantic surfaceName)  
                            with points = PList.ofList [p]; modelTrafo = Trafo3d.Translation p
                    }, None

        let model = { model with working = Some working }

        match (working.geometry, (working.points |> PList.count)) with
            | Geometry.Point, 1 -> 
                finishAndAppendAndSend model bc, None
            | Geometry.Line, 2 -> 
                finishAndAppendAndSend model bc, None
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
        
    let pickler = MBrace.FsPickler.Json.JsonSerializer(indent=true)

    let stash (model : DrawingModel) =
        { model with past = Some model; future = None }

    let update (w : Form) (bc : BlockingCollection<string>) (model : DrawingModel) (act : Action) =
        match (act, model.draw) with
            | StartDrawing, _ ->                     
                    { model with draw = true }
            | StopDrawing, _ -> 
                    { model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPointAdv (point, samplePoint, name), true ->
                    let model, newSegment = addPoint samplePoint point model name bc
                    
                    match newSegment with
                        | None         -> model
                        | Some segment -> addNewSegment samplePoint model segment
                    |> stash

            | SetSegment(segmentIndex,segment), _ ->
                match model.working with
                    | None -> model
                    | Some w ->                         
                        { model with working = Some { w with segments = PList.setAt segmentIndex segment w.segments } }

            //| KeyDown Aardvark.Application.Keys.Enter, _ -> 
            //        model |> finishAndAppend
            | Finish, _ -> 
                    finishAndAppendAndSend model bc |> stash
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
            | Send, _ ->                                        
              //  let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> pickler.PickleTyped
                Log.line "Sending %i annotations" (model.annotations |> PList.count)
                let json = model.annotations |> PList.toList |> List.map JsonTypes.ofAnnotation |> Aardvark.UI.Pickler.jsonToString
                Log.warn "sending %A" json
                bc.Add json
                model
            | Clear,_ ->
                { model with annotations = PList.empty }
            | Select id, false ->
                let selected = if (not model.selected.IsEmpty) then hset.Empty else model.selected

                let selection, ann =         
                    if (not model.selected.IsEmpty) then
                        if HSet.contains id selected 
                        then 
                            HSet.remove id selected, None 
                        else 
                            HSet.add id hset.Empty, DrawingModel.tryGet model id
                    else
                        HSet.add id selected, DrawingModel.tryGet model id
                        
                { model with selected = selection; selectedAnnotation = ann }  
            | Nop, _ -> model
            | AnnotationPropertiesMsg msg,_ ->                
                match model.selectedAnnotation with
                    | Some a ->
                        let a' = AnnotationProperties.update a msg
                        let annotations' = 
                            model.annotations.AsList |> List.updateIf (fun x -> x.key = a'.key) (fun x -> a')                                     
                        { model with selectedAnnotation = Some a'; annotations = annotations' |> PList.ofList }
                    | None -> model
                        
            | Undo, _ -> 
                match model.past with
                    | Some p -> { p with future = Some model }
                    | None -> model
            | Redo, _ ->
                match model.future with
                    | Some f -> f
                    | None -> model
            | RemoveAnnotation id, _ ->
                let toRemove = model.annotations.AsList |> List.tryFindIndex (fun d -> d.key = id)
                match toRemove with
                    | Some index ->
                        let as' = model.annotations|> PList.removeAt index
                        { model with annotations = as' } |> stash
                    | None -> model             
            | ToggleVisiblity id, _ ->
                let a = model.annotations.AsList |> List.tryFind (fun d -> d.key = id)
                let a' =
                    match a with
                        | Some k -> { k with visible = not k.visible  }
                        | None -> failwith "annotation not found, can't happen"
                { model with annotations = (model.annotations.AsList |> List.updateIf(fun d -> d.key = a'.key) (fun _ -> a')) |> PList.ofList}
            | _ -> model

    module UI =
        open Aardvark.Base.Incremental

        let viewAnnotationToolsHorizontal (model:MDrawingModel) =
            Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large Write icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.geometry SetGeometry ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.projection SetProjection ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.semantic SetSemantic ]                
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
                    |> Mod.bind (function x -> if x then Mod.constant C4b.Red else a.color.c)

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
                button [clazz "ui icon button"; onMouseClick (fun _ -> Send)] [
                        i [clazz "send icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> Undo)] [i [clazz "arrow left icon"] [] ]
                button [clazz "ui icon button"; onMouseClick (fun _ -> Redo)] [i [clazz "arrow right icon"] [] ]
            ]

        let viewAnnotations (model:MDrawingModel) = 
                            
            Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto;max-height: 30%"    ]) (
                            
                        alist {                                                                     
                            for a in model.annotations do 
                                                                        
                                let! sem = a.semantic
                                let! c = mkColor model a

                                let bgc = sprintf "color: %s" (Html.ofC4b c)
                                
                                let! visible = a.visible
                                let visibleIcon = if visible then "unhide icon" else "hide icon"
                                let select = fun _ -> Select (a.key |> Mod.force)

                                yield div [clazz "item"] [
                                        i [clazz "large Sticky Note middle aligned icon";onClick select; style bgc][]
                                        div [clazz "content"] [
                                            let header = Mod.map2 (fun x y -> sprintf "%A | %A | " x y) a.geometry a.semantic
                                            let desc = Mod.map2 (fun x y -> sprintf "#Points: %A | taken on: %A" x y) (a.points |> AList.toMod |> Mod.map (fun d -> d.Count)) a.surfaceName
                                            yield Incremental.div (AttributeMap.ofList [clazz "header"]) (
                                                alist {
                                                    yield Incremental.text header
                                                    yield i [clazz visibleIcon; onClick (fun _ -> ToggleVisiblity (a.key |> Mod.force))][] 
                                                    yield i [clazz "Remove icon"; onClick (fun _ -> RemoveAnnotation (a.key |> Mod.force))][] 
                                                    
                                                } 
                                            ) 
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
        
        //let distanceInvariantTrafo (view : IMod<CameraView>) (pos:V3d) (scale:float) =
        //    let pcam = 

        let discISg color size height trafo =
            Sg.cylinder 30 color size height
                        |> Sg.noEvents
                        |> Sg.shader {
                            do! Aardvark.Opc.Shader.stableTrafo
                            do! DefaultSurfaces.vertexColor
                            do! DefaultSurfaces.simpleLighting
                        }
                        |> Sg.trafo(trafo) 

        let coneISg color radius size trafo =  
            Sg.cone 30 color radius size
                        |> Sg.noEvents
                        |> Sg.shader {
                            do! Aardvark.Opc.Shader.stableTrafo
                            do! DefaultSurfaces.vertexColor
                            do! DefaultSurfaces.simpleLighting
                        }                       
                        |> Sg.trafo(trafo) 

        let brush (hovered : IMod<Trafo3d option>) = 
            let trafo =
                hovered |> Mod.map (fun o -> 
                    match o with 
                        | Some t-> t
                        | None -> Trafo3d.Scale(V3d.Zero)
                )

            Sg.mkDotISg (Mod.constant C4b.Red) (Mod.constant 0.05) trafo
            |> Sg.effect [
                toEffect <| Aardvark.Opc.Shader.stableTrafo
                toEffect <| DefaultSurfaces.vertexColor
                toEffect <| DefaultSurfaces.simpleLighting
            ]                                                       
        
        let drawDns (anno : MAnnotation)  (view : IMod<CameraView>) (conf:MViewConfigModel)=                         
            aset {
                            
                let! dns = anno.dnsResults 
                match dns with
                    | Some x -> 
                        let center = anno.points |> AList.toMod |> Mod.map (fun list -> list.[PList.count list / 2])

                        let color = 
                            DipAndStrike.dipColor x.dipAngle conf.minDipAngle.value conf.maxDipAngle.value
                                                                    
                        let lineLengthScale = 
                            Sg.computeInvariantScale view conf.nearPlane.value center conf.arrowLength.value (Mod.constant 60.0)
                        
                        let planeScale = 
                            Sg.computeInvariantScale view conf.nearPlane.value center conf.dnsPlaneSize.value (Mod.constant 60.0)
                                                                        
                        let radius =   lineLengthScale |> Mod.map(fun d -> d * 0.05)
                        let coneSize = lineLengthScale |> Mod.map(fun d -> d * 0.3)
                        let height =   planeScale |> Mod.map(fun d -> d * 0.01)

                        let posTrafo =
                            center |> Mod.map(fun d -> Trafo3d.Translation(d))

                        // disc
                        let discTrafo =
                            Mod.map2(fun (pln:Plane3d) pos -> (Trafo3d.RotateInto(V3d.ZAxis, pln.Normal) * pos)) x.plane posTrafo

                        yield discISg color planeScale height discTrafo 

                        let! dip = x.dipDirection

                        // dip vec       
                        let dipLine = 
                            alist {
                                let! c = center
                                let! s = lineLengthScale
                                //let! f = conf.arrowSize.value

                                yield c
                                yield c + dip.Normalized * s //* f
                            }                            

                        yield Sg.lines dipLine color conf.arrowThickness.value posTrafo anno.key 

                        let coneTrafo = 
                            Mod.map2(fun c s -> Trafo3d.RotateInto(V3d.ZAxis, dip) * Trafo3d.Translation(c + dip.Normalized * s)) center lineLengthScale

                        yield coneISg color radius coneSize coneTrafo

                        //strike vecs
                        let strikeLen = 
                            Mod.map2(fun (str:V3d) s -> str.Normalized * s * 0.66) x.strikeDirection lineLengthScale

                        let strikeLine1 =
                            alist {
                                let! c = center
                                let! s = strikeLen
                                yield c; yield c - s
                            }

                        let strikeLine2 = 
                            alist {
                                let! c = center
                                let! s = strikeLen
                                yield c; yield c + s
                            }                                            

                        yield Sg.lines strikeLine1 (Mod.constant C4b.Red) conf.arrowThickness.value posTrafo anno.key // strike1 vec
                        yield Sg.lines strikeLine2 (Mod.constant C4b.Red) conf.arrowThickness.value posTrafo anno.key // strike2 vec
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
                
            let color     = Mod.bindOption anno C4b.VRVisGreen   (fun a -> a.color.c) 
            let thickness = Mod.bindOption anno 1.0              (fun a -> a.thickness.value) 
            let key       = Mod.bindOption anno Guid.Empty       (fun a -> a.key) 
            let trafo     = Mod.bindOption anno Trafo3d.Identity (fun a -> a.modelTrafo)          
                                   
            Sg.ofList [
                Sg.lines polyPoints color thickness trafo key //polyline
                Sg.dots points color view                     //support points
            ]         

        let text (anno : MAnnotation) = 
            Sg.markdown MarkdownConfig.light anno.text
                    |> Sg.noEvents
                    |> Sg.shader {
                            do! Aardvark.Opc.Shader.stableTrafo
                        }
                   // |> Sg.transform (Trafo3d.FromOrthoNormalBasis(-V3d.IOO, V3d.OOI, V3d.OIO))
                    |> Sg.trafo anno.modelTrafo
                    |> Sg.translate 0.0 0.0 3.0

        let finishedAnnotation (anno : MAnnotation)(view : IMod<CameraView>)(c : IMod<C4b>) (conf:MViewConfigModel) =
            let points = getPolyline anno
                       
            Sg.ofList [
                Sg.lines points anno.color.c anno.thickness.value anno.modelTrafo anno.key;         
                Sg.dots anno.points c view                                
                drawDns anno view conf |> Sg.onOff anno.showDns
            ] |> Sg.onOff anno.visible

        let view (model:MDrawingModel) (cam:IMod<CameraView>) =
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler
            let annoSet = ASet.ofAList model.annotations 

            let annotations =
                aset {
                    for a in annoSet do
                       let c = UI.mkColor model a
                       yield finishedAnnotation a cam c model.config
                } |> Sg.set
                                
            Sg.ofList [
                brush model.hoverPosition; 
                annotations
                workingAnnotation model.working cam
            ]
            
