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
    
    open System.Windows.Forms
    open System.Text
    open System.Net.WebSockets
    open System.Threading
    open PRo3D
    open System.Collections.Concurrent    
    open PRo3D.Drawing
    open PRo3D.Groups

    open MBrace.FsPickler.Json   
    
    let getFinishedAnnotation up north (model : DrawingModel) =
        match model.working with
            | Some w ->               
                let dns = 
                    match w.points.Count with 
                        | x when x > 2 ->
                            //let up = 
                            //let north = up.Cross(V3d.OOI.Cross(up))
                            let result = w.points |> DipAndStrike.calculateDipAndStrikeResults (up) (north)
                            Some result //more acc. by using segments as well?
                        | _ -> None 
                let results = Annotation.calculateAnnotationResults w up
                Some { w with dnsResults = dns ; results = Some results }
            | None -> None

    let finishAndAppendAndSend up north (model : DrawingModel) (bc : BlockingCollection<string>) = 
        //if there are at least 3 points do dns calculation (debug, dev). 
        //might take long for large number of points so created on demand?

        //let anns = 
        //    match getFinishedAnnotation up north model with
        //        | Some a -> 
        //            model.annotations.annotationsFlat |> PList.prepend a
        //        | None -> model.annotations.annotationsFlat

        let groups = 
            match getFinishedAnnotation up north model with
              | Some a -> 
                let json = a |> JsonTypes.ofAnnotation |> Aardvark.UI.Pickler.jsonToString                 
                bc.Add json                    
                    
                model.annotations 
                  |> Groups.addLeafToActiveGroup (Leaf.Annotations a)
              | None -> model.annotations             

        { model with  working = None; pendingIntersections = ThreadPool.empty; annotations = groups } 

    let addPoint up north samplePoint p model surfaceName bc =
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
                                            let points = [ 
                                                    for s in 0 .. PRo3D.Config.sampleCount do
                                                        let p = newSegment.startPoint + dir * (float s / float PRo3D.Config.sampleCount) // world space
                                                        match samplePoint p with
                                                            | None -> ()
                                                            | Some projectedPoint -> yield projectedPoint
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
                finishAndAppendAndSend up north model bc, None
            | Geometry.Line, 2 -> 
                finishAndAppendAndSend up north model bc, None
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

    type SmallConfig<'a> =
        {
            up    : Lens<'a,V3d>
            north : Lens<'a,V3d>
        }

    type MSmallConfig<'ma> =
        {            
            getNearPlane       : 'ma -> IMod<float>
            getHfov            : 'ma -> IMod<float>
            getMaxDipAngle     : 'ma -> IMod<float>
            getMinDipAngle     : 'ma -> IMod<float>
            getArrowThickness  : 'ma -> IMod<float>
            getArrowLength     : 'ma -> IMod<float>
            getDnsPlaneSize    : 'ma -> IMod<float>
        }

    let update<'a> (bigConfig : 'a) (smallConfig : SmallConfig<'a> ) (w : Form) (bc : BlockingCollection<string>) (model : DrawingModel) (act : Action) =
        match (act, model.draw) with
            | StartDrawing, _ ->                     
                    { model with draw = true }
            | StopDrawing, _ -> 
                    { model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPointAdv (point, samplePoint, name), true ->
                    let up    = smallConfig.up.Get(bigConfig)
                    let north = smallConfig.north.Get(bigConfig)

                    let model, newSegment = addPoint up north samplePoint point model name bc
                    
                    match newSegment with
                        | None         -> model
                        | Some segment -> addNewSegment samplePoint model segment
                    |> stash

            | SetSegment(segmentIndex,segment), _ ->
                match model.working with
                    | None -> model
                    | Some w ->                         
                        { model with working = Some { w with segments = PList.setAt segmentIndex segment w.segments } }

            | Finish, _ -> 
                    let up    = smallConfig.up.Get(bigConfig)
                    let north = smallConfig.north.Get(bigConfig)

                    (finishAndAppendAndSend up north model bc) |> stash
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
                        { model with annotations = Groups.loadGroups path }
            | QuickLoad,_ ->  
                let groups = Groups.loadGroups "./lastMeasure" 
                { model with annotations = groups }
            | Send, _ ->                                                      
                model
            | Clear,_ ->
                { model with annotations = GroupsModel.initial }
            | Nop, _ -> model                   
            | Undo, _ -> 
                match model.past with
                    | Some p -> { p with future = Some model }
                    | None -> model
            | Redo, _ ->
                match model.future with
                    | Some f -> f
                    | None -> model           
            | GroupsMessage msg,_ ->
                let m = { model with annotations = Groups.update model.annotations msg}
                m
            | _ -> model

    module UI =
        open Aardvark.Base.Incremental
        //open Aardvark.UI.TreeViewApp

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
        
        let mkColor (model : MGroupsModel) (a : MAnnotation) =
            let id = a.key |> Mod.force

            let color =  
                model.selectedChilds
                    |> ASet.map(fun x -> x.id = id)
                    |> ASet.contains true
                    |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else a.color.c)
            
            color

        let viewAnnotationImportExport =
           div [clazz "ui buttons inverted"] [
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Save Annotations"; onMouseClick (fun _ -> Save)] [
                            i [clazz "save icon"] [] ] 
                )
                //i [clazz "save icon"; onMouseClick (fun _ -> Save)] [] |> UI.wrapToolTip "Save Annotations" 
                
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Load Annotations"; onMouseClick (fun _ -> Load)] [
                            i [clazz "folder outline icon"] [] ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Clear Annotation List"; onMouseClick (fun _ -> Clear)] [
                            i [clazz "file outline icon"] [] ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Load recent Annotations"; onMouseClick (fun _ -> QuickLoad)] [
                            i [clazz "folder icon"] [] ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "???"; onMouseClick (fun _ -> Send)] [
                            i [clazz "send icon"] [] ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Undo"; onMouseClick (fun _ -> Undo)] [i [clazz "arrow left icon"] [] ]
                )
                onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    button [clazz "ui icon button"; attribute "data-content" "Redo"; onMouseClick (fun _ -> Redo)] [i [clazz "arrow right icon"] [] ]
                )
            ] 
         
      

        let viewAnnotationsInGroup (path:list<Index>) (model:MGroupsModel)(select : MAnnotation*list<Index> -> 'outer)(lift : Groups.Action -> 'outer) (annotations: alist<Guid>) : alist<DomNode<'outer>> =
                           
                alist { 

                    let annotations = 
                        annotations
                          |> AList.map (
                            fun x -> 
                            let a = model.flat |> AMap.find x |> Mod.bind(id)
                            a
                            )
                          |> AList.choose( fun x -> 
                            match (x |> Mod.force) with
                                | MAnnotations a -> Some a
                                | _-> None )

                    for a in annotations do 
                        let select = fun _ -> select (a,path)
                                
                        let! c = mkColor model a
                        let bgc = sprintf "color: %s" (Html.ofC4b c)
                        let ac = sprintf "color: %s" (Html.ofC4b C4b.Gray)

                        let! visible = a.visible
                        let visibleIcon = if visible then "unhide icon" else "hide icon"
                        let key = a.key |> Mod.force

                        yield div [clazz "item"] [
                            i [clazz "large Sticky Note middle aligned icon";onClick select; style bgc][]
                            div [clazz "content"] [
                                let header = Mod.map2 (fun x y -> sprintf "%A | %A | " x y) a.geometry a.semantic
                                let desc = Mod.map2 (fun x y -> sprintf "#Points: %A | taken on: %A" x y) (a.points |> AList.toMod |> Mod.map (fun d -> d.Count)) a.surfaceName

                                //header
                                yield Incremental.div (AttributeMap.ofList [clazz "header"; style ac]) (
                                    alist {
                                        yield Incremental.text header
                                        yield i [ clazz visibleIcon;  onClick (fun _ -> lift <| Groups.ToggleChildVisibility (key,path))] [] |> UI.wrapToolTip "Toggle"                                                                                                                
                                        yield i [ clazz "Remove icon"; onClick (fun _ -> lift <| Groups.RemoveChild (key,path))] [] |> UI.wrapToolTip "Remove"                                                        
                                    } 
                                ) 

                                //description
                                yield div [clazz "description";style ac ] [Incremental.text desc]      
                            ]
                        ]
                }     
              
        let rec viewTree path (group : MNode) (model : MGroupsModel) : alist<DomNode<Action>> =

            alist {

                let! s = model.activeGroup
                
                
                let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subNodes    

                let icon =
                    adaptive {
                        let active = s.id
                        let! group  =  group.key

                        return if (active = group) then "circle icon" else "circle thin icon"
                    }
                      
                let map = Groups.mkGroupAttributeMap icon (GroupsMessage(Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)))
                   
                let toggleIcon =
                    adaptive {
                        let! toggle = group.visible
                        return if toggle then "unhide icon" else "hide icon"
                    }

                let toggleMap = Groups.mkGroupAttributeMap toggleIcon (GroupsMessage(Groups.ToggleGroup path))
               
                let color = sprintf "color: %s" (Html.ofC4b C4b.White)
                let desc =
                    div [style color] [       
                        Incremental.text group.name
                        Incremental.i map AList.empty |> UI.wrapToolTip "Set active"
                        
                        i [clazz "plus icon"
                           onMouseClick (fun _ -> 
                             GroupsMessage(Groups.AddGroup path))] [] 
                        |> UI.wrapToolTip "Add Group"   
                        
                        i [clazz "remove circle icon"
                           onMouseClick (fun _ -> 
                             GroupsMessage(Groups.ClearGroup path))] [] 
                        |> UI.wrapToolTip "Clear Group"
                        
                        i [clazz "remove icon red"
                           onMouseClick (fun _ -> Action.GroupsMessage(Groups.RemoveGroup path))] [] 
                        |> UI.wrapToolTip "Remove Group"

                        Incremental.i toggleMap AList.empty |> UI.wrapToolTip "Toggle Group"
                    ]
                 
                let itemAttributes =
                    amap {
                        yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                        let! selected = group.expanded
                        if selected 
                        then yield clazz "icon large outline open folder"
                        else yield clazz "icon large outline folder"
                        yield style "overflow-y : visible"
                    } |> AttributeMap.ofAMap
            
                let childrenAttribs =
                    amap {
                        yield clazz "list"
                        let! isExpanded = group.expanded
                        if isExpanded then yield style "visible"
                        else yield style "hidden"
                    }         


                let select = fun (a:MAnnotation,path:list<Index>) -> Action.GroupsMessage(Groups.SelectChilds (path, a.key |> Mod.force, ""))  // Select (path, a.key |> Mod.force)
                let lift   = fun (a:Groups.Action) -> (GroupsMessage a)

                yield div [ clazz "item"] [
                     Incremental.i itemAttributes AList.empty
                     div [ clazz "content" ] [                         
                         div [ clazz "description noselect"] [desc]
                         Incremental.div (AttributeMap.ofAMap childrenAttribs) (                          
                            alist { 
                                let! isExpanded = group.expanded
                                if isExpanded then yield! children

                                if isExpanded then 
                                    yield! viewAnnotationsInGroup path model select lift group.leaves
                            }
                         )  
                                
                     ]
                ]
                
            }



        let viewAnnotationGroups (model:MDrawingModel) = 
             require Html.semui (
                TreeView.view [] (viewTree [] model.annotations.rootGroup model.annotations)
            )  
                               
 
    let threads (m : DrawingModel) = m.pendingIntersections

    module Sg =                
             
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
        
        type innerViewConfig =
            {
                nearPlane       : IMod<float>
                hfov            : IMod<float>
                maxDipAngle     : IMod<float>
                minDipAngle     : IMod<float>
                arrowThickness  : IMod<float>
                arrowLength     : IMod<float>
                dnsPlaneSize    : IMod<float>
            }

        let drawDns (anno : MAnnotation)  (view : IMod<CameraView>) (conf:innerViewConfig)=                         
            aset {
                            
                let! dns = anno.dnsResults 
                match dns with
                    | Some x -> 
                        let center = anno.points |> AList.toMod |> Mod.map (fun list -> list.[PList.count list / 2])

                        let color = 
                            DipAndStrike.dipColor x.dipAngle conf.minDipAngle conf.maxDipAngle
                                                                    
                        let lineLengthScale = 
                            Sg.computeInvariantScale view conf.nearPlane center conf.arrowLength conf.hfov
                        
                        let planeScale = 
                            Sg.computeInvariantScale view conf.nearPlane center conf.dnsPlaneSize conf.hfov
                                                                        
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

                        yield Sg.lines dipLine color conf.arrowThickness posTrafo anno.key 

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

                        yield Sg.lines strikeLine1 (Mod.constant C4b.Red) conf.arrowThickness posTrafo anno.key // strike1 vec
                        yield Sg.lines strikeLine2 (Mod.constant C4b.Red) conf.arrowThickness posTrafo anno.key // strike2 vec
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

        let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) = 
            let right = forw.Cross up
            let rotTrafo = 
                new Trafo3d(
                    new M44d(
                        right.X, up.X, forw.X, 0.0,
                        right.Y, up.Y, forw.Y, 0.0,
                        right.Z, up.Z, forw.Z, 0.0,
                        0.0,     0.0,  0.0,    1.0
                    ),
                    new M44d(
                        right.X, right.Y, right.Z, 0.0,
                        up.X,    up.Y,    up.Z,    0.0,
                        forw.X,  forw.Y,  forw.Z,  0.0,
                        0.0,     0.0,     0.0,     1.0
                    )
            )
            rotTrafo * modelt

        let text (view : IMod<CameraView>) conf (anno : MAnnotation) =
            let points = anno.points |> AList.toMod
            let pos = 
                points |> Mod.map(fun a -> a |> PList.toList |> List.head) 

            let invScaleTrafo = 
                Sg.computeInvariantScale view conf.nearPlane pos (Mod.constant 0.1) conf.hfov |> Mod.map Trafo3d.Scale

            let billboardTrafo = 
                adaptive {
                    let! v = view
                    let! modelt = anno.modelTrafo

                    return screenAligned v.Forward v.Up modelt
                }
            let bounds : Border2d = { left = 0.1; right = 0.1; top = 0.1; bottom = 0.1 }

            Sg.textWithBackground (Font.create "Consolas" FontStyle.Regular) C4b.White C4b.Black bounds anno.text
                |> Sg.noEvents
                |> Sg.shader {
                        do! Aardvark.Opc.Shader.stableTrafo
                    }
                // |> Sg.transform (Trafo3d.FromOrthoNormalBasis(-V3d.IOO, V3d.OOI, V3d.OIO))
                |> Sg.trafo invScaleTrafo
                |> Sg.trafo billboardTrafo                

        let finishedAnnotation (anno : MAnnotation)(view : IMod<CameraView>)(c : IMod<C4b>) (conf:innerViewConfig) (dots : IMod<bool>) =
            let points = getPolyline anno

            let optional (sg : ISg<_>) (m : IMod<bool>) : aset<ISg<_>> =
                adaptive {
                    let! m = m 
                    if m then return sg
                    else return Sg.ofSeq []
                } |> ASet.ofModSingle

            let dots  = dots |> optional (Sg.dots anno.points c view)
            let dns   = anno.showDns  |> optional (drawDns anno view conf)
            let texts = anno.text |> Mod.map (String.isEmpty >> not) |> optional (text view conf anno)
            let sgs = ASet.unionMany' [dots; dns; texts]

            Sg.ofList [
                Sg.lines points c anno.thickness.value anno.modelTrafo anno.key
                Sg.set sgs 
            ] |> Sg.onOff anno.visible
        
        let toAnnotationOpt = 
            fun y -> 
                match y with
                    | MAnnotations ann -> Some ann
                    | _ -> None

        let view<'ma> (mbigConfig : 'ma)(msmallConfig : MSmallConfig<'ma>) (model:MDrawingModel) (cam:IMod<CameraView>) : ISg<Drawing.Action> =
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler           
            let annoSet = 
                model.annotations.flat 
                    |> AMap.chooseM(fun x y -> y |> Mod.map toAnnotationOpt) 
                    |> AMap.toASet

            let config = 
                {
                    nearPlane      = msmallConfig.getNearPlane      mbigConfig
                    hfov           = msmallConfig.getHfov           mbigConfig
                    maxDipAngle    = msmallConfig.getMaxDipAngle    mbigConfig
                    minDipAngle    = msmallConfig.getMinDipAngle    mbigConfig
                    arrowLength    = msmallConfig.getArrowLength    mbigConfig
                    arrowThickness = msmallConfig.getArrowThickness mbigConfig
                    dnsPlaneSize   = msmallConfig.getDnsPlaneSize   mbigConfig
                }

            let annotations =
                aset {
                    for (_,a) in annoSet do
                       let c = UI.mkColor model.annotations a
                       let dots = a.geometry |> Mod.map(fun geo -> match geo with | Geometry.Line | Geometry.Point -> true | _ -> false)                       
                       yield finishedAnnotation a cam c config dots
                } |> Sg.set
                                
            Sg.ofList [
                brush model.hoverPosition; 
                annotations
                workingAnnotation model.working cam
            ]
            
