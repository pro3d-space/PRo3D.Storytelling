namespace PRo3D

module DrawingApp =
   // open Newtonsoft.Json
    open System
    open System.IO
    
    open System.Windows.Forms
    open System.Text
    open System.Net.WebSockets
    open System.Threading
    open System.Collections.Concurrent    

    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.Application
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Opc
    open Aardvark.Rendering.Text
    open Aardvark.VRVis
    open Aardvark.VRVis.Opc

    open Aardvark.UI
    open Aardvark.UI.Primitives
    
    open PRo3D
    open PRo3D.Drawing
    open PRo3D.Groups

    open MBrace.FsPickler.Json   
    
    let closePolyline (a:Annotation) = 
        let firstP = a.points.[0]
        let lastP = a.points.[(a.points.Count-1)]
        match a.projection with
            | Projection.Viewpoint | Projection.Sky ->     
                let newSegment = { startPoint = firstP; endPoint = lastP; points = PList.ofList [firstP;lastP] }

                if PRo3D.Config.useAsyncIntersections then
                    { a with segments = PList.append newSegment a.segments }
                else
                    let dir = newSegment.endPoint - newSegment.startPoint
                    let points = [ 
                            for s in 0 .. PRo3D.Config.sampleCount do
                                yield newSegment.startPoint + dir * (float s / float PRo3D.Config.sampleCount) // world space
                        ]
                    let newSegment = { startPoint = firstP; endPoint = lastP; points = PList.ofList points }
                    { a with segments = PList.append newSegment a.segments }
            | _ -> { a with points = a.points |> PList.append firstP }


    let getFinishedAnnotation up north (model : DrawingModel) =
        match model.working with
            | Some w ->  
                let w = match w.geometry with
                        | Geometry.Polygon -> closePolyline w
                        | _-> w 
                let dns = w.points |> DipAndStrike.calculateDipAndStrikeResults (up) (north)
                    //match w.points.Count with 
                    //    | x when x > 2 ->
                    //        //let up = 
                    //        //let north = up.Cross(V3d.OOI.Cross(up))
                    //        let result = w.points |> DipAndStrike.calculateDipAndStrikeResults (up) (north)
                    //        Some result //more acc. by using segments as well?
                    //    | _ -> None 
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
                                              
            model.annotations |> Groups.addLeafToActiveGroup (Leaf.Annotations a) true
                            
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
                        let vec = newSegment.endPoint - newSegment.startPoint
                        let dir = vec.Normalized
                        let step = vec.Length / float PRo3D.Config.sampleCount
                        let points = [ 
                          for s in 0 .. PRo3D.Config.sampleCount do
                            let p = newSegment.startPoint + dir * (float s) * step // world space
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
                //(Annotation.make model.projection model.geometry model.semantic surfaceName)  
                //    with points = PList.ofList [p]; modelTrafo = Trafo3d.Translation p
                (Annotation.make2 model.projection model.geometry model.color model.thickness surfaceName)
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
            //getMaxDipAngle     : 'ma -> IMod<float>
            //getMinDipAngle     : 'ma -> IMod<float>
            getArrowThickness  : 'ma -> IMod<float>
            getArrowLength     : 'ma -> IMod<float>
            getDnsPlaneSize    : 'ma -> IMod<float>
        }
   

    let saveAnnotations (path:string) (model:DrawingModel) = 
        match path.IsEmptyOrNull() with
            | true -> model
            | false-> 
                let sa = 
                    {
                        annotations = model.annotations
                        colorLegend = model.colorLegend
                    }
                Serialization.save "./lastMeasure" sa |> ignore 
                let saveA = Serialization.save path sa
                { model with annotations = saveA.annotations; colorLegend = saveA.colorLegend } 

    let getAnnotationPath (scenePath:string) = 
        let p = System.IO.Path.GetDirectoryName(scenePath)
        let name = System.IO.Path.GetFileNameWithoutExtension(scenePath) + ".ann"
        p + "\\" + name

    let saveSceneAnnotations (scenePath:string) (model:DrawingModel) = 
        saveAnnotations (getAnnotationPath scenePath) model

    let update<'a> (bigConfig : 'a) (smallConfig : SmallConfig<'a> ) (bc : BlockingCollection<string>) (model : DrawingModel) (act : Action) =
        match (act, model.draw) with
            | StartDrawing, _ ->                     
                    { model with draw = true }
            | StopDrawing, _ -> 
                    { model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPointAdv (point, hitFunction, name), true ->
                    let up    = smallConfig.up.Get(bigConfig)
                    let north = smallConfig.north.Get(bigConfig)

                    let model, newSegment = addPoint up north hitFunction point model name bc
                    
                    match newSegment with
                        | None         -> model
                        | Some segment -> addNewSegment hitFunction model segment
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
            | ChangeColor c, _ -> 
                { model with color = ColorPicker.update model.color c }
            | ChangeThickness th, _ ->
                { model with thickness = Numeric.update model.thickness th }
            | SetExportPath s, _ ->
                    { model with exportPath = Some s }
            | Export, _ ->
                    //let path = Path.combine([model.exportPath; "drawing.json"])
                    //printf "Writing %i annotations to %s" (model.annotations |> PList.count) path
                    //let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> JsonConvert.SerializeObject
                    //Serialization.writeToFile path json 
                    
                    model
            | Save p,_->
                //match Dialogs.saveDialog w with
                //    | None -> model
                //    | Some p -> 
                //        let sa = 
                //            {
                //                annotations = model.annotations
                //                colorLegend = model.colorLegend
                //            }
                //        Serialization.save "./lastMeasure" sa |> ignore //model.annotations
                //        //{ model with annotations = (Serialization.save p model.annotations } 
                //        let saveA = Serialization.save p sa
                //        { model with annotations = saveA.annotations; colorLegend = saveA.colorLegend } 

                //match p.IsEmptyOrNull() with
                //    | true -> model
                //    | false-> 
                //        let sa = 
                //            {
                //                annotations = model.annotations
                //                colorLegend = model.colorLegend
                //            }
                //        Serialization.save "./lastMeasure" sa |> ignore 
                //        let saveA = Serialization.save p sa
                //        { model with annotations = saveA.annotations; colorLegend = saveA.colorLegend } 
                saveAnnotations p model

            | Load p,_ ->                
              //match Dialogs.openDialog w with
              //  | None -> model
              //  | Some path -> 
              //    //let groups = Groups.tryLoadGroups path
              //    let groups = Groups.tryLoadAnnotations path
              //    match groups with 
              //      //| Some g -> { model with annotations = g  }
              //      | Some g -> { model with annotations = g.annotations; colorLegend = g.colorLegend }
              //      | None -> model

              match p.Length < 1 with
                | true -> model
                | false -> 
                  //let groups = Groups.tryLoadGroups path
                  let path = p.[0]
                  let groups = Groups.tryLoadAnnotations path
                  match groups with 
                    //| Some g -> { model with annotations = g  }
                    | Some g -> { model with annotations = g.annotations; colorLegend = g.colorLegend }
                    | None -> model
            | QuickLoad,_ ->                
              let groups = Groups.tryLoadGroups "./lastMeasure" 
              match groups with 
                | Some g -> { model with annotations = g  }
                | None -> model
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
            | ColorLegendMessage msg,_ -> 
                { model with colorLegend = FalseColorLegendApp.update model.colorLegend msg }
            | FlyToAnnotation msg, _ ->
                model
            | SaveCSV p, _ -> 
                let annotations =
                    model.annotations.flat
                        |> Leaf.toAnnotations
                        |> HMap.toList 
                        |> List.map snd
                        |> List.map (Csv.exportAnnotation model.annotations.groupsLookup)
                      //  |> Seq.ofList
                        
                let csvTable = Csv.Seq.csv "," true id annotations

                match p.IsEmptyOrNull() with
                    | true -> model
                    | false -> 
                        Csv.Seq.write (p) csvTable |> ignore
                        model
            | _ -> model

    module UI =
        open Aardvark.Base.Incremental
        //open Aardvark.UI.TreeViewApp

        let viewAnnotationToolsHorizontal (model:MDrawingModel) =
            Html.Layout.horizontal [
                Html.Layout.boxH [ i [clazz "large Write icon"][] ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.geometry SetGeometry ]
                Html.Layout.boxH [ Html.SemUi.dropDown model.projection SetProjection ]
                Html.Layout.boxH [ ColorPicker.view model.color |> UI.map ChangeColor; div[][] ]
                Html.Layout.boxH [ Numeric.view' [InputBox] model.thickness |> UI.map ChangeThickness ]
              //  Html.Layout.boxH [ Html.SemUi.dropDown model.semantic SetSemantic ]                
            ]
                      
        let mkColor (model : MGroupsModel) (a : MAnnotation) =
            let id = a.key |> Mod.force

            let color =  
                model.selectedLeaves
                    |> ASet.map(fun x -> x.id = id)
                    |> ASet.contains true
                    |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else a.color.c)
            
            color

        let isSingleSelect (model : MGroupsModel) (a : MAnnotation) =
            model.singleSelectLeaf |> Mod.map( fun x -> 
                match x with 
                  | Some selected -> selected = (a.key |> Mod.force)
                  | None -> false )

        let annotationMenu = 
            div [ clazz "ui dropdown item"] [
                text "Annotations"
                i [clazz "dropdown icon"][] 
                div [ clazz "menu"] [
                    //div [ clazz "ui inverted item"; Dialogs.onSaveFile Save;
                    //        clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Scene'}]}));")][
                    //        text "Save"
                    //]
                    div [ clazz "ui inverted item"; Dialogs.onChooseFiles Load;
                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));" )][
                            text "Load"
                    ]
                    div [ clazz "ui inverted item"; onMouseClick (fun _ -> Clear)][
                            text "Clear List"
                    ]
                    div [ clazz "ui inverted item"; Dialogs.onSaveFile SaveCSV;
                        clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements as .csv'}]}));")][
                            text "Save .csv"
                    ]
                ]
            ]
                    

        let viewAnnotationImportExport (m:MDrawingModel) =
           div [clazz "ui buttons inverted"] [
                
                //button [clazz "ui icon button"; onMouseClick (fun _ -> Save)] [
                //    i [clazz "save icon"] [] ] |> UI.wrapToolTipBottom "Save Annotations"
                button [clazz "ui icon button"; Dialogs.onSaveFile Save;
                        clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements'}]}));")] [
                    i [clazz "save icon"] [] ] |> UI.wrapToolTipBottom "Save Annotations"
                
                //button [clazz "ui icon button"; onMouseClick (fun _ -> Load)] [
                //    i [clazz "folder outline icon"] [] ] |> UI.wrapToolTipBottom "Load Annotations"
                button [clazz "ui icon button"; Dialogs.onChooseFiles Load;
                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ] [
                    i [clazz "folder outline icon"] [] ] |> UI.wrapToolTipBottom "Load Annotations"
                
                button [clazz "ui icon button"; onMouseClick (fun _ -> Clear)] [
                    i [clazz "file outline icon"] [] ] |> UI.wrapToolTipBottom "Clear Annotation List"

                button [clazz "ui icon button"; Dialogs.onSaveFile SaveCSV;
                        clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements as .csv'}]}));")] [
                    i [clazz "save icon inverted"] [] ] |> UI.wrapToolTipBottom "Save .csv"
                                
                //button [clazz "ui icon button"; onMouseClick (fun _ -> QuickLoad)] [
                //    i [clazz "folder icon"] [] ] |> UI.wrapToolTipBottom "Load recent Annotations"
                                
                //button [clazz "ui icon button"; onMouseClick (fun _ -> Send)] [
                //    i [clazz "send icon"] [] ] |> UI.wrapToolTipBottom "???"
                
                //button [clazz "ui icon button"; onMouseClick (fun _ -> Undo)] [
                //    i [clazz "arrow left icon"] [] ]  |> UI.wrapToolTipBottom "Undo"
                
                //button [clazz "ui icon button"; onMouseClick (fun _ -> Redo)] [
                //    i [clazz "arrow right icon"] [] ] |> UI.wrapToolTipBottom "Redo"
                
            ] 

        let viewAnnotations (annotations : alist<MAnnotation>) : alist<DomNode<Action>> =
          
            annotations |> AList.map(fun a ->
              div [clazz "item"] [
                i [clazz "large Sticky Note middle aligned icon"][]
                div [clazz "content"] [
                  div [clazz "header"][Incremental.text (a.geometry |> Mod.map(string))]
                  div [clazz "description"][Incremental.text (a.points |> AList.count |> Mod.map(string))]
                ]
              ])
          
               
        let viewAnnotationsInGroup (path:list<Index>) (model:MGroupsModel)(singleSelect : MAnnotation*list<Index> -> 'outer)(multiSelect : MAnnotation*list<Index> -> 'outer)(lift : Groups.Action -> 'outer) (annotations: alist<MAnnotation>) : alist<DomNode<'outer>> =                         
          annotations 
            |> AList.map(fun a ->
              
              let singleSelect = fun _ -> singleSelect(a,path)
              let multiSelect  = fun _ -> multiSelect(a,path)
              
              let ac = sprintf "color: %s" (Html.ofC4b C4b.Gray)

              let key = a.key |> Mod.force
              let visibleIcon = 
                amap {
                  yield onMouseClick (fun _ -> lift <| Groups.ToggleChildVisibility (key,path))
                  let! visible = a.visible
                  if visible then 
                    yield clazz "unhide icon" 
                  else 
                    yield clazz "hide icon"
                } |> AttributeMap.ofAMap

              let iconAttributes =
                amap {                  
                  yield clazz "large Sticky Note middle aligned icon"
                  yield onMouseClick (multiSelect)

                  let! c = mkColor model a
                  yield style (sprintf "color: %s" (Html.ofC4b c))
                } |> AttributeMap.ofAMap


              
              div [clazz "item"] [
                Incremental.i iconAttributes AList.empty
                div [clazz "content"] [
                  let header = Mod.map2 (fun x y -> sprintf "%A | %A | " x y) a.geometry a.semantic
                  let desc   = Mod.map2 (fun x y -> sprintf "#Points: %A | taken on: %A" x y) (a.points |> AList.count) a.surfaceName
                  
                  //header
                  yield Incremental.div (AttributeMap.ofList [clazz "header"; style ac]) (
                      alist {                          
                          yield div[onClick singleSelect][Incremental.text header]
                          yield Incremental.i visibleIcon AList.empty |> UI.wrapToolTip "Toggle Visible"
                          yield i [clazz "binoculars icon"; onClick (fun _ -> FlyToAnnotation key)][] |> UI.wrapToolTip "FlyTo"
                      } 
                  ) 

                  //description
                  yield div [clazz "description";style ac ] [Incremental.text desc]      
                ]
              ]                            
            )     
              
        let rec viewTree path (group : MNode) (model : MGroupsModel) (lookup : amap<Guid, MAnnotation>) : DomNode<Action> =
                                                    
            let icon =
                adaptive {
                    let! s = model.activeGroup    
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

                    Incremental.i toggleMap AList.empty |> UI.wrapToolTip "Toggle Group"
                ]
             
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                    let! expanded = group.expanded
                    if expanded then 
                      yield clazz "icon outline open folder"
                    else 
                      yield clazz "icon outline folder"
                    yield style "overflow-y : visible"
                } |> AttributeMap.ofAMap
            
            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = group.expanded
                    if (not isExpanded) then 
                      yield style "display:none"                             
                }         

            let singleSelect = 
                fun (a:MAnnotation,path:list<Index>) -> 
                    Action.GroupsMessage(Groups.SingleSelectLeaf (path, a.key |> Mod.force, ""))

            let multiSelect = 
                fun (a:MAnnotation,path:list<Index>) -> 
                    Action.GroupsMessage(Groups.AddLeafToSelection (path, a.key |> Mod.force, ""))

            let lift = fun (a:Groups.Action) -> (GroupsMessage a)

            let subNodes = group.subNodes |> AList.mapi (fun i v -> viewTree (i::path) v model lookup) 
            
            let annos = 
              group.leaves 
                |> AList.map(fun x -> lookup |> AMap.find x |> Mod.force) 
                |> viewAnnotationsInGroup path model singleSelect multiSelect lift
                //|> viewAnnotations

            let nodes = annos |> AList.append subNodes

            div [ clazz "item"] [
              Incremental.i itemAttributes AList.empty
              div [ clazz "content" ] [
                div [ clazz "description noselect"] [desc]
                Incremental.div (AttributeMap.ofAMap childrenAttribs) (nodes)
              ]
            ]
            

        let toMAnnotation (leaf : MLeaf) =
          match leaf with 
          | MAnnotations a -> a
          | _ -> failwith "wrong type"

        let viewAnnotationGroups (model:MDrawingModel) = 
          let a = 
            model.annotations.flat 
              |> AMap.map(fun _ v -> (v |> Mod.force) |> toMAnnotation)              
           
          require GuiEx.semui (
              let tree = viewTree [] model.annotations.rootGroup model.annotations a
              //Incremental.div (AttributeMap.ofList [clazz "ui list"]) ([])
              div [clazz "ui list"] [tree]
          )  
                               
 
    let threads (m : DrawingModel) = m.pendingIntersections

    module Sg =                
             
        let discISg color size height trafo =
            Sg.cylinder 30 color size height              
              |> Sg.noEvents
              |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
              |> Sg.uniform "Size" size
              |> Sg.shader {
                  do! Shader.screenSpaceScale
                  do! Shader.stableTrafo
                  do! DefaultSurfaces.vertexColor
                  //do! Aardvark.Opc.Shader.stableLight
              }
              |> Sg.trafo(trafo)

        let coneISg color radius height trafo =  
            Sg.cone 30 color radius height
               |> Sg.noEvents
               //|> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZW))
               //|> Sg.uniform "Size" ( radius |> Mod.map(fun x -> x * 700.0))
               //|> Sg.uniform "posTrafo" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward))
               |> Sg.shader {
                   //do! PRo3D.OrientationCube.Shader.screenSpaceScaleTrafo
                   do! Shader.stableTrafo
                   do! DefaultSurfaces.vertexColor
                   do! DefaultSurfaces.simpleLighting
               }
               |> Sg.trafo(trafo) 
               
               

        //let brush (hovered : IMod<Trafo3d option>) = 
        //    let trafo =
        //        hovered |> Mod.map (fun o -> 
        //            match o with 
        //                | Some t-> t
        //                | None -> Trafo3d.Scale(V3d.Zero)
        //        )

        //    Sg.mkDotISg (Mod.constant C4b.Red) (Mod.constant 0.05) trafo
        //    |> Sg.effect [
        //        toEffect <| Aardvark.Opc.Shader.stableTrafo
        //        toEffect <| DefaultSurfaces.vertexColor
        //        toEffect <| DefaultSurfaces.simpleLighting
        //    ]                                                       
        
        type innerViewConfig =
            {
                nearPlane       : IMod<float>
                hfov            : IMod<float>
                //maxDipAngle     : IMod<float>
                //minDipAngle     : IMod<float>
                arrowThickness  : IMod<float>
                arrowLength     : IMod<float>
                dnsPlaneSize    : IMod<float>
            }

        let drawDns (anno : MAnnotation) (conf:innerViewConfig) (cl : MFalseColorsModel) (cam:IMod<CameraView>) =                         
          aset {                            
            let! dns = anno.dnsResults 
            match dns with
              | Some x -> 
                let center = anno.points |> AList.toMod |> Mod.map (fun list -> list.[PList.count list / 2])

                //let color = 
                //  DipAndStrike.dipColor x.dipAngle conf.minDipAngle conf.maxDipAngle

                let color = FalseColorLegendApp.Draw.getColorDnS cl x.dipAngle
                                                                                                                    
                let scale = 
                  conf.dnsPlaneSize 
                    |> Mod.bind(fun size -> 
                      anno.results 
                        |> Mod.bind(fun y ->
                          match y with
                          | Some k -> k.length |> Mod.map(fun y -> (Math.Pow(y,1.0/3.0)) * size)
                          | None -> Mod.constant 1.0))
                          
                let discRadius = scale //Mod.map2 (*) scale screenSpaceScale
                let posTrafo = center |> Mod.map Trafo3d.Translation

                // disc
                let discTrafo =
                    Mod.map2(fun (pln:Plane3d) pos -> (Trafo3d.RotateInto(V3d.ZAxis, pln.Normal) * pos)) x.plane posTrafo

                yield discISg color discRadius (discRadius |> Mod.map(fun d -> d * 0.01)) discTrafo

                let lineLength = conf.arrowLength //discRadius |> Mod.map((*) 1.5)
                let size = Sg.computeInvariantScale cam conf.nearPlane center lineLength (Mod.constant 60.0)
                
                let coneHeight = lineLength |> Mod.map((*) 0.2) //lineLength
                let coneRadius = coneHeight |> Mod.map((*) 0.3)

                // dip arrow       

                let! lineLength' = lineLength
                let! center'     = center
                //let! size'       = size

                let! dip = x.dipDirection
                let dipLine = 
                  alist {
                      yield center'
                      yield center' + dip.Normalized * lineLength'
                  }

                let p0 = center' + dip.Normalized * lineLength'

                //yield Sg.lines dipLine color conf.arrowThickness posTrafo anno.key  
                yield Sg.scaledLines dipLine color conf.arrowThickness posTrafo anno.key

                let coneTrafo = 
                  lineLength |>
                    Mod.map(fun s -> Trafo3d.RotateInto(V3d.ZAxis, dip) * Trafo3d.Translation(center' + dip.Normalized * s))

                yield coneISg color coneRadius coneHeight coneTrafo
                //yield Sg.scaledArrows dipLine p0 color conf.arrowThickness posTrafo dip coneRadius coneHeight coneTrafo cam

                //strikes lines
                let! strike = x.strikeDirection
                let strikeLine1 =
                  alist {                                                                
                      yield center' - strike.Normalized * lineLength'
                      yield center' + strike.Normalized * lineLength'
                  }                                            

                //yield Sg.lines strikeLine1 (Mod.constant C4b.Red) conf.arrowThickness posTrafo anno.key
                yield Sg.scaledLines strikeLine1 (Mod.constant C4b.Red) conf.arrowThickness posTrafo anno.key
              | None -> ()            
          } |> Sg.set
            
        let getPolylinePoints (a : MAnnotation) =
            alist {                
                let! hasSegments = 
                  a.segments 
                    |> AList.count 
                    |> Mod.map (fun x ->
                      Log.line "[Drawing] drawing polz with %A segments" x
                      x > 0)

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

        let drawWorkingAnnotation (anno : IMod<Option<MAnnotation>>)  = 

            let polyPoints =
                alist {
                    let! anno = anno
                    match anno with
                        | Some a -> yield! getPolylinePoints a
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
                //Sg.dots polyPoints (Mod.constant C4b.VRVisGreen) // debug sample points
                Sg.dots points color                     //support points
            ]         
        
        let drawText (view : IMod<CameraView>) conf (anno : MAnnotation) = 
            let points = anno.points |> AList.toMod
            let pos = 
                points |> Mod.map(fun a -> a |> PList.toList |> List.head) 

            Sg.text view conf.nearPlane conf.hfov pos anno.modelTrafo anno.text anno.textsize.value

        let optional (sg : ISg<_>) (m : IMod<bool>) : aset<ISg<_>> =
          adaptive {
              let! m = m 
              if m then return sg
              else return Sg.empty
          } |> ASet.ofModSingle

        let finishedAnnotation (anno : MAnnotation) (c : IMod<C4b>) (conf:innerViewConfig) (view : IMod<CameraView>) (dots : IMod<bool>) =
            let points = getPolylinePoints anno            
            let dots  = dots |> optional (Sg.dots anno.points c)
           
            let texts = 
              anno.text 
                |> Mod.map (String.IsNullOrEmpty >> not) 
                |> optional (drawText view conf anno)

            let sgs = ASet.unionMany' [dots; texts]

            Sg.ofList [
                Sg.lines points c anno.thickness.value anno.modelTrafo anno.key                  
                Sg.set sgs 
            ] |> Sg.onOff anno.visible

        let finishedAnnotationDiscs (anno : MAnnotation) (conf:innerViewConfig) (cl : MFalseColorsModel) (cam:IMod<CameraView>) =
           anno.showDns |> optional (drawDns anno conf cl cam) |> Sg.set
        
        let toAnnotationOpt = 
            fun y -> 
                match y with
                    | MAnnotations ann -> Some ann
                    | _ -> None

        let view<'ma> (mbigConfig : 'ma)(msmallConfig : MSmallConfig<'ma>) (view : IMod<CameraView>) (model:MDrawingModel) : ISg<Drawing.Action> * ISg<Drawing.Action>=
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler           
            let annoSet = 
                model.annotations.flat 
                    |> AMap.chooseM(fun _ y -> y |> Mod.map toAnnotationOpt) 
                    |> AMap.toASet

            let config = 
                {
                    nearPlane      = msmallConfig.getNearPlane      mbigConfig
                    hfov           = msmallConfig.getHfov           mbigConfig
                    //maxDipAngle    = msmallConfig.getMaxDipAngle    mbigConfig
                    //minDipAngle    = msmallConfig.getMinDipAngle    mbigConfig
                    arrowLength    = msmallConfig.getArrowLength    mbigConfig
                    arrowThickness = msmallConfig.getArrowThickness mbigConfig
                    dnsPlaneSize   = msmallConfig.getDnsPlaneSize   mbigConfig
                }
           
            let annotations =              
              annoSet 
                |> ASet.map(fun (_,a) -> 
                  let c = UI.mkColor model.annotations a
                  finishedAnnotation a c config view (a.geometry |> Mod.map(function | Geometry.Point -> true | _ -> false)))
                |> Sg.set               
                                
            let overlay = 
              Sg.ofList [
               // brush model.hoverPosition; 
                annotations
                drawWorkingAnnotation model.working
              ]

            let depthTest = annoSet |> ASet.map(fun (_,a) -> finishedAnnotationDiscs a config model.colorLegend view) |> Sg.set

            (overlay, depthTest)
            
