namespace PRo3D

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

module DrawingApp =
   // open Newtonsoft.Json
        
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

    let getFinishedAnnotation up north planet (model : DrawingModel) =
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
                let results = DrawingUtilities.Calculations.calculateAnnotationResults w up planet
                Some { w with dnsResults = dns ; results = Some results }
            | None -> None

    let finishAndAppendAndSend up north planet (model : DrawingModel) (bc : BlockingCollection<string>) = 
      
      let groups = 
        match getFinishedAnnotation up north planet model with
          | Some a -> 
            let json = a |> JsonTypes.ofAnnotation |> Aardvark.UI.Pickler.jsonToString                 
            bc.Add json                    
                                              
            model.annotations |> Groups.addLeafToActiveGroup (Leaf.Annotations a) true
                            
          | None -> model.annotations             

      { model with  working = None; pendingIntersections = ThreadPool.empty; annotations = groups } 
    
    let addPoint up north planet samplePoint p model surfaceName bc =
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
              finishAndAppendAndSend up north planet model bc, None
          | Geometry.Line, 2 -> 
              finishAndAppendAndSend up north planet model bc, None
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
            up     : Lens<'a,V3d>
            north  : Lens<'a,V3d>
            planet : Lens<'a,Planet>
        }

    type MSmallConfig<'ma> =
        {            
            getNearPlane       : 'ma -> IMod<float>
            getHfov            : 'ma -> IMod<float>            
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

    let update<'a> (bigConfig : 'a) (smallConfig : SmallConfig<'a> ) (webSocket : BlockingCollection<string>) (model : DrawingModel) (act : Action) =
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
                    let planet = smallConfig.planet.Get(bigConfig)

                    let model, newSegment = addPoint up north planet hitFunction point model name webSocket
                    
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
                    let up     = smallConfig.up.Get(bigConfig)
                    let north  = smallConfig.north.Get(bigConfig)
                    let planet = smallConfig.planet.Get(bigConfig)

                    (finishAndAppendAndSend up north planet model webSocket) |> stash
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
            | Save p,_-> saveAnnotations p model

            | Load p,_ ->                
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
          let color =  
              model.selectedLeaves
                  |> ASet.map(fun x -> x.id = a.key)
                  |> ASet.contains true
                  |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else a.color.c)
          
          color

        let isSingleSelect (model : MGroupsModel) (a : MAnnotation) =
            model.singleSelectLeaf |> Mod.map( fun x -> 
                match x with 
                  | Some selected -> selected = a.key
                  | None -> false )

        let annotationMenu = 
            div [ clazz "ui dropdown item"] [
                text "Annotations"
                i [clazz "dropdown icon"][] 
                div [ clazz "menu"] [                    
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
                                
                button [clazz "ui icon button"; Dialogs.onSaveFile Save;
                        clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements'}]}));")] [
                    i [clazz "save icon"] [] ] |> UI.wrapToolTipBottom "Save Annotations"
                                
                button [clazz "ui icon button"; Dialogs.onChooseFiles Load;
                    clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ] [
                    i [clazz "folder outline icon"] [] ] |> UI.wrapToolTipBottom "Load Annotations"
                
                button [clazz "ui icon button"; onMouseClick (fun _ -> Clear)] [
                    i [clazz "file outline icon"] [] ] |> UI.wrapToolTipBottom "Clear Annotation List"

                button [clazz "ui icon button"; Dialogs.onSaveFile SaveCSV;
                        clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements as .csv'}]}));")] [
                    i [clazz "save icon inverted"] [] ] |> UI.wrapToolTipBottom "Save .csv"                                                                
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
              
              let visibleIcon = 
                amap {
                  yield onMouseClick (fun _ -> lift <| Groups.ToggleChildVisibility (a.key,path))
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
              
              let headerAttributes =
                amap {
                  yield onClick singleSelect
                  let! selected = isSingleSelect model a
                  yield if selected then style "text-transform:uppercase" else style "text-transform:none"
                } |> AttributeMap.ofAMap

              let headerText = 
                adaptive {
                  let! geometry = a.geometry
                  let! semantic = a.semantic

                  return 
                    match semantic with
                      | Semantic.None -> 
                        geometry  |> sprintf "%A"
                      | _ -> 
                        semantic  |> sprintf "%A"
              
                }

              div [clazz "item"] [
                Incremental.i iconAttributes AList.empty
                div [clazz "content"] [                  
                  let desc   = Mod.map2 (fun x y -> sprintf "#Points: %A | taken on: %A" x y) (a.points |> AList.count) a.surfaceName
                  
                  //header
                  yield Incremental.div (AttributeMap.ofList [clazz "header"; style ac]) (
                      alist {                          
                          yield div[][
                            Incremental.span headerAttributes ([Incremental.text headerText] |> AList.ofList)
                            Incremental.i visibleIcon AList.empty |> UI.wrapToolTip "Toggle Visible"
                            i [clazz "binoculars icon"; onClick (fun _ -> FlyToAnnotation a.key)][] |> UI.wrapToolTip "FlyTo"
                          ]
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
                    Action.GroupsMessage(Groups.SingleSelectLeaf (path, a.key, ""))

            let multiSelect = 
                fun (a:MAnnotation,path:list<Index>) -> 
                    Action.GroupsMessage(Groups.AddLeafToSelection (path, a.key, ""))

            let lift = fun (a:Groups.Action) -> (GroupsMessage a)

            let subNodes = group.subNodes |> AList.mapi (fun i v -> viewTree (i::path) v model lookup) 
            
            let annos = 
              group.leaves 
                |> AList.map(fun x -> lookup |> AMap.find x |> Mod.force) 
                |> viewAnnotationsInGroup path model singleSelect multiSelect lift                

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

        let config : Sg.innerViewConfig = 
            {
                nearPlane      = msmallConfig.getNearPlane      mbigConfig
                hfov           = msmallConfig.getHfov           mbigConfig                    
                arrowLength    = msmallConfig.getArrowLength    mbigConfig
                arrowThickness = msmallConfig.getArrowThickness mbigConfig
                dnsPlaneSize   = msmallConfig.getDnsPlaneSize   mbigConfig
            }
       
        let annotations =              
          annoSet 
            |> ASet.map(fun (_,a) -> 
              let c = UI.mkColor model.annotations a
              Sg.finishedAnnotation a c config view (a.geometry |> Mod.map(function | Geometry.Point -> true | _ -> false)))
            |> Sg.set               
                            
        let overlay = 
          Sg.ofList [
           // brush model.hoverPosition; 
            annotations
            Sg.drawWorkingAnnotation model.working
          ]

        let depthTest = annoSet |> ASet.map(fun (_,a) -> Sg.finishedAnnotationDiscs a config model.colorLegend view) |> Sg.set

        (overlay, depthTest)
            
