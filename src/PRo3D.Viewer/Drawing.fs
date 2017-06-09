namespace PRo3D

module Drawing =
    open Newtonsoft.Json    
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.Application
    open Aardvark.SceneGraph
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.Rendering.Text
    //open Aardvark.VRVis

    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph

    type Action =
        | SetSemantic      of Semantic
        | SetGeometry      of Geometry
        | SetProjection    of Projection
        | SetExportPath    of string
        | Move             of V3d
        | Exit    
        | AddPoint         of V3d
        | KeyDown of key : Keys
        | KeyUp of key : Keys      
        | Export

    let finishAndAppend (model : DrawingModel) = 
        let anns = match model.working with
                            | Some w -> model.annotations |> PList.append w
                            | None -> model.annotations
        { model with working = None; annotations = anns }

    let update (model : DrawingModel) (act : Action) =
        match (act, model.draw) with
            | KeyDown Keys.LeftCtrl, _ ->                     
                    { model with draw = true }
            | KeyUp Keys.LeftCtrl, _ -> 
                    {model with draw = false; hoverPosition = None }
            | Move p, true -> 
                    { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPoint p, true -> 
                    let model =
                        match model.offset with
                            | None -> { model with offset = Some p }
                            | Some _ -> model

                    let working = 
                        match model.working with
                                | Some w ->     
                                    let k = { w with points = w.points |> PList.append p }
                                    Log.line "working contains %d points" k.points.Count
                                    k
                                | None ->                                     
                                    { (Annotation.make model.projection model.geometry model.semantic) with points = PList.ofList [p]}//add annotation states

                    let model = { model with working = Some working }

                    let model = match (working.geometry, (working.points |> PList.count)) with
                                    | Geometry.Point, 1 -> model |> finishAndAppend
                                    | Geometry.Line, 2 -> model |> finishAndAppend
                                    | _ -> model

                    model                 
                    
            | KeyDown Keys.Enter, _ -> 
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
            | KeyDown Keys.D0, _ -> 
                    {model with semantic = Semantic.Horizon0 }               
            | KeyDown Keys.D1, _ -> 
                    {model with semantic = Semantic.Horizon1 }               
            | KeyDown Keys.D2, _ -> 
                    {model with semantic = Semantic.Horizon2 }               
            | KeyDown Keys.D3, _ -> 
                    {model with semantic = Semantic.Horizon3 }               
            | KeyDown Keys.D4, _ -> 
                    {model with semantic = Semantic.Horizon4 }               
            | SetExportPath s, _ ->
                    { model with exportPath = s }
            | Export, _ ->
                    //let path = Path.combine([model.exportPath; "drawing.json"])
                    //printf "Writing %i annotations to %s" (model.annotations |> PList.count) path
                    //let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> JsonConvert.SerializeObject
                    //Serialization.writeToFile path json 
                    
                    model
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

        let viewAnnotations (model:MDrawingModel) = 
            Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"]) (
                            
                        alist {                                                                     
                            for a in model.annotations do 
                                        
                                let! sem = a.semantic
                                let c = Mod.force a.color //Annotation.color.[int sem]

                                let bgc = sprintf "color: %s" (Html.ofC4b c)
                                    
                                yield div [clazz "item"] [
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

    //module DipAndStrike = 

    //    let projectOntoPlane (x:V3d) (n:V3d) = (x - (x * n)).Normalized

    //    let computeAzimuth (dir:V3d) (north:V3d) (planeNormal:V3d) =
    //        let east = north.Cross(planeNormal)
    //        let eastWestPlane = new Plane3d(east, 0.0) //separates west (-) from east  (+)
    //        let northSouthPlane = new Plane3d(north, 0.0) //separates sout (-) from north  (+)

    //        let coords = new V2d(eastWestPlane.Height(dir), northSouthPlane.Height(dir))
    //        let angle = Fun.Atan(coords.Y / coords.X).DegreesFromRadians().Abs()

    //        let nangle = match coords.X >= 0.0 with
    //                        | true -> match coords.Y >= 0.0 with
    //                                    | true -> 90.0 - angle
    //                                    | false -> angle + 90.0
    //                        | false -> match coords.Y > 0.0 with
    //                                    | true -> 270.0 + angle
    //                                    | false -> 270.0 - angle
    //        nangle

            
  
    //    let calculateDipAndStrikeResults (points:plist<V3d>) =
    //        let up = V3d.OOI
    //        let north = V3d.IOO
    //        let plane = Aardvark.VRVis.PlaneFitting.FitPlane3dLS(points);
    //        let bb = new Box3d(points);

    //        let horP = new Plane3d(up, V3d.Zero);

    //         //correct plane orientation - check if normals point in same direction
    //        let height = horP.Height(plane.Normal);
    //        if (height.Sign() = -1) then
    //                plane.Normal = plane.Normal.Negated;

    //        let n = plane.Normal;

    //        //strike
    //        let strike = up.Cross(n).Normalized;

    //        //dip vector 
    //        let dip = strike.Cross(n).Normalized;

    //        //dip plane incline .. maximum dip angle
    //        let v = strike.Cross(up).Normalized;

    //        let dns = {
    //            plane = plane
    //            dipAngle = Math.Acos(v.Dot(dip)).DegreesFromRadians()
    //            dipDirection = dip
    //            strikeDirection = strike
    //            dipAzimuth = computeAzimuth(v, north, up)
    //            strikeAzimuth = computeAzimuth(strike, north, up)
    //            centerOfMass = bb.Center
    //        }
    //        dns

    module Sg =        

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
                    |> Sg.shader {
                        do! Aardvark.Opc.Shader.stableTrafo
                        do! DefaultSurfaces.vertexColor
                        do! DefaultSurfaces.simpleLighting
                    }
                    |> Sg.noEvents
                    |> Sg.trafo(trafo) 
        
        let canvas =             
            Sg.sphere' 8 (new C4b(247,127,90)) 20.0
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.requirePicking
                |> Sg.noEvents 
                    |> Sg.withEvents [
                        Sg.onMouseMove (fun p -> (Action.Move p))
                        Sg.onClick(fun p -> Action.AddPoint p)
                        Sg.onLeave (fun _ -> Action.Exit)
                    ]  
                |> Sg.onOff (Mod.constant true)
              //  |> Sg.map DrawingMessage

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
                hovered |> Mod.map (function o -> match o with 
                                                    | Some t-> t
                                                    | None -> Trafo3d.Scale(V3d.Zero))

            mkDotISg (Mod.constant C4b.Red) (Mod.constant 0.05) trafo
       
        let dots (points : alist<V3d>) (color : IMod<C4b>) (view : IMod<CameraView>) =            
            
            aset {
                for p in points |> ASet.ofAList do
                    yield mkDotISg color (computeScale view (Mod.constant p) 5.0) (Mod.constant (Trafo3d.Translation(p)))
            } 
            |> Sg.set
           
        let lines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>)= 
            edgeLines false points trafo
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
   
        let annotation (anno : IMod<Option<MAnnotation>>)(view : IMod<CameraView>)(trafo:IMod<Trafo3d>) = 
            //alist builder?
            let points = 
                anno |> AList.bind (fun o -> 
                    match o with
                        | Some a -> a.points
                        | None -> AList.empty
                ) 
            
                
            let withDefault (m : IMod<Option<'a>>) (f : 'a -> IMod<'b>) (defaultValue : 'b) = 
                let defaultValue = defaultValue |> Mod.constant
                m |> Mod.bind (function | None -> defaultValue | Some v -> f v)

            let color = 
                withDefault anno (fun a -> a.color) C4b.VRVisGreen

            let thickness = 
                anno |> Mod.bind (function o -> match o with
                                                | Some a -> a.thickness.value
                                                | None -> Mod.constant 1.0)

            [lines points color thickness trafo; dots points color view]
         //   [dots points color view]

        let annotation' (anno : MAnnotation)(view : IMod<CameraView>)(trafo:IMod<Trafo3d>) = 
            let count = anno.segments |> AList.toMod |> (Mod.map PList.count)
            let c = Mod.force count
            let points = match c with       
                            | 0 -> anno.points
                            | _ -> anno.segments |> AList.concat
            [lines points anno.color anno.thickness.value trafo;
             dots anno.points anno.color view] 
            |> ASet.ofList

        let view (model:MDrawingModel) (cam:IMod<CameraView>) =        
            // order is irrelevant for rendering. change list to set,
            // since set provides more degrees of freedom for the compiler
            let annoSet = ASet.ofAList model.annotations 

            let trafo = model.offset |> Mod.map (fun d -> match d with | Some p -> Trafo3d.Translation p | None -> Trafo3d.Identity )

            let annotations =
                aset {
                    for a in annoSet do
                        yield! annotation' a cam trafo
                } |> Sg.set
                                
            //canvas
            [brush model.hoverPosition; annotations] @ annotation model.working cam trafo
            |> Sg.ofList
            