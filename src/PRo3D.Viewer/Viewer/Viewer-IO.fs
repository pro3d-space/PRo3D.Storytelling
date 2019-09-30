namespace PRo3D

open System
open System.IO

open Aardvark
open Aardvark.Base

open PRo3D.Groups
open PRo3D.Drawing
open PRo3D.Viewer
open PRo3D.Versioned

module ViewerIO =  
    open Chiron
    open Aardvark.Rendering.GL
    open PRo3D.Base

    //rover data
    let loadRoverData (m:Model) =
      let vps = ViewPlanApp.loadRoverData m.scene.viewPlans m.scene.scenePath
      { m with scene = { m.scene with viewPlans = vps } }
    
    let changeExtension (ext:string) (scenePath:string) = 
      let p = System.IO.Path.GetDirectoryName(scenePath)
      let name = System.IO.Path.GetFileNameWithoutExtension(scenePath) + ext
      p + "\\" + name

    //let getAnnotationPath_depr (scenePath:string) = 
    //  scenePath |> changeExtension ".ann" 

    //let getAnnotationPath (scenePath:string) = 
    //  scenePath |> changeExtension ".ann.json" 

    //annotations
    type ScenePaths =
      {
         scene               : string
         annotationGroups    : string
         annotationVersioned : string
         annotationDepr      : string
      }
    module ScenePaths =
      let create(scenepath : string) =
        {
          scene               = scenepath
          annotationGroups    = scenepath |> changeExtension ".ann" 
          annotationVersioned = scenepath |> changeExtension ".ann.json" 
          annotationDepr      = scenepath |> changeExtension ".ann_old" 
        }

    let saveVersionedAnnotations (model : DrawingModel) (paths : ScenePaths) =      
        
        Log.startTimed "[Drawing] Writing annotations"
        model.annotations.flat 
        |> HMap.toList 
        |> List.map(fun (_,b) -> b |> Leaf.toAnnotation) // |> Annotation'.convert)
        |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty |> IO.writeToFile paths.annotationVersioned
        Log.stop()
        
        Log.startTimed "[Drawing] Writing grouping"
        {
            annotations = { model.annotations with flat = HMap.empty }  //clears flat annotations from pickled
            colorLegend = model.dnsColorLegend
        } |> Serialization.save paths.annotationGroups |> ignore
        Log.stop()
        
        model
        
    let tryLoadAnnotations (scenePath : string) : option<saveAnnotations> =
        try        
            (DrawingUtilities.IO.loadAnnotations scenePath) |> Some
        with e ->        
            Log.error "[ViewerIO] couldn't load %A" e
            None
    
    let loadAnnotations (m:Model) = 
        match m.scene.scenePath with
        | Some p ->           
            let scenePaths = p |> ScenePaths.create
            let groups = scenePaths.annotationGroups |> tryLoadAnnotations
            match groups with          
            | Some g -> { m with drawing = { m.drawing with annotations = g.annotations; dnsColorLegend = g.colorLegend } }
            | None -> m
        |None -> m

      
    let loadLastFootPrint (m:Model) = 
        let fp = 
            m.scene.viewPlans.selectedViewPlan 
            |> Option.bind(fun vp -> 
                vp.selectedInstrument |> Option.map(fun instr -> (instr,vp)))
            |> Option.map(fun (instr, vp) -> 
                FootPrint.updateFootprint instr vp.position m.scene.viewPlans)
            |> Option.defaultValue PRo3d.ViewPlanModel.initFootPrint
       
        { m with footPrint = fp }
        
        
    let saveSceneAndAnnotationGroups (path:string) (m:Model) = 
        if path.IsEmptyOrNull() then m
        else
            let s' = { m.scene with scenePath = Some path}
            let drawing = 
                (path |> ScenePaths.create) 
                    |> saveVersionedAnnotations m.drawing

            let scene = s' |> Serialization.save path

            { m with scene = scene; drawing = drawing } 
            |> Model.stashAndSaveRecent path

