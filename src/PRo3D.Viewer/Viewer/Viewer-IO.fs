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

    //rover data
    let loadRoverData (m:Model) =
      let vps = ViewPlan.loadRoverData m.scene.viewPlans m.scene.scenePath
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

    let saveVersioned (model : DrawingModel) (paths : ScenePaths) =      
      
      Log.line "[Drawing] Writing annotations"
      model.annotations.flat 
          |> HMap.toList 
          |> List.map(fun (_,b) -> b |> Leaf.toAnnotation) // |> Annotation'.convert)
          |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty |> IO.writeToFile paths.annotationVersioned
      
      Log.line "[Drawing] Writing grouping"
      {
        annotations = { model.annotations with flat = HMap.empty }  //clears flat annotations from pickled
        colorLegend = model.colorLegend
      } |> Serialization.save paths.annotationGroups |> ignore

      model
        
    let tryLoadAnnotations (paths : ScenePaths) =
      try
        //load groups.ann
        let annotationGroups = paths.annotationGroups |> Serialization.loadAs<PRo3D.Groups.saveAnnotations>
          
        //if hmap non empty convert from legacy to versioned
        let flat = annotationGroups.annotations.flat
        let flat' =
          if flat |> HMap.isEmpty then flat
          else 
            let k = 
              flat 
              |> HMap.map(fun _ v -> 
                match v with 
                  | Leaf.Annotations a -> //convert
                    Log.warn "[ViewerIO] encountered legacy annotations; converting to v%d" Annotation'.current
                    a |> Annotation'.convert |> Leaf.Annotations' 
                  | _ -> v |> sprintf "[ViewerIO] encountered invalid type in anno groups %A" |> failwith
                )
            //save legacy annotation data
            annotationGroups |> Serialization.save paths.annotationDepr |> ignore
            k
            
        //load ann.json
        let annos =
          if (File.Exists paths.annotationVersioned) then
            Log.line "[ViewerIO] Reading annotations"
            let (annos : list<Annotation'>) = paths.annotationVersioned |> IO.readFromFile |> Json.parse |> Json.deserialize
            annos |> List.map(fun x -> (x.key,x |> Leaf.Annotations')) |> HMap.ofList
          else
            HMap.empty

        //hmap union converted and versioned
        let flat' = annos |> HMap.union flat'                

        let annoModel = { annotationGroups.annotations with flat = flat' }
        Some { annotationGroups with annotations = annoModel }
      with _ ->
        Log.error "[ViewerIO] couldn't load %s" paths.annotationGroups
        None       
    
    let loadLastAnnotations (m:Model) = 
      match m.scene.scenePath with
        | Some p ->           
          let groups = p |> ScenePaths.create |> tryLoadAnnotations
          match groups with          
              | Some g -> { m with drawing = { m.drawing with annotations = g.annotations; colorLegend = g.colorLegend } }
              | None -> m
        |None -> m

    //let saveAnnotationGroups (model:DrawingModel) (path:string) = 
    //  if path.IsEmptyOrNull() then model
    //  else
    //    //prohibit writing pickled flat annotations        
    //    let sa = 
    //        {
    //            annotations = { model.annotations with flat = HMap.empty }
    //            colorLegend = model.colorLegend
    //        }            
    //    let saveA = Serialization.save path sa
    //    { model with annotations = saveA.annotations; colorLegend = saveA.colorLegend }        
    
    

    let saveSceneAndAnnotationGroups (path:string) (m:Model) = 
        if path.IsEmptyOrNull() then m
        else
          let s' = { m.scene with scenePath = Some path}
          let drawing = (path |> ScenePaths.create) |> saveVersioned m.drawing
          { m with scene = Serialization.save path s'; drawing = drawing } |> Model.stashAndSaveRecent path

