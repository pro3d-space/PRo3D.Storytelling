namespace PRo3D

open System
open System.IO

open Aardvark
open Aardvark.Base

open PRo3D.Groups
open PRo3D.Drawing
open PRo3D.Viewer
open PRo3D.Versioned

module ViewerUtils =
  module IO =
    open Chiron

    let loadRoverData (m:Model) =
          let vps = ViewPlan.loadRoverData m.scene.viewPlans m.scene.scenePath
          { m with scene = { m.scene with viewPlans = vps } }
    
    let tryLoadAnnotations (path:string) =
      try
        let annotationGroups = (Serialization.loadAs<PRo3D.Groups.saveAnnotations> path)

        let flat' = 
          annotationGroups.annotations.flat 
            |> HMap.map(fun _ v -> 
              match v with 
                | Leaf.Annotations a ->
                  Log.warn "encountered legacy annotations; converting to v%d" Annotation'.current
                  a |> Annotation'.convert |> Leaf.Annotations' 
                | Leaf.Annotations' a -> 
                  if a.version < Annotation'.current then
                    Log.warn "encountered versioned annotations of v%d; updating to v%d" a.version Annotation'.current
                  elif a.version > Annotation'.current then
                    Log.error "version mixup - not supported encountered v%d; current v%A" a.version Annotation'.current

                  a |> Leaf.Annotations'
                | _ -> v)

        let annoModel = { annotationGroups.annotations with flat = flat' }

        Some { annotationGroups with annotations = annoModel }        
      with _ ->
        Log.error "[loadAnnotations:] couldn't load %s" path
        None       

    let loadLastAnnotations (m:Model) = 
      match  m.scene.scenePath with
        | Some p -> 
          let ap = DrawingApp.getAnnotationPath p
          let groups = tryLoadAnnotations ap 
          match groups with          
              | Some g -> { m with drawing = { m.drawing with annotations = g.annotations; colorLegend = g.colorLegend } }
              | None -> m
        |None -> m

    let saveVersioned model =
      let path = "./annotations.json"
      let pathgGrouping = "./annotations.grouping"
      
      Log.line "[Drawing] Writing annotations"
      model.annotations.flat 
          |> HMap.toList 
          |> List.map(fun (_,b) -> b |> Leaf.toAnnotation) // |> Annotation'.convert)
          |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty |> IO.writeToFile path
      
      Log.line "[Drawing] Writing grouping"
      let annotations' = { model.annotations with flat = HMap.empty } |> Serialization.save pathgGrouping

      { model with annotations = annotations' }

    let loadVersioned (model : DrawingModel) =
      let path = "./annotations.json"
      let pathgGrouping = "./annotations.grouping"

      Log.line "[Drawing] Reading annotations"
      let (annos : list<Annotation'>) = path |> IO.readFromFile |> Json.parse |> Json.deserialize
      let annos = annos |> List.map(fun x -> (x.key,x |> Leaf.Annotations')) |> HMap.ofList
      
      Log.line "[Drawing] Reading grouping"
      let grouping = Serialization.loadAs<GroupsModel> pathgGrouping
      let grouping : GroupsModel = { grouping with flat = annos }

      { model with annotations = grouping }

    let saveSceneAndAnnotations (path:string) (m:Model) = 
        if path.IsEmptyOrNull() then m
        else
          let s' = { m.scene with scenePath = Some path}
          let drawing = DrawingApp.saveSceneAnnotations path m.drawing
          { m with scene = Serialization.save path s'; drawing = drawing } |> Model.stashAndSaveRecent path

