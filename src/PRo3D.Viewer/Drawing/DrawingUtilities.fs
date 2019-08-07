namespace PRo3D

open PRo3D.Versioned
open PRo3D.Base
open PRo3D.Groups
open Chiron
open Chiron.Mapping

module DrawingUtilities =

    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open System
    
    open PRo3D
    
    
    module Calculations =
        open PRo3D.Base

        //let getHeightDelta (p:V3d) (upVec:V3d) = (p * upVec).Length

        let getHeightDelta2 (p:V3d) (upVec:V3d) (planet:Planet) = 
            CooTransformation.getHeight p upVec planet

        let calcResultsPoint (model:Annotation') (upVec:V3d) (planet:Planet) : AnnotationResults' =
            //let p = point //trafo.Forward.TransformPos(point)
            { 
              version = AnnotationResults'.current
              height = 0.0
              heightDelta = 0.0
              avgAltitude = CooTransformation.getAltitude model.points.[0] upVec planet
              length = 0.0
              wayLength = 0.0
              bearing = 0.0
              slope = 0.0
            }
        

        let getDistance (points:list<V3d>) = 
          points
            |> List.pairwise
            |> List.map (fun (a,b) -> V3d.Distance(a,b))
            |> List.sum

        let getSegmentDistance (s:Segment) = 
          getDistance
            [ 
              yield s.startPoint
              for p in s.points do
                 yield p
              yield s.endPoint 
            ] 
   
        let getWay (segments:plist<Segment>) = 
          [ for s in segments do
               yield getSegmentDistance s
          ] |> List.sum
                                                               

        let calcResultsLine (model:Annotation') (upVec:V3d) (northVec:V3d) (planet:Planet) : AnnotationResults' =
            let count = model.points.Count
            let dist = V3d.Distance(model.points.[0], model.points.[count-1])
            let way = match model.segments.Count with
                            | x when x > 0 -> getWay model.segments
                            |_ -> dist

            let heights = model.points 
                           // |> PList.map(fun x -> model.modelTrafo.Forward.TransformPos(x))
                            |> PList.map(fun p -> getHeightDelta2 p upVec planet ) 
                            |> PList.toList

            let hcount = heights.Length

            let line = new Line3d(model.points.[0], model.points.[count-1])
            let bearing = DipAndStrike.bearing upVec northVec line.Direction.Normalized
            let slope = DipAndStrike.pitch upVec line.Direction.Normalized

            {   
              version     = AnnotationResults'.current
              height      = (heights |> List.max) - (heights |> List.min)
              heightDelta = Fun.Abs (heights.[hcount-1] - heights.[0])
              avgAltitude = (heights |> List.average)
              length      = dist
              wayLength   = way
              bearing     = bearing
              slope       = slope
            }

        let calculateAnnotationResults (model:Annotation') (upVec:V3d) (northVec:V3d) (planet:Planet) : AnnotationResults' =
            match model.points.Count with
                | x when x > 1 -> calcResultsLine model upVec northVec planet
                | _ -> calcResultsPoint model upVec planet

        let recalcBearing (model:Annotation') (upVec:V3d) (northVec:V3d) = 
           match model.results with 
             | Some r ->
               let count = model.points.Count
               match count with
                 | x when x > 1 ->
                   let line = new Line3d(model.points.[0], model.points.[count-1])
                   let bearing = DipAndStrike.bearing upVec northVec line.Direction.Normalized
                   Some {r with bearing = bearing }
                 | _ -> Some r
             | None -> None

    module IO = 
      let changeExtension (ext:string) (scenePath:string) = 
        let p = System.IO.Path.GetDirectoryName(scenePath)
        let name = System.IO.Path.GetFileNameWithoutExtension(scenePath) + ext
        p + "\\" + name

      type AnnotationsPaths =
        {
           groups    : string
           versioned : string
           depr      : string
        }
      module AnnotationsPaths =
        let create(annotationPath : string) =
          {
            groups    = annotationPath
            versioned = annotationPath |> changeExtension ".ann.json"
            depr      = annotationPath |> changeExtension ".ann_old" 
          }

      let loadAnnotations path = 
        let annoPaths = AnnotationsPaths.create path

        let annotationGroups = annoPaths.groups |> Serialization.loadAs<PRo3D.Groups.saveAnnotations>
          
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
            annotationGroups |> Serialization.save annoPaths.depr |> ignore
            k
            
        //load .ann.json
        let annos =
          if (System.IO.File.Exists annoPaths.versioned) then
            Log.startTimed "[ViewerIO] Reading annotations"
            let (annos : list<Annotation'>) = 
              annoPaths.versioned 
                |> IO.readFromFile 
                |> Json.parse 
                |> Json.deserialize
        
            Log.stop()
        
            annos |> List.map(fun x -> (x.key,x |> Leaf.Annotations')) |> HMap.ofList
          else
            HMap.empty
        
        //hmap union converted and versioned
        let flat' = annos |> HMap.union flat'
        
        Log.line "[DrawingIO] loaded %d annotations" flat'.Count

        let flatGroupIds = annotationGroups.annotations.rootGroup |> Group.flatten
        let floatingLeaves = 
          flat'.Values 
            |> Seq.toList            
            |> List.filter(fun x -> (HSet.contains x.id flatGroupIds) |> not)
            
        
        Log.error "[ViewerIO] found %d floating leaves" (floatingLeaves.Length)
        
        //Log.warn "[ViewerIO] floating leaves %A" (floatingLeaves |> List.map(fun x -> x.id))
        
        let annoModel = 
          { annotationGroups.annotations with flat = flat' } |> Groups.addLeaves List.empty (floatingLeaves |> PList.ofList)
        
        annoModel
         
      
