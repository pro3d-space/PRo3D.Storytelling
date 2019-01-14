namespace PRo3D

module DrawingUtilities =

    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    
    open PRo3D
    
    
    module Calculations =
        open PRo3D.Base

        //let getHeightDelta (p:V3d) (upVec:V3d) = (p * upVec).Length

        let getHeightDelta2 (p:V3d) (upVec:V3d) (planet:Planet) = 
            CooTransformation.getHeight p upVec planet

        let calcResultsPoint (model:Annotation) (upVec:V3d) (planet:Planet) =
            //let p = point //trafo.Forward.TransformPos(point)
            {   height = 0.0
                heightDelta = 0.0
                avgAltitude = CooTransformation.getAltitude model.points.[0] upVec planet
                length = 0.0
                wayLength = 0.0
                bearing = 0.0
                slope = 0.0
                }

        let getDistance (points:list<V3d>) = points
                                                |> List.pairwise
                                                |> List.map (fun (a,b) -> V3d.Distance(a,b))
                                                |> List.sum

        let getSegmentDistance (s:Segment) = getDistance
                                                [ yield s.startPoint ; 
                                                   for p in s.points do
                                                        yield p; 
                                                  yield s.endPoint 
                                                ] 
   
        let getWay (segments:plist<Segment>) = [ for s in segments do
                                                    yield getSegmentDistance s
                                               ] |> List.sum
                                                               

        let calcResultsLine (model:Annotation) (upVec:V3d) (northVec:V3d) (planet:Planet) =
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

            {   height = (heights |> List.max) - (heights |> List.min)
                heightDelta = Fun.Abs (heights.[hcount-1] - heights.[0])
                avgAltitude = (heights |> List.average)
                length = dist
                wayLength = way
                bearing = bearing
                slope = slope
                }

        let calculateAnnotationResults (model:Annotation) (upVec:V3d) (northVec:V3d) (planet:Planet) =
            match model.points.Count with
                | x when x > 1 -> calcResultsLine model upVec northVec planet
                | _ -> calcResultsPoint model upVec planet


        let recalcBearing (model:Annotation) (upVec:V3d) (northVec:V3d) = 
            match model.results with 
              | Some r ->
                        let count = model.points.Count
                        match count with
                          | x when x > 1 ->
                                    let line = new Line3d(model.points.[0], model.points.[count-1])
                                    let bearing = DipAndStrike.bearing upVec northVec line.Direction.Normalized
                                    Some {r with bearing = bearing }
                          | _ -> Some r
              | _-> None

