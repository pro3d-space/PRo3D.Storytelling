namespace PRo3D

module DipAndStrike =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    
    open Aardvark.Base.Incremental
    open Aardvark.Fitting

    open Aardvark.UI
    open Aardvark.UI.Primitives    

    let projectOntoPlane (x:V3d) (n:V3d) = (x - (x * n)).Normalized

    let computeAzimuth (dir:V3d) (north:V3d) (planeNormal:V3d) =
        let east = north.Cross(planeNormal)
        let eastWestPlane = new Plane3d(east, 0.0) //separates west (-) from east  (+)
        let northSouthPlane = new Plane3d(north, 0.0) //separates sout (-) from north  (+)

        let coords = new V2d(eastWestPlane.Height(dir), northSouthPlane.Height(dir))
        let angle = Fun.Atan(coords.Y / coords.X).DegreesFromRadians().Abs()

        match coords.X.Sign() with
                | -1 -> match coords.Y.Sign() with
                            | 1 -> 270.0 + angle
                            | _ -> 270.0 - angle 
                | _ -> match coords.Y.Sign() with
                            | -1 -> angle + 90.0
                            | _ -> 90.0 - angle

    let bearing (up:V3d) (north : V3d) (dir:V3d) =
        computeAzimuth dir north up

    let pitch (up:V3d) (dir:V3d) =
        let ground = new Plane3d(up.Normalized, 0.0)
        Math.Asin(ground.Height(dir)).DegreesFromRadians()

    let computeStandardDeviation avg (input : List<float>) =

      let sosq = 
        input 
          |> List.map(fun (x:double) ->
              let k = (x.Abs() - avg)
              Math.Pow(k,2.0))
          |> List.sum

      Math.Sqrt (sosq / float (input.Length - 1))

    let calculateDnsErrors (points:plist<V3d>) =
      let points = points |> PList.filter(fun x -> not x.IsNaN)

      if points.Count < 3 then []
      else
        let v3dArray = points.AsList |> List.toArray
        let mutable plane = PlaneFitting.FitPlane3dLeastSquares(v3dArray)

        let distances = 
          points 
            |> PList.toList
            |> List.map(fun x -> (plane.Height x))

        distances
      

    let calculateDipAndStrikeResults (up:V3d) (north : V3d) (points:plist<V3d>)=

        let points = points |> PList.filter(fun x -> not x.IsNaN)

        match points.Count with 
            | x when x > 2 ->

                let v3dArray = points.AsList |> List.toArray // points.toArray not possible because of: int -> V3d[]

                //let p = v3dArray.[0]
               // let up = p.Normalized        

                let mutable plane = PlaneFitting.FitPlane3dLeastSquares(v3dArray)
                let bb = new Box3d(points)

                let distances = 
                  points 
                    |> PList.toList
                    |> List.map(fun x -> (plane.Height x).Abs())

                let sos = distances |> List.map(fun x -> x * x) |> List.sum /// (float distances.Length)
                
                let avg = distances |> List.average
                let max = distances |> List.max
                let min = distances |> List.min
             
                let std = distances |> computeStandardDeviation avg

                Log.line("[dipandStrike]: avg %f; max %f; min %f; std: %f; sols: %f") avg max min std sos

                let horP = new Plane3d(up, V3d.Zero)

                //correct plane orientation - check if normals point in same direction
                let height = horP.Height(plane.Normal)
                let planeNormal = 
                  match height.Sign() with
                  | -1 -> plane.Normal.Negated
                  | _  -> plane.Normal

                plane.Normal <- planeNormal

                //strike
                let strike = up.Cross(planeNormal).Normalized

                //dip vector 
                let dip = strike.Cross(planeNormal).Normalized

                //dip plane incline .. maximum dip angle
                let v = strike.Cross(up).Normalized

                let dns = {
                    plane           = plane
                    dipAngle        = Math.Acos(v.Dot(dip)).DegreesFromRadians()
                    dipDirection    = dip
                    strikeDirection = strike
                    dipAzimuth      = computeAzimuth v north up
                    strikeAzimuth   = computeAzimuth strike north up
                    centerOfMass    = bb.Center
                    error           = 
                      { 
                        average = avg
                        min = min
                        max = max
                        stdev = std
                        sumOfSquares = sos
                      }
                }
                Some dns
            | _ -> None 
        

    let dipColor (dipAngle:IMod<float>) (min:IMod<float>) (max:IMod<float>) =
        adaptive {
            let! dipAngle = dipAngle
            let! min = min
            let! max = max

            let range = new Range1d(min, max)
            let hue = (dipAngle - range.Min) / range.Size
            let hsv = HSVf((1.0 - hue) * 0.625, 1.0, 1.0)

            return hsv.ToC3f().ToC4b()
        }
    
    let viewUI (model : MAnnotation) =

        let da   = Mod.bindOption model.dnsResults Double.NaN (fun a -> a.dipAngle)
        let daz  = Mod.bindOption model.dnsResults Double.NaN (fun a -> a.dipAzimuth)
        let staz = Mod.bindOption model.dnsResults Double.NaN (fun a -> a.strikeAzimuth)
        
        require Html.semui (
            Html.table [ 
                Html.row "Dipping Angle:"       [Incremental.text (da   |> Mod.map  (fun d -> sprintf "%.2f°" (d)))]
                Html.row "Dipping Orientation:" [Incremental.text (daz  |> Mod.map  (fun d -> sprintf "%.2f°" (d)))]
                Html.row "Strike Orientation:"  [Incremental.text (staz |> Mod.map  (fun d -> sprintf "%.2f°" (d)))]
            ]
        )

