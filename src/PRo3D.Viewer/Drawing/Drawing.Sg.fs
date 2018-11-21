namespace PRo3D.DrawingApp

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

module Sg =                             
  let discISg color size height trafo =
      Sg.cylinder 30 color size height              
        |> Sg.noEvents
        |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
        |> Sg.uniform "Size" size
        |> Sg.shader {
            //do! Shader.screenSpaceScale
            do! Shader.stableTrafo
            do! DefaultSurfaces.vertexColor
            do! Shader.stableLight
        }
        |> Sg.trafo(trafo)

  let coneISg color radius height trafo =  
      Sg.cone 30 color radius height
         |> Sg.noEvents
         //|> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZW))
         //|> Sg.uniform "Size" ( radius |> Mod.map(fun x -> x * 700.0))
         //|> Sg.uniform "posTrafo" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward))
         |> Sg.shader {                   
             do! Shader.stableTrafo
             do! DefaultSurfaces.vertexColor
             do! Shader.stableLight
         }
         |> Sg.trafo(trafo) 
         
  type innerViewConfig =
      {
          nearPlane       : IMod<float>
          hfov            : IMod<float>                
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
          
          yield Sg.scaledLines dipLine color conf.arrowThickness posTrafo 

          let coneTrafo = 
            lineLength |>
              Mod.map(fun s -> Trafo3d.RotateInto(V3d.ZAxis, dip) * Trafo3d.Translation(center' + dip.Normalized * s))

          yield coneISg color coneRadius coneHeight coneTrafo                

          //strikes lines
          let! strike = x.strikeDirection
          let strikeLine1 =
            alist {                                                                
                yield center' - strike.Normalized * lineLength'
                yield center' + strike.Normalized * lineLength'
            }                                            

          //yield Sg.lines strikeLine1 (Mod.constant C4b.Red) conf.arrowThickness posTrafo anno.key
          yield Sg.scaledLines strikeLine1 (Mod.constant C4b.Red) conf.arrowThickness posTrafo
        | None -> ()            
    } |> Sg.set
      
  let getPolylinePoints (a : MAnnotation) =
      alist {                
          let! hasSegments = 
            a.segments 
              |> AList.count 
              |> Mod.map (fun x ->
                Log.line "[Drawing] drawing poly with %A segments" x
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

  let pickingLines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>) (id: IMod<Guid>) (picking : bool) = 
    match (points |> AList.toArray).Length with
        | 0  -> Sg.empty //| 1
        |  _ ->
            let edges = Sg.edgeLines false points trafo 
            //let pickable = 
            //    adaptive {
            //        let! edg = edges
            //        let! w = width
            //        match edg.Length with
            //            | 0 -> let sphere = Sphere3d((points |> AList.toArray).[0], 0.1) // point
            //                   return { shape = PickShape.Sphere sphere; trafo = Trafo3d.Identity } // line
            //            | 1 -> 
            //              let e = edg.[0]
            //              let cylinder = Cylinder3d(e.P0, e.P1, 0.1)
            //              return { shape = PickShape.Cylinder cylinder; trafo = Trafo3d.Identity }
            //            | _ -> return { shape = PickShape.Box (Box3d(points |> AList.toArray)); trafo = Trafo3d.Identity } 
           //     }
   
            let pline =
                edges
                  |> Aardvark.SceneGraph.SgPrimitives.Sg.lines color
                  |> Sg.noEvents
                  |> Sg.shader {
                        do! Shader.stableTrafo
                        do! DefaultSurfaces.vertexColor
                        do! Shader.thickLine    
                    }                               
                  |> Sg.trafo trafo
                  |> Sg.uniform "LineWidth" width
                  |> Sg.uniform "DepthOffset" (Mod.constant 0.2)
           //       |> Sg.pickable' pickable
                  |> Sg.noEvents
                  |> Sg.withEvents [
                        SceneEventKind.Down, (
                            fun sceneHit ->
                            if (picking) then
                                true, Seq.ofList[PickAnnotation (sceneHit, (id|> Mod.force))]
                            else
                                false, Seq.ofList[])
                    ]
    
            pline
               

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

      let id = 
          adaptive {
              let! anno = anno
              match anno with
                  | Some a -> return a.key
                  | None -> return Guid.Empty
          }
          
      let color     = Mod.bindOption anno C4b.VRVisGreen   (fun a -> a.color.c) 
      let thickness = Mod.bindOption anno 1.0              (fun a -> a.thickness.value)             
      let trafo     = Mod.bindOption anno Trafo3d.Identity (fun a -> a.modelTrafo)
                             
      Sg.ofList [
          Sg.lines polyPoints color thickness trafo //polyline
          //pickingLines polyPoints color thickness trafo id true
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
          //Sg.lines points c anno.thickness.value anno.modelTrafo
          pickingLines points c anno.thickness.value anno.modelTrafo (anno.key |> Mod.constant) true
          Sg.set sgs 
      ] |> Sg.onOff anno.visible

  //let finishedPickingAnnotation (anno : MAnnotation) (c : IMod<C4b>) (conf:innerViewConfig) (view : IMod<CameraView>) (dots : IMod<bool>) =
  //  let points = anno |> getPolylinePoints     
  //  let dots  = dots |> optional (Sg.dots anno.points c)
     
  //  let texts = 
  //      anno.text 
  //          |> Mod.map (String.IsNullOrEmpty >> not) 
  //          |> optional (drawText view conf anno)

  //  let sgs = ASet.unionMany' [dots; texts]

    

  //  Sg.ofList [
  //      Sg.cylinders points
  //      Sg.set sgs 
  //  ] |> Sg.onOff anno.visible

  let finishedAnnotationDiscs (anno : MAnnotation) (conf:innerViewConfig) (cl : MFalseColorsModel) (cam:IMod<CameraView>) =
     anno.showDns |> optional (drawDns anno conf cl cam) |> Sg.set |> Sg.onOff anno.visible
        

