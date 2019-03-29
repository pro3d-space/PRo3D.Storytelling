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

open Aardvark.GeoSpatial.Opc

open Aardvark.UI
open Aardvark.UI.Primitives    
open PRo3D
open PRo3D.Drawing
open PRo3D.Groups
open PRo3D.Versioned

module Sg =                             
  open PRo3D.DrawingUtilities

  //open PRo3D.Surfaces.Mutable.SgSurfaceModule

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
          offset          : IMod<float>
      }

  let drawDns (anno : MAnnotation') (conf:innerViewConfig) (cl : MFalseColorsModel) (cam:IMod<CameraView>) =                         
    aset {                            
      let! dns = anno.dnsResults 
      match dns with
        | Some x -> 
          let center = anno.points |> AList.toMod |> Mod.map (fun list -> list.[PList.count list / 2])
          
          let color = FalseColorLegendApp.Draw.getColorDnS cl x.dipAngle
               
          let lengthFactor = 
            anno.points 
              |> AList.toMod 
              |> Mod.map(fun x -> (x.AsList |> Calculations.getDistance) / 3.0)

          //let lengthFactor =             
          //  anno.results 
          //    |> Mod.bind(fun y ->
          //      match y with
          //      | Some k -> k.length |> Mod.map(fun y -> y / 3.0 )
          //      | None -> Mod.constant 1.0)
                    
          let discRadius = conf.dnsPlaneSize |> Mod.map2 (*) lengthFactor
          let posTrafo = center |> Mod.map Trafo3d.Translation

          // disc
          let discTrafo =
              Mod.map2(fun (pln:Plane3d) pos -> (Trafo3d.RotateInto(V3d.ZAxis, pln.Normal) * pos)) x.plane posTrafo

          yield discISg color discRadius (discRadius |> Mod.map(fun d -> d * 0.01)) discTrafo

          let lineLength = conf.arrowLength |> Mod.map2 (*) lengthFactor //discRadius |> Mod.map((*) 1.5)
          
          let coneHeight = lineLength |> Mod.map((*) 0.2) //lineLength
          let coneRadius = coneHeight |> Mod.map((*) 0.3)

          // dip arrow       
          let! lineLength' = lineLength
          let! center'     = center

          let! dip = x.dipDirection
          let dipLine = 
            alist {
                yield center'
                yield center' + dip.Normalized * lineLength'
            }
          
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
      
  let getPolylinePoints (a : MAnnotation') =
    alist {                          
      let! hasSegments = (a.segments |> AList.count) |> Mod.map(fun x -> x > 0)
      if hasSegments |> not then
          yield! a.points
      else
        for segment in a.segments do
          let! startPoint = segment.startPoint
          let! endPoint = segment.endPoint
          yield  startPoint
          yield! segment.points
          yield  endPoint
    }

  let read a =
        StencilMode(StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)

  let write a =
        StencilMode(StencilOperationFunction.Replace, StencilOperationFunction.Replace, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)

  let pass0 = RenderPass.main
  let pass1 = RenderPass.after "outline" RenderPassOrder.Arbitrary pass0

                 
  let pickableLine (points : alist<V3d>) (offset : IMod<float>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>) (id: IMod<Guid>) (picking: bool) = 
    let edges = Sg.edgeLines false points trafo 
    let pickable = 
      adaptive {
        let! edg = edges
        let! t = trafo
        let! w = width
        let! points = points |> AList.toMod
        match edg.Length with          
          | 1 -> 
            let e = edg |> Array.head
            let cylinder = Cylinder3d(e.P0, e.P1, 0.1)
            return { shape = PickShape.Cylinder cylinder; trafo = t }
          | _ -> 
            let box = points |> Box3d
            if box.IsInvalid then failwith "hargh dem box"
            return { 
              shape = box |> PickShape.Box
              trafo = Trafo3d.Identity 
            } 
      }
   
    let pline =
      edges
        |> Aardvark.SceneGraph.SgPrimitives.Sg.lines color
        |> Sg.noEvents
        |> Sg.shader {
              do! Shader.stableTrafo
              do! DefaultSurfaces.vertexColor
              do! Shader.Shaders.thickLine
              do! Shader.Shaders.depthOffsetFS
          }      
        //|> Sg.pass pass0
        |> Sg.trafo trafo
        |> Sg.uniform "LineWidth" width
        |> Sg.uniform "DepthOffset" (
            offset |> Mod.map (fun depthWorld -> 
                let dLin = depthWorld / (100.0 - 0.1)
                printfn "linear depth offset: %A" dLin
                dLin
            ))        
        |> Sg.pickable' pickable
        |> Sg.noEvents
        |> Sg.withEvents [
              SceneEventKind.Down, (
                  fun sceneHit ->
                  if (picking) then
                      let id = id|> Mod.force
                      Log.warn "[AnnotationPicking] picked %A" id
                      true, Seq.ofList[PickAnnotation (sceneHit, (id))]
                  else
                      false, Seq.ofList[])
          ]
    
    pline

  let outlineSize (width:IMod<float>) =
    adaptive
        {
            let! w = width
            return (w + 3.0)
        }

  let outlineLine (points : alist<V3d>) (v:int) (color:IMod<C4b>) (trafo : IMod<Trafo3d>) (width : IMod<float>) (id: IMod<Guid>) =
        
        let edges = Sg.edgeLines false points trafo
        let sg = edges |> Aardvark.SceneGraph.SgPrimitives.Sg.lines color
     
        let regular =
            sg
            |> Sg.noEvents
            |> Sg.pass pass0
            |> Sg.shader {
                do! Shader.stableTrafo
                do! DefaultSurfaces.vertexColor
                do! Shader.Shaders.thickLine                
            }                  
            |> Sg.trafo trafo
            |> Sg.uniform "LineWidth" width            
            |> Sg.noEvents
            |> Sg.withEvents [
                SceneEventKind.Down, (
                    fun sceneHit ->
                    if (true) then
                        true, Seq.ofList[PickAnnotation (sceneHit, (id|> Mod.force))]
                    else
                        false, Seq.ofList[])
            ]
    
        let mask = 
            sg
            |> Sg.noEvents
            |> Sg.pass pass0
            |> Sg.stencilMode (Mod.constant (write v))
            |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])
            |> Sg.shader {
                 do! Shader.stableTrafo
                 do! DefaultSurfaces.vertexColor
                 do! Shader.Shaders.thickLine
             }                               
             |> Sg.trafo trafo
             |> Sg.uniform "LineWidth" width
             |> Sg.uniform "DepthOffset" (Mod.constant 0.001)

        let outl = 
            sg
            |> Sg.noEvents
            |> Sg.trafo trafo
            |> Sg.stencilMode (Mod.constant (read v))
            |> Sg.depthTest (Mod.constant DepthTestMode.None)
            |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])
            |> Sg.pass pass1
            |> Sg.shader {
                do! Shader.stableTrafo
                do! DefaultSurfaces.thickLine
                do! DefaultSurfaces.thickLineRoundCaps
                do! DefaultSurfaces.constantColor C4f.VRVisGreen
            }
            |> Sg.uniform "LineWidth" (outlineSize width)
            |> Sg.uniform "DepthOffset" (Mod.constant 0.0)
       
        Aardvark.SceneGraph.SgFSharp.Sg.ofSeq [regular; mask; outl] |> Sg.noEvents 

  
  let outlinePoint (points : alist<V3d>) (v:int) (color:IMod<C4b>) (width : IMod<float>) =
    //let p = 
    //    points
    //    |> AList.toMod
    //    |> Mod.map( fun x -> x.AsArray.[0])
        
    let sg = Sg.dots points width color //Sg.mkDotISg color (Mod.constant 5.0) p 
    let sgo = Sg.dots points (outlineSize width) (Mod.constant C4b.VRVisGreen) //Sg.mkDotISg color (outlineSize (Mod.constant 5.0)) p
    let regulars = 
        sg
         |> Sg.pass pass0
         |> Sg.shader {
                do! Shader.screenSpaceScale
                do! Shader.stableTrafo
                do! DefaultSurfaces.vertexColor
            }
    let masks = 
        sg
         |> Sg.pass pass0
         |> Sg.stencilMode (Mod.constant (write v))
         |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])
         |> Sg.shader {
               do! Shader.screenSpaceScale
               do! Shader.stableTrafo
               do! DefaultSurfaces.vertexColor
            }

    let outls = 
        sgo
         |> Sg.stencilMode (Mod.constant (read v))
         |> Sg.depthTest (Mod.constant DepthTestMode.None)
         |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])
         |> Sg.pass pass1
         |> Sg.uniform "LineWidth" (outlineSize width)
         |> Sg.shader {
                do! Shader.screenSpaceScale
                do! Shader.stableTrafo
                do! Shader.lines
                do! DefaultSurfaces.thickLine
                do! DefaultSurfaces.thickLineRoundCaps
                do! DefaultSurfaces.constantColor C4f.VRVisGreen
            }
    Aardvark.SceneGraph.SgFSharp.Sg.ofSeq [regulars; masks; outls] |> Sg.noEvents //regular; mask; 

  //TODO LF ... make a nice enum/union type for v, what is v?  
  let outline (points : alist<V3d>) (v:int) (color:IMod<C4b>) (trafo : IMod<Trafo3d>) (width : IMod<float>) (id: IMod<Guid>) =
    aset{
        let! length = points |> AList.count
        match length with 
        | 0  -> yield Sg.empty 
        | 1 -> yield outlinePoint points v color width
        | _ -> yield outlineLine points v color trafo width id
    } |> Sg.set

  let drawWorkingAnnotation (offset : IMod<float>) (anno : IMod<Option<MAnnotation'>>)  = 

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
          Sg.lines polyPoints offset color thickness trafo //polyline
          //Sg.dots polyPoints (Mod.constant C4b.VRVisGreen) // debug sample points
          Sg.dots points thickness color                     //support points
      ]         
  
  let drawText' (view : IMod<CameraView>) conf (text:IMod<string>)(anno : MAnnotation') = 
    let points = anno.points |> AList.toMod
    let pos = 
        points |> Mod.map(fun a -> a |> PList.toList |> List.head) 

    Sg.text view conf.nearPlane conf.hfov pos anno.modelTrafo text anno.textsize.value

  let drawText (view : IMod<CameraView>) conf (anno : MAnnotation') = 
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

  let finishedAnnotation (anno : MAnnotation') (c : IMod<C4b>) (conf:innerViewConfig) 
    (view : IMod<CameraView>) (showPoints : IMod<bool>) (picked:IMod<bool>)  =
      let points = getPolylinePoints anno      
      let dots   = showPoints |> optional (Sg.getDotsIsg anno.points (anno.thickness.value |> Mod.map(fun x -> x+2.0)) c anno.geometry)
     
      let azimuth =
        adaptive { 
          let! results = anno.dnsResults 
          let! x = 
            match results with
            | Some r -> r.dipAzimuth
            | None -> Mod.constant Double.NaN

          return x
        }

      let azimuthText = (drawText' view conf (azimuth |> Mod.map(fun x -> sprintf "%.2f" x)) anno)
        
      let texts = 
        anno.text 
          |> Mod.map (String.IsNullOrEmpty >> not) 
          |> optional (drawText view conf anno)

      let sgs = ASet.unionMany' [dots; texts ]

      
      //let selectionColor = Mod.map2(fun x color -> if x then C4b.VRVisGreen else color) picked c

      let pickingLines = 
        pickableLine points conf.offset c anno.thickness.value anno.modelTrafo (anno.key |> Mod.constant) true
        //aset
        //    {
        //        let! picked = picked
        //        match picked with //TODO LF ... if then else is okay for bools
        //            | true -> yield outline points 1 c anno.modelTrafo anno.thickness.value (anno.key |> Mod.constant)
        //            |_-> yield pickingLine points c anno.thickness.value anno.modelTrafo (anno.key |> Mod.constant) true
        //    } |> Sg.set
           
      let selectionGeometry = 
        aset
            {
                let! picked = picked
                if picked then
                  yield outline points 1 c anno.modelTrafo anno.thickness.value (anno.key |> Mod.constant)                  
            } |> Sg.set

      Sg.ofList [
          //Sg.lines points c anno.thickness.value anno.modelTrafo
          selectionGeometry
          //azimuthText
          pickingLines
          Sg.set sgs 
      ] |> Sg.onOff anno.visible
 
  let finishedAnnotationDiscs (anno : MAnnotation') (conf:innerViewConfig) (cl : MFalseColorsModel) (cam:IMod<CameraView>) =
     anno.showDns |> optional (drawDns anno conf cl cam) |> Sg.set |> Sg.onOff anno.visible
        

