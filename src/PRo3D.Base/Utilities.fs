namespace PRo3D.Base

open Aardvark.Base
open Aardvark.SceneGraph
open Aardvark.UI
open MBrace.FsPickler
open MBrace.FsPickler.Json

module Utilities =
  type ClientStatistics =
    {
        session         : System.Guid
        name            : string
        frameCount      : int
        invalidateTime  : float
        renderTime      : float
        compressTime    : float
        frameTime       : float
    }

  let takeScreenshot baseAddress (width:int) (height:int) name folder =
        let wc = new System.Net.WebClient()
        
        let clientStatistic = 
            let path = sprintf "%s/rendering/stats.json" baseAddress //sprintf "%s/rendering/stats.json" baseAddress
            Log.line "[Screenshot] querying rendering stats at: %s" path
            let result = wc.DownloadString(path)
            let clientBla : list<ClientStatistics> =
                Pickler.unpickleOfJson  result
            match clientBla.Length with
                | 1 -> clientBla // clientBla.[1] 
                | _ -> failwith "no client bla"

        for cs in clientStatistic do
            let screenshot =            
                sprintf "%s/rendering/screenshot/%s?w=%d&h=%d&samples=4" baseAddress cs.name width height
            Log.line "[Screenshot] Running screenshot on: %s" screenshot    

            match System.IO.Directory.Exists folder with
              | true -> ()
              | false -> System.IO.Directory.CreateDirectory folder |> ignore
            
           // let filename = cs.name + name
            wc.DownloadFile(screenshot,Path.combine [folder; name])

module Shader = 
    open FShade
    open Aardvark.Base.Rendering

    type UniformScope with
        member x.PointSize : float = uniform?PointSize

    type pointVertex =
        {
            [<Position>] pos : V4d
            [<PointSize>] p : float
            [<Color>] c : V4d
            [<TexCoord; Interpolation(InterpolationMode.Sample)>] tc : V2d
            [<SourceVertexIndex>] i : int
        }

    let constantColor (color : V4d) (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            return { v with c = color; p = ps }
        }

    let singleColor (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            let c  : V4d   = uniform?SingleColor
            return { v with c = c; p = ps }
        }

    let differentColor (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            return { v with c = v.c; p = ps }
        }

    let pointTrafo (v : pointVertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.pos
            return { 
                v with 
                    pos = uniform.ProjTrafo * vp
            }
        }

    let pointSpriteFragment (v : pointVertex) =
        fragment {
            let tc = v.tc

            let c = 2.0 * tc - V2d.II
            if c.Length > 1.0 then
                discard()

            return v
        }

    let lines (t : Triangle<pointVertex>) =
        line {
            yield t.P0
            yield t.P1
            restartStrip()
            
            yield t.P1
            yield t.P2
            restartStrip()

            yield t.P2
            yield t.P0
            restartStrip()
        }

module Sg =
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering

    let drawColoredPoints pointsF colors pointSize = 
        Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
        |> Sg.vertexAttribute DefaultSemantic.Colors colors
        |> Sg.uniform "PointSize" pointSize
        |> Sg.effect [
            toEffect Shader.pointTrafo 
            toEffect Shader.differentColor
            toEffect DefaultSurfaces.pointSprite
            toEffect Shader.pointSpriteFragment
          ]

    let drawSingleColorPoints (pointsF : IMod<V3f[]>) (color : IMod<V4d>) pointSize offset = 
      Sg.draw IndexedGeometryMode.PointList
      |> Sg.vertexAttribute DefaultSemantic.Positions pointsF
      |> Sg.uniform "PointSize" pointSize
      |> Sg.uniform "SingleColor" color
      |> Sg.uniform "DepthOffset" (
            offset |> Mod.map (fun depthWorld -> depthWorld / (100.0 - 0.1)))
      |> Sg.effect [
            toEffect Shader.pointTrafo
            toEffect Shader.singleColor
            toEffect DefaultSurfaces.pointSprite
            toEffect Shader.pointSpriteFragment
            toEffect Aardvark.GeoSpatial.Opc.Shader.Shaders.depthOffsetFS
      ]
    
    let stablePoints (trafo : IMod<Trafo3d>) (positions : IMod<V3d[]>) =
        positions 
        |> Mod.map2(fun (t : Trafo3d) x -> 
            x |> Array.map(fun p -> (t.Backward.TransformPos(p)) |> V3f)) trafo

    let stablePoints' (positions : IMod<V3d[]>) : (IMod<V3f[]> * IMod<Trafo3d>) = 
        let trafo =
            positions 
            |> Mod.map Array.tryHead 
            |> Mod.map (function | Some p -> Trafo3d.Translation p | None -> Trafo3d.Identity)

        stablePoints trafo positions, trafo

        