namespace PRo3D.Minerva

//open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Events


open Aardvark.GeoSpatial.Opc
open System


open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text 
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
//open Aardvark.Application.Utilities
open FShade
open Aardvark.UI
open FSharp.Data

//module Files = 

//    let getFeature (row:CsvRow) : Feature =
//        let id' = row.GetColumn "{Key}Product_id"
//        let inst = row.GetColumn "{Category}Instrument_id"
//        let sol' = row.GetColumn "{Value}{Sol}Planet_day_number"

//        let omega = row.GetColumn "{Angle}Omega"
//        let phi = row.GetColumn "{Angle}Phi"
//        let kappa = row.GetColumn "{Angle}Kappa"

//        let x = row.GetColumn "{CartX}X"
//        let y = row.GetColumn "{CartY}Y"
//        let z = row.GetColumn "{CartZ}Z"

//        {
//          id          = id'
//          instrument  = inst |> Model.toInstrument
//          typus       = FeatureCollection
//          boundingBox = Box2d.Invalid//feature?bbox |> parseBoundingBox
//          properties  = prop
//          geometry    = { geometry with positions = position |> List.singleton }
//          sol         = sol'
//        } 

//    let loadDataDump = 
//        let directory = @".\MinervaData"
//        let allData = CsvFile.Load(@".\MinervaData\dump.csv").Cache()

//        allData.Rows
//            |> Seq.map( fun x -> getFeature x )

//        //allData

module Shader = 

    type UniformScope with
        member x.PointSize : float = uniform?PointSize

    type pointVertex =
        {
            [<Position>] pos : V4d
            [<PointSize>] p : float
            [<Color>] c : V4d
            [<TexCoord; Interpolation(InterpolationMode.Sample)>] tc : V2d
        }

    let constantColor (color : V4d) (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            return { v with c = color; p = ps }
        }

    let differentColor (v : pointVertex) =
        vertex {
            let ps : float = uniform?PointSize
            return { v with c = v.c; p = ps }
        }

    let pointTrafo (v : pointVertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return { 
                v with 
                    pos = uniform.ViewProjTrafo * wp
            }
        }

    type PSVertex = { [<TexCoord; Interpolation(InterpolationMode.Sample)>] tc : V2d }

    let pointSpriteFragment (v : pointVertex) =
        fragment {
            let tc = v.tc // + 0.00000001 * v.sp

            let c = 2.0 * tc - V2d.II
            if c.Length > 1.0 then
                discard()

            return v
        }


module Drawing =

    let createHarrisPoints (positions : V3d[]) (size : IMod<float>) = // (color : C4b) =
        //let colors = Array.create positions.Length color
        let pointSize = Mod.constant 50.0
        //let c = V4d((float)color.R/255.0, (float)color.G/255.0, (float)color.B/255.0, (float)color.A/255.0)

        Aardvark.UI.``F# Sg``.Sg.ofList [
                 Sg.draw IndexedGeometryMode.PointList
            ]
             |> Sg.vertexAttribute DefaultSemantic.Positions (positions |> Mod.constant)
             //|> Sg.vertexAttribute DefaultSemantic.Colors (colors |> Mod.constant)
             //|> Sg.vertexAttribute "Positions" vertices
             |> Sg.uniform "PointSize" size
            |> Sg.shader {
                do! Shader.pointTrafo
                do! Shader.constantColor V4d.OIOI //c 
                //do! DefaultSurfaces.vertexColor |> toEffect
            }

       

    let createHarrisPoints2 (positions : IMod<V3d[]>)  (colors : IMod<C4b[]>) (size : IMod<float>) =
        //let pointSize = Mod.constant 80.0

        Aardvark.UI.``F# Sg``.Sg.ofList [
                 Sg.draw IndexedGeometryMode.PointList
            ]
             |> Sg.vertexAttribute DefaultSemantic.Positions (positions)
             |> Sg.vertexAttribute DefaultSemantic.Colors (colors)
             |> Sg.uniform "PointSize" size
            |> Sg.shader {
                do! Shader.pointTrafo
                do! Shader.differentColor 
                do! DefaultSurfaces.vertexColor |> toEffect
            }

    let testPoints (positions : V3d[])  (colors : V4d[]) =

        let pointSize = Mod.constant 70.0

        Sg.draw IndexedGeometryMode.PointList
            |> Sg.vertexAttribute DefaultSemantic.Positions (positions |> Mod.constant)
            |> Sg.vertexAttribute DefaultSemantic.Colors (colors |> Mod.constant)
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect; 
                //Shader.pointTrafo |> toEffect; 
                //Shader.differentColor |> toEffect; 
                DefaultSurfaces.pointSprite |> toEffect; 
                DefaultSurfaces.vertexColor |> toEffect;
                Shader.pointSpriteFragment |> toEffect
                ]
            |> Sg.uniform "PointSize" pointSize

    let testPoints' (positions : IMod<V3d[]>)  (colors : IMod<C4b[]>) =

        let pointSize = Mod.constant 10.0

        Sg.draw IndexedGeometryMode.PointList
            |> Sg.vertexAttribute DefaultSemantic.Positions (positions )
            |> Sg.vertexAttribute DefaultSemantic.Colors (colors)
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect; 
                //Shader.pointTrafo |> toEffect; 
                //Shader.differentColor |> toEffect; 
                DefaultSurfaces.pointSprite |> toEffect; 
                DefaultSurfaces.vertexColor |> toEffect;
                Shader.pointSpriteFragment |> toEffect
                ]
            |> Sg.uniform "PointSize" pointSize
        

    let createIndexedGeometry (positions : V3f[])  (colors : C4b[]) =
        let index = [|0..positions.Length|]
        let indexAttributes =
            let def = [
                DefaultSemantic.Positions, (positions) :> Array
                DefaultSemantic.Colors, (colors |> Array.map (fun x -> x)) :> Array
             ]

            def |> SymDict.ofList 


        let geometry =
            IndexedGeometry(
                Mode              = IndexedGeometryMode.PointList,
                IndexArray        = index,
                IndexedAttributes = indexAttributes
            )  
            
        let pointSize = Mod.constant 70.0

        geometry
         |> Sg.ofIndexedGeometry
         |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect; 
                //Shader.pointTrafo |> toEffect; 
                //Shader.differentColor |> toEffect; 
                DefaultSurfaces.pointSprite |> toEffect; 
                DefaultSurfaces.vertexColor |> toEffect;
                Shader.pointSpriteFragment |> toEffect
                ]
         |> Sg.uniform "PointSize" pointSize
           

    let drawPoint color size (pos : IMod<V3d>) = 
        Sg.sphere 4 color size
            |> Sg.noEvents
            |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
            //|> Sg.uniform "PointSize" size
            //|> Sg.uniform "WorldPos" pos

    let mkDotISg color (size : IMod<float>) (pos : IMod<V3d>) = 
        Sg.sphere 2 color (Mod.constant 1.0)
            |> Sg.noEvents
            |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
            |> Sg.uniform "Size" size
            |> Sg.uniform "WorldPos" pos

    let dots (points : alist<V3d>) (size:IMod<float>) (color : IMod<C4b>) = //TODO TO refactor, put into base prj
    //let dots (pts : IMod<V3d[]>) (size:IMod<float>) (color : IMod<C4b>) = 
        aset {
            //let! points = pts 
            //for p in points do
            for p in points |> ASet.ofAList do
               // let scale = computeScale view (Mod.constant p) 5.0                               
                yield mkDotISg color (size) (Mod.constant p)
        } 
        |> Sg.set
        |> Sg.effect [
            toEffect <| Shader.screenSpaceScale
            toEffect <| Shader.stableTrafo
            toEffect <| DefaultSurfaces.vertexColor          
        ]

    let indexedGeometryDots (points : alist<V3d>) (size:IMod<float>) (color : IMod<C4b>) =       
        let points' = points |> AList.toMod |> Mod.map(fun x -> x |> PList.toArray)
        let colors = points' |> Mod.map2(fun c x -> Array.create x.Length c) color
      
        Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute DefaultSemantic.Positions points'         
        |> Sg.vertexAttribute DefaultSemantic.Colors colors
        |> Sg.effect [
            toEffect Shader.stableTrafo
            toEffect DefaultSurfaces.vertexColor
            //toEffect DefaultSurfaces.pointSprite
        ]
        |> Sg.uniform "PointSize" size
  
 
    let computeInvariantScale (view : IMod<CameraView>) (near : IMod<float>) (p:V3d) (size:IMod<float>) (hfov:IMod<float>) =
        adaptive {
            //let! p = p
            let! v = view
            let! near = near
            let! size = size
            let! hfov = hfov
            let hfov_rad = Conversion.RadiansFromDegrees(hfov)
         
            let wz = Fun.Tan(hfov_rad / 2.0) * near * size
            let dist = V3d.Distance(p, v.Location)
      
            return ( wz / near ) * dist
        }

    let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) =
        let right = up.Cross forw
        let rotTrafo = 
            new Trafo3d(
                new M44d(
                    right.X, up.X, forw.X, 0.0,
                    right.Y, up.Y, forw.Y, 0.0,
                    right.Z, up.Z, forw.Z, 0.0,
                    0.0,     0.0,  0.0,    1.0
                ),
                new M44d(
                    right.X, right.Y, right.Z, 0.0,
                    up.X,    up.Y,    up.Z,    0.0,
                    forw.X,  forw.Y,  forw.Z,  0.0,
                    0.0,     0.0,     0.0,     1.0
                )
        )
        rotTrafo * modelt
        

    let text (view : IMod<CameraView>) (near : IMod<float>) (hfov:IMod<float>) (pos:V3d) (modelTrafo:Trafo3d) (text:IMod<string>) (size:IMod<float>) =
        let invScaleTrafo = //Trafo3d.Scale(0.5)
            computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale //(Mod.constant 0.05)
      
        let billboardTrafo = 
            adaptive {
                let! v = view
                let modelt = modelTrafo
      
                return screenAligned v.Forward v.Up modelt
            }      
      
        Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
            |> Sg.noEvents
            |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
            do! Shader.stableTrafo
            }            
            |> Aardvark.UI.``F# Sg``.Sg.trafo invScaleTrafo //(Mod.constant invScaleTrafo)
            |> Aardvark.UI.``F# Sg``.Sg.trafo billboardTrafo  

