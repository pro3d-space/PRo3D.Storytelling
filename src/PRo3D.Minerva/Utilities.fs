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

module Files = 

    let getProperties (ins : Instrument) (insId:string) (row:CsvRow) : PRo3D.Minerva.Properties = 
        match ins with
            | Instrument.MAHLI ->   
                {
                MAHLI_Properties.id = insId |> FeatureId
                beginTime =  DateTime.Parse(row.GetColumn "{Timestamp}Start_time")
                endTime =  DateTime.Parse(row.GetColumn "{Timestamp}Stop_time")
                } |> Properties.MAHLI
            | Instrument.FrontHazcam | Instrument.FrontHazcamL | Instrument.FrontHazcamR ->
                {
                FrontHazcam_Properties.id = insId |> FeatureId
                beginTime =  DateTime.Parse(row.GetColumn "{Timestamp}Start_time")
                endTime =  DateTime.Parse(row.GetColumn "{Timestamp}Stop_time")
                } |> Properties.FrontHazcam
            | Instrument.Mastcam | Instrument.MastcamL | Instrument.MastcamR ->
                {
                Mastcam_Properties.id = insId |> FeatureId
                beginTime =  DateTime.Parse(row.GetColumn "{Timestamp}Start_time")
                endTime =  DateTime.Parse(row.GetColumn "{Timestamp}Stop_time")
                } |> Properties.Mastcam
            | Instrument.APXS ->
              {
                APXS_Properties.id = insId |> FeatureId
              } |> Properties.APXS       
            | Instrument.ChemLib ->
              {
                ChemCam_Properties.id = insId |> FeatureId           
              } |> Properties.ChemCam       
            | Instrument.ChemRmi ->
              {
                ChemCam_Properties.id = insId |> FeatureId            
              } |> Properties.ChemCam       
            | Instrument.NotImplemented ->
              {
                APXS_Properties.id = insId |> FeatureId
              } |> Properties.APXS               
            | _ -> failwith "encountered invalid instrument from parsing"

    let getFeature (row:CsvRow) : option<Feature> =

        let id' = row.GetColumn "{Key}Product_id"
        match id' with
         | "" -> None
         | _-> 
            let inst = row.GetColumn "{Category}Instrument_id"
            let instrument = inst |> Model.toInstrument
            let sol' = (row.GetColumn "{Value}{Sol}Planet_day_number").AsInteger()

            let omega = (row.GetColumn "{Angle}Omega").AsFloat()
            let phi = (row.GetColumn "{Angle}Phi").AsFloat()
            let kappa = (row.GetColumn "{Angle}Kappa").AsFloat()

            let x = (row.GetColumn "{CartX}X").AsFloat()
            let y = (row.GetColumn "{CartY}Y").AsFloat()
            let z = (row.GetColumn "{CartZ}Z").AsFloat()

            let instName = row.GetColumn "{Category}Instrument_name"

            let props = getProperties instrument inst row

            let geo = 
                {
                    typus = Typus.Point
                    coordinates = V3d(omega, phi, kappa) |> List.singleton
                    positions = V3d(x, y, z) |> List.singleton
                }

            let feature = 
                {
                  id          = id'
                  instrument  = instrument
                  typus       = Feature
                  boundingBox = Box2d.Invalid//feature?bbox |> parseBoundingBox
                  properties  = props
                  geometry    = geo
                  sol         = sol'
                } 
            Some feature

    open System.IO    
    open PRo3D.Base
    let loadDataFile =
        let cachePath = @".\MinervaData\dump.cache"
        let path = @".\MinervaData\dump.csv"
        match (File.Exists path, File.Exists cachePath) with
         | (true, false) -> 
            let allData = CsvFile.Load(path).Cache()

            let features = 
                allData.Rows
                    |> Seq.choose( fun x -> getFeature x )
                
            {
              name        = "dump"
              typus       = Typus.FeatureCollection    
              boundingBox = Box2d.Invalid
              features    = features |> PList.ofSeq
            } |> Serialization.save cachePath
         | (_, true) -> Serialization.loadAs cachePath
         | _ -> 
            Log.error "[Minerva] sth. went wrong with dump.csv"
            Initial.data        

module Shader = 

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

    //type PSVertex = { [<TexCoord; Interpolation(InterpolationMode.Sample)>] tc : V2d }

    let pointSpriteFragment (v : pointVertex) =
        fragment {
            let tc = v.tc // + 0.00000001 * v.sp

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
            
            
    let drawIndexedGeometryPoints (sgfeatures : MSgFeatures) (pointSize:IMod<float>) (outline:bool) = 

        let pointsF = 
            sgfeatures.positions
                |> Mod.map2(
                fun (h:Trafo3d) points -> 
                    points |> Array.map(fun (x:V3d) -> (h.Backward.TransformPos x).ToV3f() ) 
                    ) sgfeatures.trafo

        let color = 
            match outline with
             | true -> pointsF
                        |> Mod.map( fun points -> points |> Array.map(fun (p:V3f) -> C4b.VRVisGreen ))
             | false -> sgfeatures.colors

        Sg.draw IndexedGeometryMode.PointList
            |> Sg.vertexAttribute DefaultSemantic.Positions (pointsF )
            |> Sg.vertexAttribute DefaultSemantic.Colors (color)
            |> Sg.effect [
                 DefaultSurfaces.stableTrafo |> toEffect; 
                DefaultSurfaces.pointSprite |> toEffect;
                Shader.pointSpriteFragment |> toEffect
                ]
            |> Sg.trafo sgfeatures.trafo
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

    let outlineSize (width:IMod<float>) =
        adaptive
            {
                let! w = width
                return (w + 3.0)
            }

    let read a =
        StencilMode(StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)

    let write a =
        StencilMode(StencilOperationFunction.Replace, StencilOperationFunction.Replace, StencilOperationFunction.Keep, StencilCompareFunction.Greater, a, 0xffu)

    let pass0 = RenderPass.main
    let pass1 = RenderPass.after "outline" RenderPassOrder.Arbitrary pass0

    let outlinePoints (sgfeatures : MSgFeatures) (pointSize:IMod<float>) =

        let sg = drawIndexedGeometryPoints sgfeatures pointSize false //Sg.dots points width color 
        let sgo = drawIndexedGeometryPoints sgfeatures (outlineSize pointSize) true  //Sg.dots points (outlineSize width) (Mod.constant C4b.VRVisGreen) 
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
             |> Sg.stencilMode (Mod.constant (write 1))
             |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Stencil])
             |> Sg.shader {
                   do! Shader.screenSpaceScale
                   do! Shader.stableTrafo
                   do! DefaultSurfaces.vertexColor
                }

        let outls = 
            sgo
             |> Sg.stencilMode (Mod.constant (read 1))
             |> Sg.depthTest (Mod.constant DepthTestMode.None)
             |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])
             |> Sg.pass pass1
             |> Sg.uniform "LineWidth" (outlineSize pointSize)
             |> Sg.shader {
                    do! Shader.screenSpaceScale
                    do! Shader.stableTrafo
                    do! Shader.lines
                    do! DefaultSurfaces.thickLine
                    do! DefaultSurfaces.thickLineRoundCaps
                    do! DefaultSurfaces.constantColor C4f.VRVisGreen
                }
        Aardvark.SceneGraph.SgFSharp.Sg.ofSeq [masks; outls] |> Sg.noEvents //regular; mask; 

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
        let invScaleTrafo = computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale
        //    //computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale //(Mod.constant 0.05)
      
        let billboardTrafo = 
            adaptive {
                let! v = view
                //let! size = size
                let! scaleTrafo = invScaleTrafo
                let modelt = scaleTrafo * modelTrafo
      
                return screenAligned v.Forward v.Up modelt
            }      
      
        Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
            |> Sg.noEvents
            |> Aardvark.UI.FShadeSceneGraph.Sg.shader {
            do! Shader.stableTrafo
            }                  
            |> Aardvark.UI.``F# Sg``.Sg.trafo billboardTrafo  

