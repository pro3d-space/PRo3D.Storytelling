namespace Aardvark.Opc

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.SceneGraph
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.Semantics

module NaivePatchLoD =

    let colors =
        [ 0, C4b.Green
          1, C4b.Blue
          2, C4b.Yellow
          3, C4b.Red
          4, C4b.VRVisGreen
          5, C4b.Yellow ] |> Map.ofList

    let patch2Sg (baseDir : string) (depth : int) (patch : Patch) =
        let tex = patch.info.Textures |> List.head 
        let tiff = Path.combine [baseDir; "Images"; tex.fileName]

        let extensions = [ ".dds"; ".tif"; ".tiff"]

        let rec tryFindTex exts path =
            match exts with
                | x::xs -> 
                    let cuzrrent = System.IO.Path.ChangeExtension(path,x)
                    if File.Exists cuzrrent then cuzrrent else tryFindTex xs path
                | [] -> failwithf "texture not found: %s" path

        let textureImage = tryFindTex extensions tiff

        let ex = Path.GetExtension textureImage
        if ex = ".dds" then printfn "asdfasdasd"

        (Patch.load baseDir patch.info).Value
            |> fst
            |> Sg.ofIndexedGeometry
            |> Sg.uniform "LoDColor" (Mod.constant (Map.find depth colors))
            |> Sg.diffuseFileTexture' textureImage true
            |> Sg.trafo (Mod.constant patch.info.Local2Global)


    let computeScreenSpaceArea (b : Box3d) (viewProj : Trafo3d) =
        let viewSpacePoints =
            b.ComputeCorners()
                |> Array.map (fun v ->
                    viewProj.Forward.TransformPosProj(v).XY
                )

        let poly = viewSpacePoints |> Polygon2d

        let clipped = 
            poly.ComputeConvexHullIndexPolygon().ToPolygon2d()//.ConvexClipped(Box2d(-V2d.II, V2d.II))

        clipped.ComputeArea()


    let toSg (preTrafo : Trafo3d) (s : QTree<Patch>) : ISg =
        failwith ""
//        let rec mk d s =
//            match s with
//                | Node(p, children) ->
//                    let decider (scope : NaiveLod.LodScope) = 
////                        let bb = p.info.
////                        let mvp : Trafo3d = scope.ModelViewTrafo.GetValue()
//
//                        let bbT = p.info.GlobalBoundingBox.Transformed(preTrafo) //.Transformed(scope.trafo)
//                        let cam = scope.cameraPosition
////                        let screenSpaceBox = 
////                            bb.ComputeCorners() 
////                            |> Array.map (fun p -> mvp.Forward.TransformPosProj(p))
////                            |> Box3d
//                        let d = (bbT.Center - cam).Length 
//
//                        let area = computeScreenSpaceArea bbT (scope.ViewProjTrafo.GetValue())
//
//                        //printfn "d=%A" d
//                        area > 3.5
//                        //screenSpaceBox.SizeX < 0.3 && screenSpaceBox.SizeY < 0.3
//                    let self = patch2Sg
//                    NaiveLod.Sg.loD (patch2Sg d p) (children |> Array.map (mk (d+1)) |> Sg.ofSeq) decider
//                | Leaf v -> patch2Sg d v 
//        mk 0 s
//    
//

