namespace Aardvark.Opc

module Shader =

    open Aardvark.Base
    open Aardvark.Base.IO
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    
    open FShade

    type Vertex =
        {
            [<Position>]                pos     : V4d            
            [<WorldPosition>]           wp      : V4d
            [<TexCoord>]                tc      : V2d
            [<Color>]                   c       : V4d
            [<Normal>]                  n       : V3d
            [<Semantic("Scalar")>]      scalar  : float
            [<Semantic("LightDir")>]    ldir    : V3d
        }

    [<ReflectedDefinition>]
    let transformNormal (n : V3d) =
        uniform.ModelViewTrafoInv.Transposed * V4d(n, 0.0)
            |> Vec.xyz
            |> Vec.normalize

    //type Vertex = {
    //    [<Position>]                pos     : V4d
    //    [<Normal>]                  n       : V3d
    //    [<BiNormal>]                b       : V3d
    //    [<Tangent>]                 t       : V3d
    //    [<Color>]                   c       : V4d
    //    [<TexCoord>]                tc      : V2d
    //    [<Semantic("LightDir")>]    ldir    : V3d
    //}

    let triangleFilter'' (input : Triangle<Vertex>) =
        triangle {
            let p0 = input.P0.wp.XYZ
            let p1 = input.P1.wp.XYZ
            let p2 = input.P2.wp.XYZ
            let cross = Vec.cross (p1 - p0) (p2 - p0)            
            let area = cross.LengthSquared
            let maxArea = uniform?TriangleSize
            let check = area < maxArea
            if check then
                yield input.P0 
                yield input.P1
                yield input.P2
        }

    let triangleFilter' (input : Triangle<Vertex>) =
        triangle {
            let A = input.P0.wp.XYZ
            let B = input.P1.wp.XYZ
            let C = input.P2.wp.XYZ

            let maxRel = uniform?TriangleSize

            let a = (B - A).Length < maxRel
            let b = (C - B).Length < maxRel
            let c = (A - C).Length < maxRel                        

            let check = a && b && c
            if check then
                yield input.P0 
                yield input.P1
                yield input.P2
        }

    let triangleFilter (input : Triangle<Vertex>) =
        triangle {
            let p0 = input.P0.wp.XYZ
            let p1 = input.P1.wp.XYZ
            let p2 = input.P2.wp.XYZ

            let maxRel = uniform?TriangleSize

            let a = (p1 - p0).Normalized
            let b = (p2 - p1).Normalized
            let c = (p0 - p2).Normalized          

            let alpha = (a.Dot b).Abs() < maxRel
            let beta  = (b.Dot c).Abs() < maxRel
            let gamma = (c.Dot a).Abs() < maxRel

            let check = (alpha && beta && gamma) || maxRel >= 0.999999999999999
            if check then
                yield input.P0 
                yield input.P1
                yield input.P2
        }

    let LoDColor  (v : Vertex) =
        fragment {
            if uniform?LodVisEnabled then
                let c : V4d = uniform?LoDColor
                let gamma = 1.0
                let grayscale = 0.2126 * v.c.X ** gamma + 0.7152 * v.c.Y ** gamma  + 0.0722 * v.c.Z ** gamma 
                return grayscale * c
            else return v.c
        }

    let selectionColor (v : Vertex) =
        fragment {
            if uniform?selected then
                //let c : V4d = uniform?selectionColor
                //return c * v.c
                let gamma = 1.3
                return V4d(v.c.X ** (1.0 / gamma), v.c.Y ** (1.0 / gamma),v.c.Z ** (1.0 / gamma), 1.0)
            else return v.c
        }

    let stableTrafo (v : Vertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.pos
            let wp = uniform.ModelTrafo * v.pos
            return { 
                v with
                    pos  = uniform.ProjTrafo * vp
                    wp   = wp
                    n    = transformNormal v.n
                    ldir = V3d.Zero - vp.XYZ |> Vec.normalize
            } 
        } 

    let fixPositionTrafo (v : Vertex) =
        vertex {
            let right = uniform.ViewTrafo.C0
            let up = uniform.ViewTrafo.C1
            let forw = uniform.ViewTrafo.C2
            //let camPos = uniform.CameraLocation
            //let mTrans = new M44d(
            //                1.0, 0.0, 0.0, 1.0,
            //                0.0, 1.0, 0.0, 1.0,
            //                0.0, 0.0, 1.0, 0.0,
            //                0.0, 0.0, 0.0,      1.0
            //            )
            let mRot = new M44d(
                        right.X, up.X, forw.X, 0.0,
                        right.Y, up.Y, forw.Y, 0.0,
                        right.Z, up.Z, forw.Z, 0.0,
                        0.0,     0.0,  0.0,    1.0
                    )
            
            //let pR = uniform.ProjTrafo.C0
            //let pU = uniform.ProjTrafo.C1
            //let pF = uniform.ProjTrafo.C2
            //let pT = uniform.ProjTrafo.C3

            //let mProj = new M44d(
            //                pR.X, pU.X, pF.X, 0.0,
            //                pR.Y, pU.Y, pF.Y, 0.0,
            //                pR.Z, pU.Z, pF.Z, 0.0,
            //                0.0,     0.0,  0.0,    1.0
            //            )
            //let mProjT = new M44d(
            //                1.0, 0.0, 0.0, pT.X,
            //                0.0, 1.0, 0.0, pT.Y,
            //                0.0, 0.0, 1.0, pT.Z,
            //                0.0, 0.0, 0.0, 1.0
            //            )
            //let transVp = mTrans * v.pos
            let newModelView = mRot * uniform.ModelTrafo
            let vp = newModelView * v.pos
            let wp = uniform.ModelTrafo * v.pos

            let n = newModelView.Inverse.Transposed * V4d(v.n, 0.0)
                        |> Vec.xyz
                        |> Vec.normalize
            return { 
                v with
                    pos  = vp //uniform.ProjTrafo *
                    wp   = wp
                    n    = n //transformNormal v.n
                    ldir = V3d.Zero - vp.XYZ |> Vec.normalize
            } 
        } 

    let stableLight (v : Vertex) =
        fragment {
            let n = v.n |> Vec.normalize
            let c = v.ldir |> Vec.normalize
             
            let diffuse = Vec.dot c n |> abs            
 
            return V4d(v.c.XYZ * diffuse, v.c.W)
        }

    let private colormap =
        sampler2d {
            texture uniform?ColorMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
    }

    let falseColor (v : Vertex) =
        fragment {           
            if uniform?falseColors then
                let range : V2d = uniform?MinMax
                let norm = V2d((v.scalar - range.X)/ (range.Y - range.X), 0.5)

                let c = colormap.Sample(norm).XYZ

                return v.c * V4d(c.X, c.Y, c.Z, 1.0)
            else
                return v.c
        }
