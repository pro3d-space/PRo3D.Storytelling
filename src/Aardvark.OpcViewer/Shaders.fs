namespace Aardvark.Opc

module Shader =

    open Aardvark.Base
    open Aardvark.Base.IO
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    
    open FShade

    type Vertex =
        {
            [<Position>]           pos     : V4d            
            [<WorldPosition>]      wp      : V4d
            [<TexCoord>]           tc      : V2d
            [<Color>]              c       : V4d
            [<Normal>]             n       : V3d
            [<Semantic("Scalar")>] scalar  : float
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
                    pos = uniform.ProjTrafo * vp
                    wp = wp
                    n = uniform.NormalMatrix * v.n                    
            } 
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
