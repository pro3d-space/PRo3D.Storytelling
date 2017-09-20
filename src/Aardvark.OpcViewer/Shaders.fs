namespace Aardvark.Opc

module Shader =

    open Aardvark.Base
    open Aardvark.Base.IO
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    
    open FShade

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
                let c : V4d = uniform?selectionColor
                return c * v.c
            else return v.c
        }

    let stableTrafo (v : Vertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.pos
            let wp = uniform.ModelTrafo * v.pos
            return {
                pos = uniform.ProjTrafo * vp
                wp = wp
                n = uniform.NormalMatrix * v.n
                b = uniform.NormalMatrix * v.b
                t = uniform.NormalMatrix * v.t
                c = v.c
                tc = v.tc 
            }
        } 
