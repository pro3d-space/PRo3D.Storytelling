//namespace PRo3D.Base

//open Aardvark.GeoSpatial.Opc.Shader    
//open Aardvark.Base
//open Aardvark.Base.Incremental
//open Aardvark.Base.Rendering
//open Aardvark.SceneGraph
//open Aardvark.Rendering
//open Aardvark.UI
//open Aardvark.UI.Trafos
//open Aardvark.UI.Primitives
//open PRo3D
//open FShade

//module Shaders =
          
//    type SuperVertex = 
//          {
//              [<Position>] pos :  V4d
//              [<SourceVertexIndex>] i : int
//          }

//    let lines (t : Triangle<SuperVertex>) =
//      line {
//          yield t.P0
//          yield t.P1
//          restartStrip()
          
//          yield t.P1
//          yield t.P2
//          restartStrip()

//          yield t.P2
//          yield t.P0
//          restartStrip()
//      }
//    let private colormap =
//          sampler2d {
//              texture uniform?ColorMapTexture
//              filter Filter.MinMagMipLinear
//              addressU WrapMode.Wrap
//              addressV WrapMode.Wrap
//    }

//    let falseColor (v : Vertex) =
//        fragment {           
//            if uniform?falseColors then
//                let range : V2d = uniform?MinMax
//                let norm = V2d((v.scalar - range.X)/ (range.Y - range.X), 0.5)

//                let c = colormap.Sample(norm).XYZ

//                return v.c * V4d(c.X, c.Y, c.Z, 1.0)
//            else
//                return v.c
//        }

//    [<ReflectedDefinition>]
//    let hsv2rgb (h : float) (s : float) (v : float) =
//      let h = Fun.Frac(h)
//      let chr = v * s
//      let x = chr * (1.0 - Fun.Abs(Fun.Frac(h * 3.0) * 2.0 - 1.0))
//      let m = v - chr
//      let t = (int)(h * 6.0)
//      match t with
//          | 0 -> V3d(chr + m, x + m, m)
//          | 1 -> V3d(x + m, chr + m, m)
//          | 2 -> V3d(m, chr + m, x + m)
//          | 3 -> V3d(m, x + m, chr + m)
//          | 4 -> V3d(x + m, m, chr + m)
//          | 5 -> V3d(chr + m, m, x + m)
//          | _ -> V3d(chr + m, x + m, m)

//    [<ReflectedDefinition>]
//    let mapFalseColors value : float =         
//        let invert           = uniform?inverted
//        let fcUpperBound     = uniform?upperBound
//        let fcLowerBound     = uniform?lowerBound
//        let fcInterval       = uniform?interval
//        let fcUpperHueBound  = uniform?endC
//        let fcLowerHueBound  = uniform?startC

//        let low         = if (invert = false) then fcLowerBound else fcUpperBound
//        let up          = if (invert = false) then fcUpperBound else fcLowerBound
//        let interval    = if (invert = false) then fcInterval   else -1.0 * fcInterval        

//        let rangeValue = up - low + interval
//        let normInterv = (interval / rangeValue)

//        //map range to 0..1 according to lower/upperbound
//        let k = (value - low + interval) / rangeValue

//        //discretize lookup
//        let bucket = floor (k / normInterv)
//        let k = ((float) bucket) * normInterv |> clamp 0.0 1.0

//        let uH = fcUpperHueBound * 255.0
//        let lH = fcLowerHueBound * 255.0
//        //map values to hue range
//        let fcHueUpperBound = if (uH < lH) then uH + 1.0 else uH
//        let rangeHue = uH - lH // fcHueUpperBound - lH
//        (k * rangeHue) + lH

//    [<ReflectedDefinition>]
//    let mapFalseColors2 value : float =
//          let fcInterval     = uniform?interval
//          let startColorHue     = uniform?startC
//          let endColorHue       = uniform?endC
//          let invertMapping  = uniform?inverted
//          let fcUpperBound   = uniform?upperBound
//          let fcLowerBound   = uniform?lowerBound

//          let range = fcUpperBound - fcLowerBound
//          let numOfRangeGaps = int(round (range / fcInterval)) //round
//          let numOfStops = if (numOfRangeGaps + 2) > 100 then 100 else (numOfRangeGaps + 2)
//          //let numOfStops = if (numOfRangeGaps + 2) > 200 then 200 else (numOfRangeGaps + 2)

//          let startColor = startColorHue *255.0
//          let endColor = endColorHue *255.0
//          let hStepSize =
//              if startColor < endColor then 
//                  (endColor - startColor) / ((float)(numOfStops-1))
//              else 
//                  ((endColor + 1.0) - startColor) / ((float)(numOfStops-1))
                  
//          let pos = 
//              match fcLowerBound < value with
//                      | true -> (int ( round(value - fcLowerBound) / fcInterval )) //+1//round
//                      | _ -> 0
//          let pos1 = 
//              match fcUpperBound < value with
//                  | true -> numOfRangeGaps
//                  | _ -> pos
                       
//          let currColorH = 
//              if invertMapping then
//                  (endColor - (hStepSize * ((float)(pos1))))
//              else 
//                  (startColor + (hStepSize * ((float)(pos1))))
//          currColorH

//    let private diffuseSampler =
//        sampler2d {
//            texture uniform?DiffuseColorTexture          
//            filter Filter.Anisotropic
//            maxAnisotropy 16
//            addressU WrapMode.Wrap
//            addressV WrapMode.Wrap
//        }

//    let improvedDiffuseTexture (v : Vertex) =
//      fragment {
//          let texColor = diffuseSampler.Sample(v.tc,-1.0)
//          return texColor
//      }
              
//    let falseColorLegend2 (v : Vertex) =
//        fragment {    

//            if (uniform?falseColors) 
//            then
//                let hue = mapFalseColors v.scalar //mapFalseColors2 v.scalar
//                let c = hsv2rgb ((clamp 0.0 255.0 hue)/ 255.0 ) 1.0 1.0 // 
//                return v.c * V4d(c.X, c.Y, c.Z, 1.0)
//            else
//                return v.c
//        }

//    //TODO LF: comment what does this shader do?
//    let falseColorLegendTest (v : Vertex) =
//        fragment {    

//            if (uniform?falseColors) 
//            then
//               let fcUpperBound     = uniform?upperBound
//               let fcLowerBound     = uniform?lowerBound
//               let k = (v.scalar - fcLowerBound) / (fcUpperBound-fcLowerBound) 
//               let value = clamp 0.0 1.0 k
//               return V4d(value, value, value, 1.0) //value // V4d(value, value, value, 1.0)
//            else
//                return v.c
//        }
    
//    module ColorAdaptation =

//        [<ReflectedDefinition>]
//        let myTrunc (value : float) =
//          clamp 0.0 255.0 value
        
//        let mapContrast (v : Vertex) =
//            fragment { 
//                if (uniform?useContrastS) then
//                    let c = uniform?contrastS
//                    let nc = V4d(v.c.X*255.0, v.c.Y*255.0,v.c.Z*255.0, 255.0)
                    
//                    let factor = (259.0 * (c + 255.0)) / (255.0 * (259.0 - c))
//                    let red    = (myTrunc (factor * (nc.X   - 128.0) + 128.0)) / 255.0
//                    let green  = (myTrunc (factor * (nc.Y   - 128.0) + 128.0)) / 255.0
//                    let blue   = (myTrunc (factor * (nc.Z   - 128.0) + 128.0)) / 255.0
//                    return V4d(red, green, blue, 1.0)
//                else
//                    return v.c
//            }

//        let mapBrightness (v : Vertex) = 
//            fragment { 
//                if (uniform?useBrightnS) then
//                    let b = uniform?brightnessS
//                    let nc = V4d(v.c.X*255.0, v.c.Y*255.0,v.c.Z*255.0, 255.0)        
                    
//                    let red   = (myTrunc(nc.X + b)) / 255.0 
//                    let green = (myTrunc(nc.Y + b)) / 255.0 
//                    let blue  = (myTrunc(nc.Z + b)) / 255.0 
//                    return V4d(red, green, blue, 1.0)
//                else
//                    return v.c
//            }

//        let mapGamma (v : Vertex) = 
//            fragment { 
//                if (uniform?useGammaS) then
//                    let g = uniform?gammaS
//                    let gammaCorrection = 1.0 / g
//                    return V4d(v.c.X**gammaCorrection, v.c.Y**gammaCorrection, v.c.Z**gammaCorrection, 1.0)
//                else
//                    return v.c
//            }

//        let grayscale (v : Vertex) = 
//            fragment { 
//                if (uniform?useGrayS) then
//                    let value = v.c.X * 0.299 + v.c.Y * 0.587 + v.c.Z * 0.114
//                    return V4d(value, value, value, 1.0)
//                else
//                    return v.c
//            }

//        let addColor (v : Vertex) = 
//            fragment { 
//                if (uniform?useColorS) then
//                    let col : V3d =  uniform?colorS
//                    let nc = V4d(v.c.X*255.0, v.c.Y*255.0, v.c.Z*255.0, 255.0)
            
//                    let red   = (myTrunc( nc.X * col.X)) / 255.0
//                    let green = (myTrunc( nc.Y * col.Y)) / 255.0
//                    let blue  = (myTrunc( nc.Z * col.Z)) / 255.0
//                    return V4d(red, green, blue, 1.0)
//                else
//                    return v.c
//            }
    
//    module FootPrint = 

//        type Vertex =
//              {
//                  [<Position>]                pos     : V4d            
//                  [<WorldPosition>]           wp      : V4d
//                  [<TexCoord>]                tc      : V2d
//                  [<Color>]                   c       : V4d
//                  [<Normal>]                  n       : V3d
//                  [<SourceVertexIndex>]       sv      : int
//                  [<Semantic("Scalar")>]      scalar  : float
//                  [<Semantic("LightDir")>]    ldir    : V3d
//                  [<Semantic("Tex0")>]        tc0     : V4d

//              }
        
//        let private footprintmap =
//              sampler2d {
//                  texture uniform?FootPrintTexture
//                  filter Filter.MinMagMipLinear
//                  borderColor C4f.Black
//                  addressU WrapMode.Border
//                  addressV WrapMode.Border
//                  addressW WrapMode.Border
//        }

//        let footprintV (v : Vertex) =
//              vertex {
//                  let vp = uniform.ModelViewTrafo * v.pos
//                  let p = uniform.ProjTrafo * v.pos
                  
//                  let instrumentMatrix    : M44d   = uniform?instrumentMVP
//                  let projectionMatrix    : M44d   = uniform?projMVP
//                  let instrumentView      : M44d   = uniform?instViewMVP
//                  let globalToLocal       : V3d   = uniform?globalToLocal
                 
//                  //let test = forwardMatrix * v.wp |> Vec.normalize
//                  let test = v.wp - V4d(globalToLocal, 0.0) |> Vec.normalize
//                  let v1 = instrumentView * test |> Vec.normalize
//                  let v2 = projectionMatrix * v1 |> Vec.normalize
//                  //let tt = instrumentMatrix * test
//                  return { v with tc0 = v2 ; sv = 0 } //v.wp
//              }

//        let footprintV2 (v : Vertex) =
//            vertex {
//                let instrumentMatrix : M44d = uniform?instrumentMVP
//                let vp = uniform.ModelViewTrafo * v.pos
//                let wp = uniform.ModelTrafo * v.pos

//                return { 
//                    v with
//                        pos  = uniform.ProjTrafo * vp
//                        wp   = wp
//                        n    = transformNormal v.n
//                        ldir = V3d.Zero - vp.XYZ |> Vec.normalize
//                        tc0  = instrumentMatrix * v.pos
//                } 
//            } 

//        let footPrintF (v : Vertex) =
//            fragment {           
//                if uniform?footprintVisible then
//                    let outside = v.c * 0.7 
//                    let t = v.tc0.XY / v.tc0.W
//                    let c = 
//                        if t.X > -1.0 && t.X < 1.0 && t.Y > -1.0 && t.Y < 1.0 then
//                           v.c 
//                        elif t.X > -1.01 && t.X < 1.01 && t.Y > -1.01 && t.Y < 1.01 then
//                           v.c * V4d(1.0, 0.0, 0.0, 1.0)
//                        else
//                           outside 

//                    if (v.tc0.Z <= 0.0) then
//                        return outside
//                    else
//                        return c 
                     
//                else
//                  return v.c
//            }

//        let footPrintF2 (v : Vertex) =
//            fragment {           
//                if uniform?footprintVisible then
//                    //let col = v.c * V4d(1.0, 0.0, 0.0, 1.0)
//                    let proTex0 = v.tc0.XY / v.tc0.W
//                    let c = footprintmap.Sample(proTex0)
//                    let col = 
//                        if (c.W <= 0.50) then
//                            (v.c * 0.7)
//                        elif (c.W <= 0.999) then
//                            v.c * V4d(1.0, 0.0, 0.0, 1.0)
//                        else
//                            v.c
                 
//                    if (v.tc0.Z <= 0.0) then
//                        return (v.c * 0.7)
//                    else
//                        return col 
//                else
//                    return v.c
//            }

