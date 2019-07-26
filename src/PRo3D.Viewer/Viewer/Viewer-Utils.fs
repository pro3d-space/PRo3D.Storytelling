namespace PRo3D

open System
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text

open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives.Sg
open Aardvark.GeoSpatial.Opc

open MBrace.FsPickler
open System.IO

open PRo3D.Navigation2
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Viewer
open PRo3D.Groups
open PRo3d.Mutable
open Aardvark.UI.Trafos
open Aardvark.UI.Animation


module ViewerUtils =    
    open Aardvark.GeoSpatial.Opc.Shader
    open FShade
                          
    let colormap = 
        let config = { wantMipMaps = false; wantSrgb = false; wantCompressed = false }
        FileTexture("HueColorMap.png",config) :> ITexture

    let pickable' (pick :IMod<Pickable>) (sg: ISg) =
        Sg.PickableApplicator (pick, Mod.constant sg)
    
    let toModSurface (leaf : IMod<MLeaf>) = 
         adaptive {
            let! c = leaf
            match c with 
                | MSurfaces s -> return s
                | _ -> return c |> sprintf "wrong type %A; expected MSurfaces" |> failwith
            }

    let lookUp guid (table:amap<Guid, IMod<MLeaf>>) =
        
        let entry = table |> AMap.find guid

        entry |> Mod.bind(fun x -> x |> toModSurface)
    
    let addImageCorrectionParameters (surf:IMod<MSurface>)  (isg:ISg<'a>) =
        
            //Mod.bind(fun x -> lookUp (x.surface) blarg )
        let contr    = surf |> Mod.bind( fun x -> x.colorCorrection.contrast.value )
        let useContr = surf |> Mod.bind( fun x -> x.colorCorrection.useContrast )
        let bright   = surf |> Mod.bind( fun x -> x.colorCorrection.brightness.value )
        let useB     = surf |> Mod.bind( fun x -> x.colorCorrection.useBrightn) 
        let gamma    = surf |> Mod.bind( fun x -> x.colorCorrection.gamma.value )
        let useG     = surf |> Mod.bind( fun x -> x.colorCorrection.useGamma )
        let useGray  = surf |> Mod.bind( fun x -> x.colorCorrection.useGrayscale )
        let useColor = surf |> Mod.bind( fun x -> x.colorCorrection.useColor )
        let color    = surf |> Mod.bind( fun x -> x.colorCorrection.color.c )

        isg
            |> Sg.uniform "useContrastS"   useContr  //image correction
            |> Sg.uniform "contrastS"      contr
            |> Sg.uniform "useBrightnS"    useB
            |> Sg.uniform "brightnessS"    bright
            |> Sg.uniform "useGammaS"      useG 
            |> Sg.uniform "gammaS"         gamma
            |> Sg.uniform "useGrayS"       useGray
            |> Sg.uniform "useColorS"      useColor
            |> Sg.uniform "colorS"         color


   // let viewSingleSurfaceSg (surface : MSgSurface) (surfaceTable : amap<Guid, IMod<MLeaf>>) (frustum : IMod<Frustum>) (selectedId : IMod<Option<Guid>>) (isctrl:IMod<bool>) (globalBB : IMod<Box3d>) (refsys:MReferenceSystem) =
    

    let addAttributeFalsecolorMappingParameters (surf:IMod<MSurface>)  (isg:ISg<'a>) =
            
        let selectedScalar =
            adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return scalar.IsSome
            }  
      
        let scalar = surf |> Mod.bind( fun x -> x.selectedScalar )

        //let texturLayer =  
        //    adaptive {
        //        let! scalar = scalar
        //        let! s = surf
        //        match scalar with
        //         | Some sc -> 
        //            return s.textureLayers |> AList.toList
        //                                   |> List.find( fun (tl:TextureLayer) -> tl.label = (Mod.force sc.label ))
        //         | None -> return list.Empty
               
        //    }  
      

        let interval = scalar |> Mod.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.interval.value
                            | None   -> Mod.constant(1.0)
                        )

        let inverted = scalar |> Mod.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.invertMapping
                            | None   -> Mod.constant(false)
                        )     
        
        let upperB = scalar |> Mod.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.upperBound.value
                            | None   -> Mod.constant(1.0)
                        )

        let lowerB = scalar |> Mod.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.lowerBound.value
                            | None   -> Mod.constant(1.0)
                        )

        let upperC = 
          scalar 
            |> Mod.bind (fun x ->
               match x with 
                 | Some s -> 
                   s.colorLegend.upperColor.c 
                     |> Mod.map(fun x -> 
                       let t = x.ToC3f()
                       let t1 = HSVf.FromC3f(t)
                       let t2 = (float)t1.H
                       t2)
                 | None -> Mod.constant(1.0)
               )
        let lowerC = 
          scalar 
            |> Mod.bind ( fun x ->
              match x with 
                | Some s -> 
                  s.colorLegend.lowerColor.c 
                    |> Mod.map(fun x -> ((float)(HSVf.FromC3f (x.ToC3f())).H))
                | None   -> Mod.constant(0.0)
              )

        let rangeToMinMax = 
          scalar 
            |> Mod.bind (fun x ->
              match x with 
                  | Some s -> s.definedRange |> Mod.map(fun y -> V2d(y.Min, y.Max))
                  | None   -> Mod.constant(V2d(0.0,1.0))
              )
        isg
            |> Sg.uniform "falseColors"    selectedScalar
            |> Sg.uniform "startC"         lowerC  
            |> Sg.uniform "endC"           upperC
            |> Sg.uniform "interval"       interval
            |> Sg.uniform "inverted"       inverted
            |> Sg.uniform "lowerBound"     lowerB
            |> Sg.uniform "upperBound"     upperB
            |> Sg.uniform "MinMax"         rangeToMinMax
            |> Sg.texture (Sym.ofString "ColorMapTexture") (Mod.constant colormap)

    let getLodParameters (surf:IMod<MSurface>) (frustum : IMod<Frustum>) =
        adaptive {
            let! s = surf
            let! frustum = frustum 
            let sizes = V2i(1024,768)
            let! quality = s.quality.value
            let! trafo = s.preTransform //combine pre and current transform
            
            return { frustum = frustum; size = sizes; factor = Math.Pow(Math.E, quality); trafo = trafo }
        }
    
    let attributeParameters (surf:IMod<MSurface>) =
         adaptive {
            let! s = surf
            let! scalar = s.selectedScalar
            let! scalar' = 
                match scalar with
                | Some m -> m.label |> Mod.map Some
                | None -> Mod.constant None //scalar |> Option.map(fun x -> x.index) //option<IMod<int>>
            
            let! texture = s.selectedTexture 
            let attr : AttributeParameters = 
                {
                    selectedTexture = texture |> Option.map(fun x -> x.index)
                    selectedScalar  = scalar'//scalar  |> Option.map(fun x -> x.index |> Mod.force)
                }

            return attr
        }

    let viewSingleSurfaceSg (surface : MSgSurface) (blarg : amap<Guid, IMod<MLeaf>>) (frustum : IMod<Frustum>) (selectedId : IMod<Option<Guid>>)
                            (isctrl:IMod<bool>) (globalBB : IMod<Box3d>) (refsys:MReferenceSystem) (fp:MFootPrint) (vp:IMod<Option<MViewPlan>>) 
                            (useHighlighting:IMod<bool>) =
        adaptive {
          let! exists = (blarg |> AMap.keys) |> ASet.contains surface.surface
          if exists then
            
            let surf = lookUp (surface.surface) blarg
                //Mod.bind(fun x -> lookUp (x.surface) blarg )
            
            let isSelected = Mod.map2(fun x y ->
              match x with
                | Some id -> (id = surface.surface) && y
                | None -> false) selectedId useHighlighting
            
            let createSg (sg : ISg) =
                sg 
                |> Sg.noEvents 
                |> Sg.cullMode(surf |> Mod.bind(fun x -> x.cullMode))
                |> Sg.fillMode(surf |> Mod.bind(fun x -> x.fillMode))
            
            let pickable = 
              Mod.map2( fun (a:Box3d) (b:Trafo3d) -> 
                { shape = PickShape.Box (a.Transformed(b)); trafo = Trafo3d.Identity }) globalBB (Transformations.fullTrafo surf refsys)
            
            let pickBox = 
              pickable 
                |> Mod.map(fun k ->
                  match k.shape with
                    | PickShape.Box bb -> bb
                    | _ -> Box3d.Invalid)
            
            let triangleFilter = surf |> Mod.bind(fun s -> s.triangleSize.value)
            
            let trafo =
               adaptive {
                    let! fullTrafo = Transformations.fullTrafo surf refsys
                    let! s = surf
                    let! sc = s.scaling.value
                    let! t = s.preTransform
                    return (t * fullTrafo ) // * Trafo3d.Scale(sc))
                }
            
            let bla : SamplerStateDescription -> SamplerStateDescription = 
              (fun x -> 
                x.Filter <- new TextureFilter(TextureFilterMode.Linear, TextureFilterMode.Point, TextureFilterMode.Linear, true ); 
                x)
            
            
            let footprintVisible = //Mod.map2 (fun (vp:Option<MViewPlan>) vis -> (vp.IsSome && vis)) vp, fp.isVisible
                adaptive {
                    let! vp = vp
                    let! visible = fp.isVisible
                    let! id = fp.vpId
                    return (vp.IsSome && visible)
                }
            
            let footprintMatrix = 
                adaptive {
                    let! fppm = fp.projectionMatrix
                    let! fpvm = fp.instViewMatrix
                    let! s = surf
                    let! ts = s.preTransform
                    let! t = trafo
                    //return (t.Forward * fpvm * fppm) //* t.Forward 
                    return (fppm * fpvm) // * ts.Forward
                } 
            
            let test =             
              surface.sceneGraph
                |> Mod.map createSg
                |> Sg.dynamic
                |> Sg.trafo trafo //(Transformations.fullTrafo surf refsys)
                |> Sg.modifySamplerState (DefaultSemantic.DiffuseColorTexture)(Mod.constant(bla))
                |> Sg.uniform "selected"      (isSelected) // isSelected
                |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))
                |> addAttributeFalsecolorMappingParameters surf
                |> Sg.uniform "TriangleSize"   triangleFilter  //triangle filter
                |> addImageCorrectionParameters surf
                |> Sg.uniform "footprintVisible" footprintVisible
                |> Sg.uniform "instrumentMVP" footprintMatrix
                |> Sg.uniform "projMVP" fp.projectionMatrix
                |> Sg.uniform "globalToLocal" fp.globalToLocalPos
                |> Sg.uniform "instViewMVP" fp.instViewMatrix
                |> Sg.texture (Sym.ofString "FootPrintTexture") fp.projTex
                |> Sg.onOff(surf |> Mod.bind(fun x -> x.isVisible))
                |> Sg.LodParameters( getLodParameters surf frustum )
                |> Sg.AttributeParameters( attributeParameters surf )
                |> pickable' pickable
                |> Sg.noEvents 
                |> Sg.withEvents [
                     SceneEventKind.Down, (
                       fun sceneHit -> 
                         let surfM = surf       |> Mod.force
                         let name  = surfM.name |> Mod.force                                                  
                         Log.warn "spawning picksurface %s" name
                         true, Seq.ofList [PickSurface (sceneHit,name)])                         
                   ]                                               
              |> Sg.andAlso (
                (Sg.wireBox (C4b.VRVisGreen |> Mod.constant) pickBox) 
                  |> Sg.noEvents
                  |> Sg.effect [              
                      Shader.stableTrafo |> toEffect 
                      DefaultSurfaces.vertexColor |> toEffect
                    ] |> Sg.onOff (Mod.constant false)
                )
            
            return test
          else
            return Sg.empty
        } |> Sg.dynamic

        //let isSelected = Mod.map(fun x ->
        //  match x with
        //    | Some id -> id = surface.surface
        //    | None -> false) selectedId 

        //let createSg (sg : ISg) =
        //    sg 
        //    |> Sg.noEvents 
        //    |> Sg.cullMode(surf |> Mod.bind(fun x -> x.cullMode))
        //    |> Sg.fillMode(surf |> Mod.bind(fun x -> x.fillMode))

        //let pickable = 
        //  Mod.map2( fun (a:Box3d) (b:Trafo3d) -> 
        //    { shape = PickShape.Box (a.Transformed(b)); trafo = Trafo3d.Identity }) globalBB (Transformations.fullTrafo surf refsys)

        //let pickBox = 
        //  pickable 
        //    |> Mod.map(fun k ->
        //      match k.shape with
        //        | PickShape.Box bb -> bb
        //        | _ -> Box3d.Invalid)

        //let triangleFilter = surf |> Mod.bind(fun s -> s.triangleSize.value)

        //let trafo =
        //   adaptive {
        //        let! fullTrafo = Transformations.fullTrafo surf refsys
        //        let! s = surf
        //        let! sc = s.scaling.value
        //        let! t = s.preTransform
        //        return (t * fullTrafo ) // * Trafo3d.Scale(sc))
        //    }

        //let bla : SamplerStateDescription -> SamplerStateDescription = 
        //  (fun x -> 
        //    x.Filter <- new TextureFilter(TextureFilterMode.Linear, TextureFilterMode.Point, TextureFilterMode.Linear, true ); 
        //    x)
        
        
        //let footprintVisible = //Mod.map2 (fun (vp:Option<MViewPlan>) vis -> (vp.IsSome && vis)) vp, fp.isVisible
        //    adaptive {
        //        let! vp = vp
        //        let! visible = fp.isVisible
        //        let! id = fp.vpId
        //        return (vp.IsSome && visible)
        //    }

        //let footprintMatrix = 
        //    adaptive {
        //        let! t = trafo
        //        let! fppm = fp.projectionMatrix
        //        let! fpvm = fp.instViewMatrix
        //        let! s = surf
        //        let! ts = s.preTransform 
        //        let testfppm = fppm * t.Backward 
        //        let testfpvm = fpvm * t.Backward
        //        //return (t.Forward * fpvm * fppm) //* t.Forward 
        //        //return (fppm * fpvm) // * ts.Forward
        //        return (testfppm * testfpvm)
        //    } 
        
        //let projMTest = 
        //    adaptive {
        //        let! fppm = fp.projectionMatrix
        //        let! t = trafo
        //        return fppm/t.Forward
        //    }
        //let instVMMTest = 
        //    adaptive {
        //        let! fppm = fp.projectionMatrix
        //        let! t = trafo
        //        return fppm/t.Forward
        //    }
        //let test =             
        //  surface.sceneGraph
        //    |> Mod.map createSg
        //    |> Sg.dynamic
        //    |> Sg.trafo trafo //(Transformations.fullTrafo surf refsys)
        //    |> Sg.modifySamplerState (DefaultSemantic.DiffuseColorTexture)(Mod.constant(bla))
        //    |> Sg.uniform "selected"      (isSelected) // isSelected
        //    |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))
        //    |> addAttributeFalsecolorMappingParameters surf
        //    |> Sg.uniform "TriangleSize"   triangleFilter  //triangle filter
        //    |> addImageCorrectionParameters surf
        //    |> Sg.uniform "footprintVisible" footprintVisible
        //    |> Sg.uniform "instrumentMVP" footprintMatrix
        //    |> Sg.uniform "projMVP" fp.projectionMatrix
        //    |> Sg.uniform "globalToLocal" fp.globalToLocalPos
        //    |> Sg.uniform "instViewMVP" fp.instViewMatrix
        //    |> Sg.texture (Sym.ofString "FootPrintTexture") fp.projTex
        //    |> Sg.onOff(surf |> Mod.bind(fun x -> x.isVisible))
        //    |> Sg.LodParameters( getLodParameters surf frustum )
        //    |> Sg.AttributeParameters( attributeParameters surf )
        //    |> pickable' pickable
        //    |> Sg.noEvents 
        //    |> Sg.withEvents [
        //         SceneEventKind.Down, (
        //           fun sceneHit -> 
        //             let surfM = surf       |> Mod.force
        //             let name  = surfM.name |> Mod.force                                                  
        //             Log.warn "spawning picksurface %s" name
        //             true, Seq.ofList [PickSurface (sceneHit,name)])                         
        //       ]                                               
        //  |> Sg.andAlso (
        //    (Sg.wireBox (C4b.VRVisGreen |> Mod.constant) pickBox) 
        //      |> Sg.noEvents
        //      |> Sg.effect [              
        //          Shader.stableTrafo |> toEffect 
        //          DefaultSurfaces.vertexColor |> toEffect
        //        ] |> Sg.onOff (Mod.constant false)
        //    )

        //test

    let frustum (m:MModel) =
        let near = m.scene.config.nearPlane.value
        let far = m.scene.config.farPlane.value
        (Navigation.UI.frustum near far)

    let fixAlpha (v : Vertex) =
        fragment {         
           return V4d(v.c.X, v.c.Y,v.c.Z, 1.0)           
        }

    let triangleFilterX (input : Triangle<Vertex>) =
        triangle {
            let p0 = input.P0.wp.XYZ
            let p1 = input.P1.wp.XYZ
            let p2 = input.P2.wp.XYZ

            let maxSize = uniform?TriangleSize

            let a = (p1 - p0)
            let b = (p2 - p1)
            let c = (p0 - p2)

            let alpha = a.Length < maxSize
            let beta  = b.Length < maxSize
            let gamma = c.Length < maxSize

            let check = (alpha && beta && gamma)
            if check then
                yield input.P0 
                yield input.P1
                yield input.P2
        }

    let renderCommands (sgGrouped:alist<amap<Guid,MSgSurface>>) overlayed depthTested (m:MModel) =
        let usehighlighting = m.scene.config.useSurfaceHighlighting
        let selected = m.scene.surfacesModel.surfaces.singleSelectLeaf
        let refSystem = m.scene.referenceSystem
        let grouped = 
            sgGrouped |> AList.map(
                fun x -> ( x 
                    |> AMap.map(fun _ sf -> 
                        let bla = m.scene.surfacesModel.surfaces.flat
                        viewSingleSurfaceSg sf bla m.frustum selected m.ctrlFlag sf.globalBB refSystem m.footPrint m.scene.viewPlans.selectedViewPlan usehighlighting )
                    |> AMap.toASet 
                    |> ASet.map snd                     
                )                
            )
        //grouped   
        alist {        
            let! c = grouped |> AList.count
            let mutable i = 0
            for set in grouped do
                i <- i + 1
                let sg = 
                    set 
                        |> Sg.set
                        |> Sg.effect [
                            Shader.stableTrafo             |> toEffect                          
                            triangleFilterX                |> toEffect                               
                            PRo3D.Shader.improvedDiffuseTexture |> toEffect
                            fixAlpha |> toEffect
                            Shader.selectionColor          |> toEffect       
                            Shader.LoDColor                |> toEffect                             
                            PRo3D.Shader.falseColorLegend2 |> toEffect
                            //PRo3D.Shader.falseColorLegendTest |> toEffect
                            //PRo3D.Shader.mapContrast       |> toEffect
                            //PRo3D.Shader.mapBrightness     |> toEffect
                            //PRo3D.Shader.mapGamma          |> toEffect                            
                            //PRo3D.Shader.grayscale         |> toEffect
                            //PRo3D.Shader.addColor          |> toEffect
                            PRo3D.Shader.footprintV        |> toEffect
                            PRo3D.Shader.footPrintF        |> toEffect
                        ]
                        |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
                        |> Sg.uniform "LodVisEnabled" m.scene.config.lodColoring //()                        

                yield RenderCommand.SceneGraph sg

                //if i = c then //now gets rendered multiple times
                 // assign priorities globally, or for each anno and make sets
                yield RenderCommand.SceneGraph depthTested

                yield Aardvark.UI.RenderCommand.Clear(None,Some (Mod.constant 1.0))
            
            yield RenderCommand.SceneGraph overlayed
            
        }  
