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
open Aardvark.VRVis
open Aardvark.VRVis.Opc

open MBrace.FsPickler
open System.IO

open PRo3D.Navigation2
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Viewer
open PRo3D.Groups
open Aardvark.UI.Trafos
open Aardvark.UI.Animation


module ViewerUtils =    
    open Aardvark.SceneGraph.Opc.Shader
    open FShade
    
    //open OpenCvSharp.XFeatures2D
    
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
                |_ -> return failwith "wrong type"
            }

    let lookUp guid (table:amap<Guid, IMod<MLeaf>>) =
        let entry = table |> AMap.find guid

        entry |> Mod.bind(fun x -> x |> toModSurface)

   
        
    let viewSingleSurfaceSg (surface : MSgSurface) (blarg : amap<Guid, IMod<MLeaf>>) (frustum : IMod<Frustum>) (selectedId : IMod<Option<Guid>>) (isctrl:IMod<bool>) (globalBB : IMod<Box3d>) =
        
        let surf = lookUp (surface.surface) blarg
            //Mod.bind(fun x -> lookUp (x.surface) blarg )

        let isSelected = Mod.map(fun x ->
          match x with
            | Some id -> id = surface.surface
            | None -> false) selectedId 

        let createSg (sg : ISg) =
            sg 
            |> Sg.noEvents 
            |> Sg.cullMode(surf |> Mod.bind(fun x -> x.cullMode))
            |> Sg.fillMode(surf |> Mod.bind(fun x -> x.fillMode))         
                
        let pickable = 
            adaptive {
                let! bb = globalBB
                return { shape = PickShape.Box bb; trafo = Trafo3d.Identity }
            }        

        let globalToLocal = 
            surface.globalBB |> Mod.map(fun x -> Trafo3d.Translation -x.Center)
            
        let selectedScalar =
            adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return scalar.IsSome
            }
        
        let scalar =
           adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return scalar
            } 

        // attribute false color mapping
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

        let upperC = scalar |> Mod.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.upperColor.c |> Mod.map(fun x -> let t = x.ToC3f()
                                                                                       let t1 = HSVf.FromC3f(t)
                                                                                       let t2 = (float)t1.H
                                                                                       t2 )
                            | None   -> Mod.constant(1.0)
                        )
        let lowerC = 
          scalar 
            |> Mod.bind ( fun x ->
              match x with 
                  | Some s -> s.colorLegend.lowerColor.c |> Mod.map(fun x -> ((float)(HSVf.FromC3f (x.ToC3f())).H))
                  | None   -> Mod.constant(0.0)
              )

        let rangeToMinMax = scalar |> Mod.bind ( fun x -> 
                                match x with 
                                    | Some s -> s.definedRange |> Mod.map ( fun y -> V2d(y.Min, y.Max))
                                    | None   -> Mod.constant(V2d(0.0,1.0))
                                )
     
        // image correction
        let contr =
           adaptive {
                let! s = surf
                let! c = s.colorCorrection.contrast.value
                return c
            } 

        let useContr =
           adaptive {
                let! s = surf
                let! uc = s.colorCorrection.useContrast
                return uc
            } 

        let bright =
           adaptive {
                let! s = surf
                let! b = s.colorCorrection.brightness.value
                return b
            } 

        let useB =
           adaptive {
                let! s = surf
                let! ub = s.colorCorrection.useBrightn
                return ub
            } 

        let gamma =
           adaptive {
                let! s = surf
                let! g = s.colorCorrection.gamma.value
                return g
            } 

        let useG =
           adaptive {
                let! s = surf
                let! ug = s.colorCorrection.useGamma
                return ug
            } 

        let useGray =
           adaptive {
                let! s = surf
                let! ug = s.colorCorrection.useGrayscale
                return ug
            } 

        let useColor =
           adaptive {
                let! s = surf
                let! uc = s.colorCorrection.useColor
                return uc
            } 
        let color =
           adaptive {
                let! s = surf
                let! uc = s.colorCorrection.color.c
                return uc
            }

        let trafo =
           adaptive {
                let! s = surf
                let! sc = s.scaling.value
                let! t = s.preTransform
                return (Trafo3d.Scale(sc) * t)
            }
        
        let test = 
            aset {
                let sg =  
                    surface.sceneGraph
                        |> Mod.map createSg
                        |> Sg.dynamic
                        |> Sg.uniform "selected"       isSelected
                        |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))   
                        |> Sg.uniform "falseColors"    selectedScalar
                        |> Sg.uniform "startC"         lowerC
                        |> Sg.uniform "endC"           upperC
                        |> Sg.uniform "interval"       interval
                        |> Sg.uniform "inverted"       inverted
                        |> Sg.uniform "lowerBound"     lowerB
                        |> Sg.uniform "upperBound"     upperB
                        |> Sg.uniform "MinMax"         rangeToMinMax
                        |> Sg.uniform "TriangleSize" (
                                adaptive {
                                    let! s = surf
                                    let! maxSize = s.triangleSize.value                                                                        
                                    return maxSize
                                })
                        |> Sg.uniform "useContrastS"  useContr
                        |> Sg.uniform "contrastS"     contr
                        |> Sg.uniform "useBrightnS"   useB
                        |> Sg.uniform "brightnessS"   bright
                        |> Sg.uniform "useGammaS"     useG 
                        |> Sg.uniform "gammaS"        gamma
                        |> Sg.uniform "useGrayS"      useGray
                        |> Sg.uniform "useColorS"     useColor
                        |> Sg.uniform "colorS"        color
                        |> Sg.texture (Sym.ofString "ColorMapTexture") (Mod.constant colormap)
                        |> Sg.onOff(surf |> Mod.bind(fun x -> x.isVisible))
                        |> Sg.LodParameters( 
                            adaptive {
                                let! s = surf
                                let! frustum = frustum 
                                let sizes = V2i(1024,768)
                                let! quality = s.quality.value
                                let! trafo = s.preTransform //combine pre and current transform

                                return { frustum = frustum; size = sizes; factor = Math.Pow(Math.E, quality); trafo = trafo }
                            })   
                        |> Sg.AttributeParameters(
                            adaptive {
                                let! s = surf
                                let! texture = s.selectedTexture 
                                let! scalar = s.selectedScalar

                                let attr : AttributeParameters = 
                                    {
                                        selectedTexture = texture |> Option.map(fun x -> x.index)
                                        selectedScalar  = scalar  |> Option.map(fun x -> x.index |> Mod.force)
                                    }

                                return attr
                            })
                        |> pickable' pickable
                        |> Sg.noEvents 
                        |> Sg.withEvents [
                             SceneEventKind.Down, (
                               fun sceneHit -> 
                                 let surfM = surf       |> Mod.force
                                 let name  = surfM.name |> Mod.force                                                  
                                 //Log.warn "spawing picksurface %s" name
                                 true, Seq.ofList [PickSurface (sceneHit,name)])                         
                           ]

                let! picking = surface.picking
                match picking with
                    | Picking.PickMesh pm ->
                        yield sg
                        yield
                            pm
                                |> Sg.requirePicking
                                |> Sg.noEvents
                                |> Sg.withEvents [
                                    Sg.onMouseDown ( fun b v -> PickObject (v, surface.surface) )
                                ]
                                |> Sg.shader {
                                    do! DefaultSurfaces.trafo
                                    do! DefaultSurfaces.constantColor C4f.White
                                }
                                |> Sg.trafo trafo //(surf |> Mod.bind(fun x -> x.preTransform)) 
                                |> Sg.depthTest (Mod.constant DepthTestMode.Never)

                    | Picking.KdTree kd -> yield sg

            } |> Sg.set
        test

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

    let renderCommands (sgGrouped:alist<amap<Guid,MSgSurface>>) overlayed discs (m:MModel) =
        let selected = m.scene.surfacesModel.surfaces.singleSelectLeaf
        
        let grouped = 
            sgGrouped |> AList.map(
                fun x -> ( x 
                    |> AMap.map(fun _ sf -> 
                        let bla = m.scene.surfacesModel.surfaces.flat
                        viewSingleSurfaceSg sf bla m.frustum selected m.ctrlFlag sf.globalBB )
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
                            DefaultSurfaces.diffuseTexture |> toEffect
                            fixAlpha |> toEffect
                            Shader.selectionColor          |> toEffect       
                            Shader.LoDColor                |> toEffect                             
                            PRo3D.Shader.falseColorLegend2 |> toEffect 
                            PRo3D.Shader.mapContrast       |> toEffect
                            PRo3D.Shader.mapBrightness     |> toEffect
                            PRo3D.Shader.mapGamma          |> toEffect                            
                            PRo3D.Shader.grayscale         |> toEffect
                            PRo3D.Shader.addColor          |> toEffect
                            triangleFilterX              |> toEffect                            
                        ]
                        |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
                        |> Sg.uniform "LodVisEnabled" m.scene.config.lodColoring //()
                        //|> Sg.pass RenderPass.main

                yield RenderCommand.SceneGraph sg                    

                if i = c then
                  yield RenderCommand.SceneGraph discs

                yield Aardvark.UI.RenderCommand.Clear(None,Some (Mod.constant 1.0))
            
            yield RenderCommand.SceneGraph overlayed
        }  
