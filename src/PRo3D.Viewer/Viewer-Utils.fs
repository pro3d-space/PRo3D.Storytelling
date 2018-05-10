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

open Aardvark.Opc.Internal
open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives.Sg
open Aardvark.VRVis

open MBrace.FsPickler
open System.IO

open PRo3D.Navigation2
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Viewer
open PRo3D.Groups
open Aardvark.UI.Trafos
open Aardvark.UI.Animation

type PropertyActions =
    | DrawingMessage    of Drawing.Action
    | AnnotationMessage of AnnotationProperties.Action

type ViewerAction =                
    | DrawingMessage            of Drawing.Action
    | AnnotationGroupsMessageViewer   of Groups.Action
    | NavigationMessage         of Navigation.Action
    | AnimationMessage          of AnimationAction
    | ReferenceSystemMessage    of ReferenceSystemApp.Action
    | AnnotationMessage         of AnnotationProperties.Action
    | BookmarkMessage           of Bookmarks.Action
    | BookmarkUIMessage         of Groups.Action
    | RoverMessage              of RoverApp.Action
    | ViewPlanMessage           of ViewPlan.Action
    | SetCamera                 of CameraView        
    | SetCameraAndFrustum       of CameraView * double * double        
    | SetCameraAndFrustum2      of CameraView * Frustum
    | ImportSurface   
    | ImportObject   
    | ImportAnnotationGroups
    | ImportSurfaceTrafo
    | SwitchViewerMode
    | DnSProperties             of PropertyActions
    | ConfigPropertiesMessage   of ConfigProperties.Action
    | QuickLoad1
    | QuickLoad2
    | DeleteLast
    | AddSg                     of ISg
    | PickSurface               of SceneHit*string
    | PickObject                of V3d*Guid
    | SaveScene
    | SaveAs                    
    | OpenScene
    | LoadScene                 of string
    | NewScene
    | KeyDown                   of key : Aardvark.Application.Keys
    | KeyUp                     of key : Aardvark.Application.Keys      
    | SetKind                   of TrafoKind
    | SetInteraction            of Interactions        
    | SetMode                   of TrafoMode
    | Translate                 of string * TrafoController.Action
    | Rotate                    of string * TrafoController.Action
    | SurfaceActions            of SurfaceApp.Action
    | AlignmentActions          of PRo3d.Align.AlignmentActions
    | SetTabMenu                of TabMenu
    | OpenSceneFileLocation     of string
    | NoAction                  of string
    //| UpdateDockConfig          of DockConfig


module ViewerUtils =
    //open OpenCvSharp.XFeatures2D
    
    let colormap = 
        let config = { wantMipMaps = false; wantSrgb = false; wantCompressed = false }
        FileTexture("HueColorMap.png",config) :> ITexture

    let pickable' (pick :IMod<Pickable>) (sg: ISg) =
        Sg.PickableApplicator (pick, Mod.constant sg)

    //let getSurface (child:IMod<Option<IMod<MLeaf>>>) =
    //    adaptive {
    //        let! c = child
    //        match c with 
    //            | Some s -> 
    //                let! s' = s 
    //                match s' with
    //                    | MSurfaces y -> return Some y
    //                    | _-> return None 
    //            |_ -> return None
    //        }

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

    type MSgSurfaces with
        member x.surface =
            match x with
                | MSgSurfaceOPC opc -> opc.surface
                | MSgSurfaceOBJ obj -> obj.surface

        member x.globalBB =
            match x with
                | MSgSurfaceOPC opc -> opc.globalBB
                | MSgSurfaceOBJ obj -> obj.globalBB

        member x.trafo =
            match x with
                | MSgSurfaceOPC opc -> opc.trafo
                | MSgSurfaceOBJ obj -> obj.trafo

        member x.sceneGraph =
            match x with
                | MSgSurfaceOPC opc -> opc.sceneGraph
                | MSgSurfaceOBJ obj -> obj.sceneGraph

        //member x.picking =
        //    match x with
        //        | MSgSurfaceOPC opc -> opc.sceneGraph
        //        | MSgSurfaceOBJ obj -> obj.pickMesh
                                      
       
           

    let viewSingleSurfaceSgOPC (surface : MSgSurfaces) (blarg : amap<Guid, IMod<MLeaf>>) (frustum : IMod<Frustum>) (selectedId : IMod<Option<Guid>>) (globalBB : IMod<Box3d>) =
        
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

        let rangeToMinMax (s:MSgSurfaces) =
            adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return match scalar with 
                          | Some s -> V2d(s.definedRange.Min, s.definedRange.Max)
                          | None   -> V2d(0.0,1.0)
            }

        let globalToLocal = 
            surface.globalBB |> Mod.map(fun x -> Trafo3d.Translation -x.Center)
            
        let selectedScalar =
            adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return scalar.IsSome
            }
       
        let sg = 
            surface.sceneGraph
                |> Mod.map createSg
                |> Sg.dynamic
                |> Sg.uniform "selected"       isSelected
                |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))   
                |> Sg.uniform "falseColors"    selectedScalar
                |> Sg.uniform "MinMax"         (surface |> rangeToMinMax)
                |> Sg.uniform "TriangleSize" (
                        adaptive {
                            let! s = surf
                            let! maxAngle = s.triangleSize.value
                            Log.warn "maxAngle: %A" maxAngle
                            Log.warn "maxAngleCos: %A" (maxAngle |> Double.radiansFromDegrees |> Math.Cos)
                            return maxAngle |> Double.radiansFromDegrees |> Math.Cos
                        })
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

                        let attr : Aardvark.Opc.Internal.AttributeParameters = 
                            {
                                selectedTexture = texture |> Option.map(fun x -> x.index)
                                selectedScalar  = scalar  |> Option.map(fun x -> x.index)
                            }

                        return attr
                    })
                |> pickable' pickable
                |> Sg.noEvents                
                |> Sg.withEvents [
                        SceneEventKind.Down, (
                            fun sceneHit -> 
                                let bla = surf |> Mod.force
                                let bla = bla.name |> Mod.force
                                true, Seq.ofList [PickSurface (sceneHit,bla)])
                    ]
                //|> Sg.trafo (surface.globalBB 
                //                |> Mod.map(fun x -> 
                //                    let t = Trafo3d.Translation(-x.Center)
                //                    printfn "%A" t.Forward.C3.XYZ
                //                    t))
                //|> Sg.trafo (surf |> Mod.bind(fun x -> x.preTransform))                
                //|> Sg.trafo (surface.trafo.previewTrafo |> Mod.map(fun x -> printfn "%A" x.Forward.C3.XYZ; x))

               
        sg
    
    let renderOBJ (surfaces : amap<Guid,IMod<MSgSurfaces>>)  =
        Ag.initialize()
        let sg = 
            let scaleTrafo = Mod.constant (Trafo3d.Scale(10.0))
            let obj = Aardvark.SceneGraph.IO.Loader.Assimp.load @"C:\Users\ortner\Desktop\aardvark.obj"
            
            obj
                |> Sg.adapter
                
                // flip the z coordinates (since the model is upside down)
                |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
                |> Sg.scale 3.0
                |> Sg.noEvents
                //|> Sg.trafo scaleTrafo
                |> Sg.withEvents [
                    Sg.onMouseDown ( fun b v -> SurfaceApp.Action.PickObject v )
                ]
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! DefaultSurfaces.normalMap
                    do! DefaultSurfaces.simpleLighting
                }
                        
        sg

    let viewSingleSurfaceSgOBJ (surface : MSgSurfaces) (blarg : amap<Guid, IMod<MLeaf>>) (frustum : IMod<Frustum>) (selectedId : IMod<Option<Guid>>) (globalBB : IMod<Box3d>) =
        
        let surf = lookUp (surface.surface) blarg

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

        let rangeToMinMax (s:MSgSurfaces) =
            adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return match scalar with 
                          | Some s -> V2d(s.definedRange.Min, s.definedRange.Max)
                          | None   -> V2d(0.0,1.0)
            }

        let globalToLocal = 
            surface.globalBB |> Mod.map(fun x -> Trafo3d.Translation -x.Center)
            
        let selectedScalar =
            adaptive {
                let! s = surf
                let! scalar = s.selectedScalar
                return scalar.IsSome
            }
       
        let sg = 
            let scaleTrafo = Mod.constant (Trafo3d.Scale(10.0))
            //let obj = Aardvark.SceneGraph.IO.Loader.Assimp.load @"C:\Users\laura\VRVis\Data\OBJ\aardvark.obj" 
            
            aset {
                let! obj = surface.sceneGraph
                yield obj 
                    |> Sg.adapter
                    |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
                    |> Sg.requirePicking
                    |> pickable' pickable
                    |> Sg.noEvents
                    //|> Sg.trafo globalToLocal
                    |> Sg.withEvents [
                        Sg.onMouseDown ( fun b v -> PickObject (v, surface.surface) )
                    ]
                    |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.diffuseTexture
                        do! DefaultSurfaces.normalMap
                        do! DefaultSurfaces.simpleLighting
                    }
                 }       |> Sg.set

        //let sgPicking = 
        //    aset {
        //        let! mesh = surface.picking
        //        yield mesh
        //        |> Sg.requirePicking
        //        |> pickable' pickable
        //        |> Sg.noEvents
        //        |> Sg.withEvents [
        //            // Sg.onEnter ( fun v -> Enter v )
        //            Sg.onMouseDown ( fun b v -> PickObject (v, surface.surface) )
        //        ]
        //        |> Sg.shader {
        //            do! DefaultSurfaces.trafo
        //            do! DefaultSurfaces.constantColor C4f.White
        //        }
        //        |> Sg.depthTest (Mod.constant DepthTestMode.Never)
                         
        //        } |> Sg.set
        //[sg ;sgPicking ] |> Sg.ofList
        sg

        

    let frustum (m:MModel) =
        let near = m.scene.config.nearPlane.value
        let far = m.scene.config.farPlane.value
        (Navigation.UI.frustum near far)

    let renderCommands (sgGrouped:alist<amap<Guid,IMod<MSgSurfaces>>>) overlayed discs (m:MModel) =
        let selected = m.scene.surfacesModel.surfaces.singleSelectLeaf
        
        let grouped = 
            sgGrouped |> AList.map(
                fun x -> ( x 
                    |> AMap.map(fun _ sf -> 
                        let bla = m.scene.surfacesModel.surfaces.flat
                        let sf = Mod.force sf
                        let test = match sf with
                                    | MSgSurfaceOPC surf ->
                                         (viewSingleSurfaceSgOPC sf bla m.frustum selected sf.globalBB)
                                    | MSgSurfaceOBJ surf -> 
                                        (viewSingleSurfaceSgOBJ sf bla m.frustum selected sf.globalBB)
                        test
                                )
                    //|> AMap.choose(fun _ y -> y ) 
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
                            Shader.selectionColor          |> toEffect       
                            Shader.LoDColor                |> toEffect
                            Shader.falseColor              |> toEffect                            
                            Shader.triangleFilter          |> toEffect
                        ]
                        |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
                        |> Sg.uniform "LodVisEnabled" m.scene.config.lodColoring //()

                yield RenderCommand.SceneGraph sg                    

                if i = c then
                  yield RenderCommand.SceneGraph discs

                yield Aardvark.UI.RenderCommand.Clear(None,Some (Mod.constant 1.0))
            
            yield RenderCommand.SceneGraph overlayed
        }  
