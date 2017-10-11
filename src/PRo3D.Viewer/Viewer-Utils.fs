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

open Aardvark.Opc    
open Aardvark.SceneGraph.SgPrimitives.Sg
open Aardvark.VRVis

open MBrace.FsPickler
open System.IO

open PRo3D.Navigation2
open PRo3D.ReferenceSystem
open PRo3D.Surfaces
open PRo3D.Viewer

type PropertyActions =
    | DrawingMessage    of Drawing.Action
    | AnnotationMessage of AnnotationProperties.Action

type ViewerAction =                
    | DrawingMessage            of Drawing.Action
    | AnnotationGroupsMessage   of AnnotationGroups.Action
    | NavigationMessage         of Navigation.Action
    | ReferenceSystemMessage    of ReferenceSystemApp.Action
    | AnnotationMessage         of AnnotationProperties.Action
    | BookmarkMessage           of Bookmarks.Action
    | SetCamera of CameraView        
    | ImportSurface 
    | ImportMeasurements 
    | DnSProperties             of PropertyActions
    | ConfigPropertiesMessage   of ConfigProperties.Action
    | QuickLoad1
    | QuickLoad2
    | DeleteLast
    | AddSg                     of ISg
    | PickSurface               of SceneHit*string
    | SaveScene
    | SaveAs                    
    | OpenScene
    | LoadScene                 of string
    | NewScene
    | KeyDown                   of key : Aardvark.Application.Keys
    | KeyUp                     of key : Aardvark.Application.Keys      
    | SetInteraction            of Interactions        
        
    | SurfaceActions            of SurfaceApp.Action


module ViewerUtils =
    
    let colormap = 
        let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
        FileTexture("HueColorMap.png",config) :> ITexture

    let pickable' (pick :IMod<Pickable>) (sg: ISg) =
        Sg.PickableApplicator (pick, Mod.constant sg)

    let viewSingleSurfaceSg (surface : MSgSurface) (frustum : IMod<Frustum>) (selectedSurf : IMod<Option<MSurface>>) (globalBB : IMod<Box3d>) =
        let isSelected = 
            Mod.bind2 (fun guid (selected: Option<MSurface>) ->
                match selected with
                    | Some s -> Mod.map (fun selectedGuid -> selectedGuid = guid) s.guid
                    | None -> Mod.constant false
            ) surface.surface.guid selectedSurf

        let createSg (sg : ISg) =
            sg 
            |> Sg.noEvents 
            |> Sg.cullMode(surface.surface.cullMode) 
            |> Sg.fillMode(surface.surface.fillMode)         
                
        let pickable = 
            adaptive {
                let! bb = globalBB
                return { shape = PickShape.Box bb; trafo = Trafo3d.Identity }
            }        

        let rangeToMinMax (s:MSgSurface) =
            adaptive {
                let! scalar = surface.surface.selectedScalar
                return match scalar with 
                          | Some s -> V2d(s.definedRange.Min, s.definedRange.Max)
                          | None   -> V2d(0.0,1.0)
            }
                
        let sg = 
            surface.sceneGraph 
                |> Mod.map createSg
                |> Sg.dynamic
                |> Sg.uniform "selected"       isSelected
                |> Sg.uniform "selectionColor" (Mod.constant (C4b (200uy,200uy,255uy,255uy)))   
                |> Sg.uniform "falseColors"    (surface.surface.selectedScalar |> Mod.map Option.isSome)
                |> Sg.uniform "MinMax"         (surface |> rangeToMinMax)             
                |> Sg.texture (Sym.ofString "ColorMapTexture") (Mod.constant colormap)
                |> Sg.onOff(surface.surface.isVisible)                             
                |> Sg.LodParameters( 
                    adaptive {
                        let! frustum = frustum 
                        let sizes = V2i(1024,768)
                        let! quality = surface.surface.quality.value
                        let! trafo = surface.surface.trafo

                        return { frustum = frustum; size = sizes; factor = Math.Pow(Math.E, quality); trafo = trafo }
                    })   
                |> Sg.AttributeParameters(
                    adaptive {
                        let! texture = surface.surface.selectedTexture 
                        let! scalar = surface.surface.selectedScalar

                        let attr : Aardvark.Opc.AttributeParameters = 
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
                                true, Seq.ofList [PickSurface (sceneHit,(surface.surface.name |> Mod.force))])                        
                    ]
        sg

    let frustum (m:MModel) =
        let near = m.scene.config.nearPlane.value
        let far = m.scene.config.farPlane.value
        (Navigation.UI.frustum near far)

    let renderCommands sgGrouped overlayed (m:MModel) =
        let grouped = 
            sgGrouped |> AList.map(
                fun x -> ( x 
                    |> AMap.map(fun _ sf -> 
                        viewSingleSurfaceSg sf (frustum m) m.scene.surfacesModel.selectedSurface sf.globalBB)
                    |> AMap.toASet 
                    |> ASet.map snd 
                )
            )
            
        alist {                
            for set in grouped do
                let sg = 
                    set 
                        |> Sg.set
                        |> Sg.effect [
                            Shader.stableTrafo |> toEffect                  
                            DefaultSurfaces.diffuseTexture |> toEffect
                            Shader.selectionColor |> toEffect       
                            Shader.LoDColor |> toEffect
                            Shader.falseColor |> toEffect
                        ]
                        |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
                        |> Sg.uniform "LodVisEnabled" m.scene.config.lodColoring //()

                yield RenderCommand.SceneGraph sg                    
                yield Aardvark.UI.RenderCommand.Clear(None,Some (Mod.constant 1.0))

            yield RenderCommand.SceneGraph overlayed
        }  
