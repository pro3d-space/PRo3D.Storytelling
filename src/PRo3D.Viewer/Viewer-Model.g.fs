namespace PRo3D.Viewer

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Viewer

[<AutoOpen>]
module Mutable =

    
    
    type MSgSurface(__initial : PRo3D.Viewer.SgSurface) =
        inherit obj()
        let mutable __current = __initial
        let _surface = PRo3D.Mutable.MSurface.Create(__initial.surface)
        let _globalBB = ResetMod.Create(__initial.globalBB)
        let _sceneGraph = ResetMod.Create(__initial.sceneGraph)
        let _kdTree = ResetMod.Create(__initial.kdTree)
        
        member x.surface = _surface
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.kdTree = _kdTree :> IMod<_>
        
        member x.Update(v : PRo3D.Viewer.SgSurface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                PRo3D.Mutable.MSurface.Update(_surface, v.surface)
                ResetMod.Update(_globalBB,v.globalBB)
                ResetMod.Update(_sceneGraph,v.sceneGraph)
                ResetMod.Update(_kdTree,v.kdTree)
                
        
        static member Create(__initial : PRo3D.Viewer.SgSurface) : MSgSurface = MSgSurface(__initial)
        static member Update(m : MSgSurface, v : PRo3D.Viewer.SgSurface) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Viewer.SgSurface> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SgSurface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let surface =
                { new Lens<PRo3D.Viewer.SgSurface, PRo3D.Surface>() with
                    override x.Get(r) = r.surface
                    override x.Set(r,v) = { r with surface = v }
                    override x.Update(r,f) = { r with surface = f r.surface }
                }
            let globalBB =
                { new Lens<PRo3D.Viewer.SgSurface, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.globalBB
                    override x.Set(r,v) = { r with globalBB = v }
                    override x.Update(r,f) = { r with globalBB = f r.globalBB }
                }
            let sceneGraph =
                { new Lens<PRo3D.Viewer.SgSurface, Aardvark.SceneGraph.ISg>() with
                    override x.Get(r) = r.sceneGraph
                    override x.Set(r,v) = { r with sceneGraph = v }
                    override x.Update(r,f) = { r with sceneGraph = f r.sceneGraph }
                }
            let kdTree =
                { new Lens<PRo3D.Viewer.SgSurface, Aardvark.VRVis.KdTreeSet>() with
                    override x.Get(r) = r.kdTree
                    override x.Set(r,v) = { r with kdTree = v }
                    override x.Update(r,f) = { r with kdTree = f r.kdTree }
                }
    
    
    type MScene(__initial : PRo3D.Viewer.Scene) =
        inherit obj()
        let mutable __current = __initial
        let _cameraModel = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraModel)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _surfaces = ResetMod.Create(__initial.surfaces)
        
        member x.cameraModel = _cameraModel
        member x.interaction = _interaction :> IMod<_>
        member x.surfaces = _surfaces :> IMod<_>
        
        member x.Update(v : PRo3D.Viewer.Scene) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraModel, v.cameraModel)
                ResetMod.Update(_interaction,v.interaction)
                ResetMod.Update(_surfaces,v.surfaces)
                
        
        static member Create(__initial : PRo3D.Viewer.Scene) : MScene = MScene(__initial)
        static member Update(m : MScene, v : PRo3D.Viewer.Scene) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Viewer.Scene> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Scene =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let cameraModel =
                { new Lens<PRo3D.Viewer.Scene, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraModel
                    override x.Set(r,v) = { r with cameraModel = v }
                    override x.Update(r,f) = { r with cameraModel = f r.cameraModel }
                }
            let interaction =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.InteractionMode>() with
                    override x.Get(r) = r.interaction
                    override x.Set(r,v) = { r with interaction = v }
                    override x.Update(r,f) = { r with interaction = f r.interaction }
                }
            let surfaces =
                { new Lens<PRo3D.Viewer.Scene, Microsoft.FSharp.Collections.list<PRo3D.Surface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
    
    
    type MModel(__initial : PRo3D.Viewer.Model) =
        inherit obj()
        let mutable __current = __initial
        let _scene = MScene.Create(__initial.scene)
        let _drawing = PRo3D.Drawing.Mutable.MDrawingModel.Create(__initial.drawing)
        let _surfaces = MSet.Create(unbox, __initial.surfaces, (fun v -> MSgSurface.Create(v)), (fun (m,v) -> MSgSurface.Update(m, v)), (fun v -> v))
        let _properties = ResetMod.Create(__initial.properties)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _picking = ResetMod.Create(__initial.picking)
        let _orientations = PRo3D.Mutable.MOrientationDisplayModel.Create(__initial.orientations)
        
        member x.scene = _scene
        member x.drawing = _drawing
        member x.surfaces = _surfaces :> aset<_>
        member x.properties = _properties :> IMod<_>
        member x.interaction = _interaction :> IMod<_>
        member x.picking = _picking :> IMod<_>
        member x.orientations = _orientations
        
        member x.Update(v : PRo3D.Viewer.Model) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MScene.Update(_scene, v.scene)
                PRo3D.Drawing.Mutable.MDrawingModel.Update(_drawing, v.drawing)
                MSet.Update(_surfaces, v.surfaces)
                ResetMod.Update(_properties,v.properties)
                ResetMod.Update(_interaction,v.interaction)
                ResetMod.Update(_picking,v.picking)
                PRo3D.Mutable.MOrientationDisplayModel.Update(_orientations, v.orientations)
                
        
        static member Create(__initial : PRo3D.Viewer.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : PRo3D.Viewer.Model) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Viewer.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let scene =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Scene>() with
                    override x.Get(r) = r.scene
                    override x.Set(r,v) = { r with scene = v }
                    override x.Update(r,f) = { r with scene = f r.scene }
                }
            let drawing =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Drawing.DrawingModel>() with
                    override x.Get(r) = r.drawing
                    override x.Set(r,v) = { r with drawing = v }
                    override x.Update(r,f) = { r with drawing = f r.drawing }
                }
            let surfaces =
                { new Lens<PRo3D.Viewer.Model, Aardvark.Base.hset<PRo3D.Viewer.SgSurface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
            let properties =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Properties>() with
                    override x.Get(r) = r.properties
                    override x.Set(r,v) = { r with properties = v }
                    override x.Update(r,f) = { r with properties = f r.properties }
                }
            let interaction =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Interactions>() with
                    override x.Get(r) = r.interaction
                    override x.Set(r,v) = { r with interaction = v }
                    override x.Update(r,f) = { r with interaction = f r.interaction }
                }
            let picking =
                { new Lens<PRo3D.Viewer.Model, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.picking
                    override x.Set(r,v) = { r with picking = v }
                    override x.Update(r,f) = { r with picking = f r.picking }
                }
            let orientations =
                { new Lens<PRo3D.Viewer.Model, PRo3D.OrientationDisplayModel>() with
                    override x.Get(r) = r.orientations
                    override x.Set(r,v) = { r with orientations = v }
                    override x.Update(r,f) = { r with orientations = f r.orientations }
                }
