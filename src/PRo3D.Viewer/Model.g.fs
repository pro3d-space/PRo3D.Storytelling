namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    type MSurface(__initial : PRo3D.Surface) = 
        let mutable __current = __initial
        let _guid = ResetMod.Create(__initial.guid)
        let _name = ResetMod.Create(__initial.name)
        let _importPath = ResetMod.Create(__initial.importPath)
        let _opcPaths = ResetMod.Create(__initial.opcPaths)
        let _globalBB = ResetMod.Create(__initial.globalBB)
        let _sceneGraph = ResetMod.Create(__initial.sceneGraph)
        let _kdTree = ResetMod.Create(__initial.kdTree)
        
        member x.guid = _guid :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.importPath = _importPath :> IMod<_>
        member x.opcPaths = _opcPaths :> IMod<_>
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.kdTree = _kdTree :> IMod<_>
        
        member x.Update(v : PRo3D.Surface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_guid,v.guid)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_importPath,v.importPath)
                ResetMod.Update(_opcPaths,v.opcPaths)
                ResetMod.Update(_globalBB,v.globalBB)
                ResetMod.Update(_sceneGraph,v.sceneGraph)
                ResetMod.Update(_kdTree,v.kdTree)
        
        static member Create(v : PRo3D.Surface) = MSurface(v)
        static member Update(m : MSurface, v : PRo3D.Surface) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let guid =
                { new Lens<PRo3D.Surface, System.Guid>() with
                    override x.Get(r) = r.guid
                    override x.Set(r,v) = { r with guid = v }
                    override x.Update(r,f) = { r with guid = f r.guid }
                }
            let name =
                { new Lens<PRo3D.Surface, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let importPath =
                { new Lens<PRo3D.Surface, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.importPath
                    override x.Set(r,v) = { r with importPath = v }
                    override x.Update(r,f) = { r with importPath = f r.importPath }
                }
            let opcPaths =
                { new Lens<PRo3D.Surface, Microsoft.FSharp.Collections.list<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.opcPaths
                    override x.Set(r,v) = { r with opcPaths = v }
                    override x.Update(r,f) = { r with opcPaths = f r.opcPaths }
                }
            let globalBB =
                { new Lens<PRo3D.Surface, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.globalBB
                    override x.Set(r,v) = { r with globalBB = v }
                    override x.Update(r,f) = { r with globalBB = f r.globalBB }
                }
            let sceneGraph =
                { new Lens<PRo3D.Surface, Aardvark.SceneGraph.ISg>() with
                    override x.Get(r) = r.sceneGraph
                    override x.Set(r,v) = { r with sceneGraph = v }
                    override x.Update(r,f) = { r with sceneGraph = f r.sceneGraph }
                }
            let kdTree =
                { new Lens<PRo3D.Surface, Aardvark.VRVis.KdTreeSet>() with
                    override x.Get(r) = r.kdTree
                    override x.Set(r,v) = { r with kdTree = v }
                    override x.Update(r,f) = { r with kdTree = f r.kdTree }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MScene(__initial : PRo3D.Scene) = 
        let mutable __current = __initial
        let _cameraModel = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraModel)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _surfaces = ResetMod.Create(__initial.surfaces)
        
        member x.cameraModel = _cameraModel
        member x.interaction = _interaction :> IMod<_>
        member x.surfaces = _surfaces :> IMod<_>
        
        member x.Update(v : PRo3D.Scene) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraModel, v.cameraModel)
                ResetMod.Update(_interaction,v.interaction)
                ResetMod.Update(_surfaces,v.surfaces)
        
        static member Create(v : PRo3D.Scene) = MScene(v)
        static member Update(m : MScene, v : PRo3D.Scene) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Scene =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let cameraModel =
                { new Lens<PRo3D.Scene, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraModel
                    override x.Set(r,v) = { r with cameraModel = v }
                    override x.Update(r,f) = { r with cameraModel = f r.cameraModel }
                }
            let interaction =
                { new Lens<PRo3D.Scene, PRo3D.InteractionMode>() with
                    override x.Get(r) = r.interaction
                    override x.Set(r,v) = { r with interaction = v }
                    override x.Update(r,f) = { r with interaction = f r.interaction }
                }
            let surfaces =
                { new Lens<PRo3D.Scene, Microsoft.FSharp.Collections.list<PRo3D.Surface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MModel(__initial : PRo3D.Model) = 
        let mutable __current = __initial
        let _scene = MScene.Create(__initial.scene)
        let _surfaces = MSet.Create((fun (v : PRo3D.Surface) -> v.guid :> obj), __initial.surfaces, (fun v -> MSurface.Create(v)), (fun (m,v) -> MSurface.Update(m, v)), (fun v -> v))
        let _draw = ResetMod.Create(__initial.draw)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        
        member x.scene = _scene
        member x.surfaces = _surfaces :> aset<_>
        member x.draw = _draw :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        
        member x.Update(v : PRo3D.Model) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MScene.Update(_scene, v.scene)
                MSet.Update(_surfaces, v.surfaces)
                ResetMod.Update(_draw,v.draw)
                MOption.Update(_hoverPosition, v.hoverPosition)
        
        static member Create(v : PRo3D.Model) = MModel(v)
        static member Update(m : MModel, v : PRo3D.Model) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let scene =
                { new Lens<PRo3D.Model, PRo3D.Scene>() with
                    override x.Get(r) = r.scene
                    override x.Set(r,v) = { r with scene = v }
                    override x.Update(r,f) = { r with scene = f r.scene }
                }
            let surfaces =
                { new Lens<PRo3D.Model, Aardvark.Base.hset<PRo3D.Surface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
            let draw =
                { new Lens<PRo3D.Model, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
            let hoverPosition =
                { new Lens<PRo3D.Model, Microsoft.FSharp.Core.option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
