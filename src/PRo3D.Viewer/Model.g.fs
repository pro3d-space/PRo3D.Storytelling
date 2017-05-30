namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    [<System.Runtime.CompilerServices.Extension>]
    type MSurface private(__initial : PRo3D.Surface) =
        let mutable __current = __initial
        let _guid = ResetMod(__initial.guid)
        let _name = ResetMod(__initial.name)
        let _importPath = ResetMod(__initial.importPath)
        let _opcPaths = ResetMod(__initial.opcPaths)
        
        member x.guid = _guid :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.importPath = _importPath :> IMod<_>
        member x.opcPaths = _opcPaths :> IMod<_>
        
        member x.Update(__model : PRo3D.Surface) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _guid.Update(__model.guid)
                _name.Update(__model.name)
                _importPath.Update(__model.importPath)
                _opcPaths.Update(__model.opcPaths)
        
        static member Update(__self : MSurface, __model : PRo3D.Surface) = __self.Update(__model)
        
        static member Create(initial) = MSurface(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MSurface =
        let inline guid (m : MSurface) = m.guid
        let inline name (m : MSurface) = m.name
        let inline importPath (m : MSurface) = m.importPath
        let inline opcPaths (m : MSurface) = m.opcPaths
    
    
    
    
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
    [<StructuredFormatDisplay("{AsString}")>]
    [<System.Runtime.CompilerServices.Extension>]
    type MSgSurface private(__initial : PRo3D.SgSurface) =
        let mutable __current = __initial
        let _surface = MSurface.Create(__initial.surface)
        let _globalBB = ResetMod(__initial.globalBB)
        let _sceneGraph = ResetMod(__initial.sceneGraph)
        let _kdTree = ResetMod(__initial.kdTree)
        
        member x.surface = _surface
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.kdTree = _kdTree :> IMod<_>
        
        member x.Update(__model : PRo3D.SgSurface) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _surface.Update(__model.surface)
                _globalBB.Update(__model.globalBB)
                _sceneGraph.Update(__model.sceneGraph)
                _kdTree.Update(__model.kdTree)
        
        static member Update(__self : MSgSurface, __model : PRo3D.SgSurface) = __self.Update(__model)
        
        static member Create(initial) = MSgSurface(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MSgSurface =
        let inline surface (m : MSgSurface) = m.surface
        let inline globalBB (m : MSgSurface) = m.globalBB
        let inline sceneGraph (m : MSgSurface) = m.sceneGraph
        let inline kdTree (m : MSgSurface) = m.kdTree
    
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SgSurface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let surface =
                { new Lens<PRo3D.SgSurface, PRo3D.Surface>() with
                    override x.Get(r) = r.surface
                    override x.Set(r,v) = { r with surface = v }
                    override x.Update(r,f) = { r with surface = f r.surface }
                }
            let globalBB =
                { new Lens<PRo3D.SgSurface, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.globalBB
                    override x.Set(r,v) = { r with globalBB = v }
                    override x.Update(r,f) = { r with globalBB = f r.globalBB }
                }
            let sceneGraph =
                { new Lens<PRo3D.SgSurface, Aardvark.SceneGraph.ISg>() with
                    override x.Get(r) = r.sceneGraph
                    override x.Set(r,v) = { r with sceneGraph = v }
                    override x.Update(r,f) = { r with sceneGraph = f r.sceneGraph }
                }
            let kdTree =
                { new Lens<PRo3D.SgSurface, Aardvark.VRVis.KdTreeSet>() with
                    override x.Get(r) = r.kdTree
                    override x.Set(r,v) = { r with kdTree = v }
                    override x.Update(r,f) = { r with kdTree = f r.kdTree }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    [<System.Runtime.CompilerServices.Extension>]
    type MScene private(__initial : PRo3D.Scene) =
        let mutable __current = __initial
        let _cameraModel = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraModel)
        let _interaction = ResetMod(__initial.interaction)
        let _surfaces = ResetMod(__initial.surfaces)
        
        member x.cameraModel = _cameraModel
        member x.interaction = _interaction :> IMod<_>
        member x.surfaces = _surfaces :> IMod<_>
        
        member x.Update(__model : PRo3D.Scene) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _cameraModel.Update(__model.cameraModel)
                _interaction.Update(__model.interaction)
                _surfaces.Update(__model.surfaces)
        
        static member Update(__self : MScene, __model : PRo3D.Scene) = __self.Update(__model)
        
        static member Create(initial) = MScene(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MScene =
        let inline cameraModel (m : MScene) = m.cameraModel
        let inline interaction (m : MScene) = m.interaction
        let inline surfaces (m : MScene) = m.surfaces
    
    
    
    
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
    [<System.Runtime.CompilerServices.Extension>]
    type MModel private(__initial : PRo3D.Model) =
        let mutable __current = __initial
        let _scene = MScene.Create(__initial.scene)
        let _surfaces = ResetMapSet(unbox, __initial.surfaces, MSgSurface.Create, MSgSurface.Update)
        let _draw = ResetMod(__initial.draw)
        let _hoverPosition = ResetMod(__initial.hoverPosition)
        
        member x.scene = _scene
        member x.surfaces = _surfaces :> aset<_>
        member x.draw = _draw :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        
        member x.Update(__model : PRo3D.Model) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _scene.Update(__model.scene)
                _surfaces.Update(__model.surfaces)
                _draw.Update(__model.draw)
                _hoverPosition.Update(__model.hoverPosition)
        
        static member Update(__self : MModel, __model : PRo3D.Model) = __self.Update(__model)
        
        static member Create(initial) = MModel(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MModel =
        let inline scene (m : MModel) = m.scene
        let inline surfaces (m : MModel) = m.surfaces
        let inline draw (m : MModel) = m.draw
        let inline hoverPosition (m : MModel) = m.hoverPosition
    
    
    
    
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
                { new Lens<PRo3D.Model, Aardvark.Base.hset<PRo3D.SgSurface>>() with
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
