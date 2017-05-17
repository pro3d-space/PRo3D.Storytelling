namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    type MSurface private(__initial : PRo3D.Surface) =
        let mutable __current = __initial
        let _guid = ResetMod(__initial.guid)
        let _globalBB = ResetMod(__initial.globalBB)
        let _sceneGraph = ResetMod(__initial.sceneGraph)
        
        member x.guid = _guid :> IMod<_>
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        
        member x.Update(__model : PRo3D.Surface) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _guid.Update(__model.guid)
                _globalBB.Update(__model.globalBB)
                _sceneGraph.Update(__model.sceneGraph)
        
        static member Create(initial) = MSurface(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MSurface =
        let inline guid (m : MSurface) = m.guid
        let inline globalBB (m : MSurface) = m.globalBB
        let inline sceneGraph (m : MSurface) = m.sceneGraph
    
    
    
    
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
    [<StructuredFormatDisplay("{AsString}")>]
    type MModel private(__initial : PRo3D.Model) =
        let mutable __current = __initial
        let _cameraModel = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraModel)
        let _importOpcs = ResetMod(__initial.importOpcs)
        let _surfaces = ResetMapSet((fun v -> v.guid :> obj), __initial.surfaces, MSurface.Create, fun (m,i) -> m.Update(i))
        
        member x.cameraModel = _cameraModel
        member x.importOpcs = _importOpcs :> IMod<_>
        member x.surfaces = _surfaces :> aset<_>
        
        member x.Update(__model : PRo3D.Model) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _cameraModel.Update(__model.cameraModel)
                _importOpcs.Update(__model.importOpcs)
                _surfaces.Update(__model.surfaces)
        
        static member Create(initial) = MModel(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MModel =
        let inline cameraModel (m : MModel) = m.cameraModel
        let inline importOpcs (m : MModel) = m.importOpcs
        let inline surfaces (m : MModel) = m.surfaces
    
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let cameraModel =
                { new Lens<PRo3D.Model, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraModel
                    override x.Set(r,v) = { r with cameraModel = v }
                    override x.Update(r,f) = { r with cameraModel = f r.cameraModel }
                }
            let importOpcs =
                { new Lens<PRo3D.Model, Microsoft.FSharp.Collections.list<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.importOpcs
                    override x.Set(r,v) = { r with importOpcs = v }
                    override x.Update(r,f) = { r with importOpcs = f r.importOpcs }
                }
            let surfaces =
                { new Lens<PRo3D.Model, Aardvark.Base.hset<PRo3D.Surface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
