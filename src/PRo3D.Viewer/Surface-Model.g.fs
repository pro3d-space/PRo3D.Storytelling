namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Surfaces

[<AutoOpen>]
module Mutable =

    
    
    type MSurface(__initial : PRo3D.Surfaces.Surface) =
        inherit obj()
        let mutable __current = __initial
        let _guid = ResetMod.Create(__initial.guid)
        let _name = ResetMod.Create(__initial.name)
        let _importPath = ResetMod.Create(__initial.importPath)
        let _opcNames = ResetMod.Create(__initial.opcNames)
        let _opcPaths = ResetMod.Create(__initial.opcPaths)
        let _relativePaths = ResetMod.Create(__initial.relativePaths)
        let _fillMode = ResetMod.Create(__initial.fillMode)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _quality = Aardvark.UI.Mutable.MNumericInput.Create(__initial.quality)
        
        member x.guid = _guid :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.importPath = _importPath :> IMod<_>
        member x.opcNames = _opcNames :> IMod<_>
        member x.opcPaths = _opcPaths :> IMod<_>
        member x.relativePaths = _relativePaths :> IMod<_>
        member x.fillMode = _fillMode :> IMod<_>
        member x.cullMode = _cullMode :> IMod<_>
        member x.isVisible = _isVisible :> IMod<_>
        member x.quality = _quality
        
        member x.Update(v : PRo3D.Surfaces.Surface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_guid,v.guid)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_importPath,v.importPath)
                ResetMod.Update(_opcNames,v.opcNames)
                ResetMod.Update(_opcPaths,v.opcPaths)
                ResetMod.Update(_relativePaths,v.relativePaths)
                ResetMod.Update(_fillMode,v.fillMode)
                ResetMod.Update(_cullMode,v.cullMode)
                ResetMod.Update(_isVisible,v.isVisible)
                Aardvark.UI.Mutable.MNumericInput.Update(_quality, v.quality)
                
        
        static member Create(__initial : PRo3D.Surfaces.Surface) : MSurface = MSurface(__initial)
        static member Update(m : MSurface, v : PRo3D.Surfaces.Surface) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Surfaces.Surface> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let guid =
                { new Lens<PRo3D.Surfaces.Surface, System.Guid>() with
                    override x.Get(r) = r.guid
                    override x.Set(r,v) = { r with guid = v }
                    override x.Update(r,f) = { r with guid = f r.guid }
                }
            let name =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let importPath =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.importPath
                    override x.Set(r,v) = { r with importPath = v }
                    override x.Update(r,f) = { r with importPath = f r.importPath }
                }
            let opcNames =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Collections.list<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.opcNames
                    override x.Set(r,v) = { r with opcNames = v }
                    override x.Update(r,f) = { r with opcNames = f r.opcNames }
                }
            let opcPaths =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Collections.list<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.opcPaths
                    override x.Set(r,v) = { r with opcPaths = v }
                    override x.Update(r,f) = { r with opcPaths = f r.opcPaths }
                }
            let relativePaths =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.relativePaths
                    override x.Set(r,v) = { r with relativePaths = v }
                    override x.Update(r,f) = { r with relativePaths = f r.relativePaths }
                }
            let fillMode =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.Rendering.FillMode>() with
                    override x.Get(r) = r.fillMode
                    override x.Set(r,v) = { r with fillMode = v }
                    override x.Update(r,f) = { r with fillMode = f r.fillMode }
                }
            let cullMode =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
            let isVisible =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
            let quality =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.quality
                    override x.Set(r,v) = { r with quality = v }
                    override x.Update(r,f) = { r with quality = f r.quality }
                }
    
    
    type MSgSurface(__initial : PRo3D.Surfaces.SgSurface) =
        inherit obj()
        let mutable __current = __initial
        let _surface = MSurface.Create(__initial.surface)
        let _globalBB = ResetMod.Create(__initial.globalBB)
        let _sceneGraph = ResetMod.Create(__initial.sceneGraph)
        let _kdTree = MMap.Create(__initial.kdTree)
        
        member x.surface = _surface
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.kdTree = _kdTree :> amap<_,_>
        
        member x.Update(v : PRo3D.Surfaces.SgSurface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MSurface.Update(_surface, v.surface)
                ResetMod.Update(_globalBB,v.globalBB)
                ResetMod.Update(_sceneGraph,v.sceneGraph)
                MMap.Update(_kdTree, v.kdTree)
                
        
        static member Create(__initial : PRo3D.Surfaces.SgSurface) : MSgSurface = MSgSurface(__initial)
        static member Update(m : MSgSurface, v : PRo3D.Surfaces.SgSurface) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Surfaces.SgSurface> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SgSurface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let surface =
                { new Lens<PRo3D.Surfaces.SgSurface, PRo3D.Surfaces.Surface>() with
                    override x.Get(r) = r.surface
                    override x.Set(r,v) = { r with surface = v }
                    override x.Update(r,f) = { r with surface = f r.surface }
                }
            let globalBB =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.globalBB
                    override x.Set(r,v) = { r with globalBB = v }
                    override x.Update(r,f) = { r with globalBB = f r.globalBB }
                }
            let sceneGraph =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.SceneGraph.ISg>() with
                    override x.Get(r) = r.sceneGraph
                    override x.Set(r,v) = { r with sceneGraph = v }
                    override x.Update(r,f) = { r with sceneGraph = f r.sceneGraph }
                }
            let kdTree =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.Base.hmap<Aardvark.Base.Box3d,Aardvark.Opc.KdTrees.Level0KdTree>>() with
                    override x.Get(r) = r.kdTree
                    override x.Set(r,v) = { r with kdTree = v }
                    override x.Update(r,f) = { r with kdTree = f r.kdTree }
                }
    
    
    type MSurfaceModel(__initial : PRo3D.Surfaces.SurfaceModel) =
        inherit obj()
        let mutable __current = __initial
        let _surfaces = ResetMod.Create(__initial.surfaces)
        let _selectedSurface = MOption.Create(__initial.selectedSurface, (fun v -> MSurface.Create(v)), (fun (m,v) -> MSurface.Update(m, v)), (fun v -> v))
        let _sgSurfaces = MMap.Create(__initial.sgSurfaces, (fun v -> MSgSurface.Create(v)), (fun (m,v) -> MSgSurface.Update(m, v)), (fun v -> v))
        
        member x.surfaces = _surfaces :> IMod<_>
        member x.selectedSurface = _selectedSurface :> IMod<_>
        member x.sgSurfaces = _sgSurfaces :> amap<_,_>
        
        member x.Update(v : PRo3D.Surfaces.SurfaceModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_surfaces,v.surfaces)
                MOption.Update(_selectedSurface, v.selectedSurface)
                MMap.Update(_sgSurfaces, v.sgSurfaces)
                
        
        static member Create(__initial : PRo3D.Surfaces.SurfaceModel) : MSurfaceModel = MSurfaceModel(__initial)
        static member Update(m : MSurfaceModel, v : PRo3D.Surfaces.SurfaceModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Surfaces.SurfaceModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SurfaceModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let surfaces =
                { new Lens<PRo3D.Surfaces.SurfaceModel, Microsoft.FSharp.Collections.list<PRo3D.Surfaces.Surface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
            let selectedSurface =
                { new Lens<PRo3D.Surfaces.SurfaceModel, Microsoft.FSharp.Core.option<PRo3D.Surfaces.Surface>>() with
                    override x.Get(r) = r.selectedSurface
                    override x.Set(r,v) = { r with selectedSurface = v }
                    override x.Update(r,f) = { r with selectedSurface = f r.selectedSurface }
                }
            let sgSurfaces =
                { new Lens<PRo3D.Surfaces.SurfaceModel, Aardvark.Base.hmap<System.Guid,PRo3D.Surfaces.SgSurface>>() with
                    override x.Get(r) = r.sgSurfaces
                    override x.Set(r,v) = { r with sgSurfaces = v }
                    override x.Update(r,f) = { r with sgSurfaces = f r.sgSurfaces }
                }
