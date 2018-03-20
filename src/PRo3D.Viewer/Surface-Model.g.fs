namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Surfaces

[<AutoOpen>]
module Mutable =

    
    
    type MSurface(__initial : PRo3D.Surfaces.Surface) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.Surface> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.Surface>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.Surface>
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
        let _priority = Aardvark.UI.Mutable.MNumericInput.Create(__initial.priority)
        let _triangleSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.triangleSize)
        let _preTransform = ResetMod.Create(__initial.preTransform)
        let _scalarLayers = MList.Create(__initial.scalarLayers)
        let _selectedScalar = MOption.Create(__initial.selectedScalar)
        let _textureLayers = MList.Create(__initial.textureLayers)
        let _selectedTexture = MOption.Create(__initial.selectedTexture)
        
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
        member x.priority = _priority
        member x.triangleSize = _triangleSize
        member x.preTransform = _preTransform :> IMod<_>
        member x.scalarLayers = _scalarLayers :> alist<_>
        member x.selectedScalar = _selectedScalar :> IMod<_>
        member x.textureLayers = _textureLayers :> alist<_>
        member x.selectedTexture = _selectedTexture :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.Surface) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
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
                Aardvark.UI.Mutable.MNumericInput.Update(_priority, v.priority)
                Aardvark.UI.Mutable.MNumericInput.Update(_triangleSize, v.triangleSize)
                ResetMod.Update(_preTransform,v.preTransform)
                MList.Update(_scalarLayers, v.scalarLayers)
                MOption.Update(_selectedScalar, v.selectedScalar)
                MList.Update(_textureLayers, v.textureLayers)
                MOption.Update(_selectedTexture, v.selectedTexture)
                
        
        static member Create(__initial : PRo3D.Surfaces.Surface) : MSurface = MSurface(__initial)
        static member Update(m : MSurface, v : PRo3D.Surfaces.Surface) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
            let priority =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.priority
                    override x.Set(r,v) = { r with priority = v }
                    override x.Update(r,f) = { r with priority = f r.priority }
                }
            let triangleSize =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.triangleSize
                    override x.Set(r,v) = { r with triangleSize = v }
                    override x.Update(r,f) = { r with triangleSize = f r.triangleSize }
                }
            let preTransform =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.preTransform
                    override x.Set(r,v) = { r with preTransform = v }
                    override x.Update(r,f) = { r with preTransform = f r.preTransform }
                }
            let scalarLayers =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.plist<PRo3D.Surfaces.ScalarLayer>>() with
                    override x.Get(r) = r.scalarLayers
                    override x.Set(r,v) = { r with scalarLayers = v }
                    override x.Update(r,f) = { r with scalarLayers = f r.scalarLayers }
                }
            let selectedScalar =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.option<PRo3D.Surfaces.ScalarLayer>>() with
                    override x.Get(r) = r.selectedScalar
                    override x.Set(r,v) = { r with selectedScalar = v }
                    override x.Update(r,f) = { r with selectedScalar = f r.selectedScalar }
                }
            let textureLayers =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.plist<PRo3D.Surfaces.TextureLayer>>() with
                    override x.Get(r) = r.textureLayers
                    override x.Set(r,v) = { r with textureLayers = v }
                    override x.Update(r,f) = { r with textureLayers = f r.textureLayers }
                }
            let selectedTexture =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.option<PRo3D.Surfaces.TextureLayer>>() with
                    override x.Get(r) = r.selectedTexture
                    override x.Set(r,v) = { r with selectedTexture = v }
                    override x.Update(r,f) = { r with selectedTexture = f r.selectedTexture }
                }
    
    
    type MSgSurface(__initial : PRo3D.Surfaces.SgSurface) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.SgSurface> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.SgSurface>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.SgSurface>
        let _trafo = Aardvark.UI.Trafos.Mutable.MTransformation.Create(__initial.trafo)
        let _globalBB = ResetMod.Create(__initial.globalBB)
        let _sceneGraph = ResetMod.Create(__initial.sceneGraph)
        let _kdTree = ResetMod.Create(__initial.kdTree)
        
        member x.surface = __current.Value.surface
        member x.trafo = _trafo
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.kdTree = _kdTree :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.SgSurface) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Trafos.Mutable.MTransformation.Update(_trafo, v.trafo)
                ResetMod.Update(_globalBB,v.globalBB)
                ResetMod.Update(_sceneGraph,v.sceneGraph)
                ResetMod.Update(_kdTree,v.kdTree)
                
        
        static member Create(__initial : PRo3D.Surfaces.SgSurface) : MSgSurface = MSgSurface(__initial)
        static member Update(m : MSgSurface, v : PRo3D.Surfaces.SgSurface) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Surfaces.SgSurface> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SgSurface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let surface =
                { new Lens<PRo3D.Surfaces.SgSurface, System.Guid>() with
                    override x.Get(r) = r.surface
                    override x.Set(r,v) = { r with surface = v }
                    override x.Update(r,f) = { r with surface = f r.surface }
                }
            let trafo =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.UI.Trafos.Transformation>() with
                    override x.Get(r) = r.trafo
                    override x.Set(r,v) = { r with trafo = v }
                    override x.Update(r,f) = { r with trafo = f r.trafo }
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
                { new Lens<PRo3D.Surfaces.SgSurface, Microsoft.FSharp.Core.obj>() with
                    override x.Get(r) = r.kdTree
                    override x.Set(r,v) = { r with kdTree = v }
                    override x.Update(r,f) = { r with kdTree = f r.kdTree }
                }
