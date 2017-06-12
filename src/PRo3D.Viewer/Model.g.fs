namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    type MDipAndStrikeResults(__initial : PRo3D.DipAndStrikeResults) = 
        let mutable __current = __initial
        let _plane = ResetMod.Create(__initial.plane)
        let _dipAngle = ResetMod.Create(__initial.dipAngle)
        let _dipDirection = ResetMod.Create(__initial.dipDirection)
        let _strikeDirection = ResetMod.Create(__initial.strikeDirection)
        let _dipAzimuth = ResetMod.Create(__initial.dipAzimuth)
        let _strikeAzimuth = ResetMod.Create(__initial.strikeAzimuth)
        let _centerOfMass = ResetMod.Create(__initial.centerOfMass)
        
        member x.plane = _plane :> IMod<_>
        member x.dipAngle = _dipAngle :> IMod<_>
        member x.dipDirection = _dipDirection :> IMod<_>
        member x.strikeDirection = _strikeDirection :> IMod<_>
        member x.dipAzimuth = _dipAzimuth :> IMod<_>
        member x.strikeAzimuth = _strikeAzimuth :> IMod<_>
        member x.centerOfMass = _centerOfMass :> IMod<_>
        
        member x.Update(v : PRo3D.DipAndStrikeResults) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_plane,v.plane)
                ResetMod.Update(_dipAngle,v.dipAngle)
                ResetMod.Update(_dipDirection,v.dipDirection)
                ResetMod.Update(_strikeDirection,v.strikeDirection)
                ResetMod.Update(_dipAzimuth,v.dipAzimuth)
                ResetMod.Update(_strikeAzimuth,v.strikeAzimuth)
                ResetMod.Update(_centerOfMass,v.centerOfMass)
        
        static member Create(v : PRo3D.DipAndStrikeResults) = MDipAndStrikeResults(v)
        static member Update(m : MDipAndStrikeResults, v : PRo3D.DipAndStrikeResults) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DipAndStrikeResults =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let plane =
                { new Lens<PRo3D.DipAndStrikeResults, Aardvark.Base.Plane3d>() with
                    override x.Get(r) = r.plane
                    override x.Set(r,v) = { r with plane = v }
                    override x.Update(r,f) = { r with plane = f r.plane }
                }
            let dipAngle =
                { new Lens<PRo3D.DipAndStrikeResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.dipAngle
                    override x.Set(r,v) = { r with dipAngle = v }
                    override x.Update(r,f) = { r with dipAngle = f r.dipAngle }
                }
            let dipDirection =
                { new Lens<PRo3D.DipAndStrikeResults, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.dipDirection
                    override x.Set(r,v) = { r with dipDirection = v }
                    override x.Update(r,f) = { r with dipDirection = f r.dipDirection }
                }
            let strikeDirection =
                { new Lens<PRo3D.DipAndStrikeResults, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.strikeDirection
                    override x.Set(r,v) = { r with strikeDirection = v }
                    override x.Update(r,f) = { r with strikeDirection = f r.strikeDirection }
                }
            let dipAzimuth =
                { new Lens<PRo3D.DipAndStrikeResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.dipAzimuth
                    override x.Set(r,v) = { r with dipAzimuth = v }
                    override x.Update(r,f) = { r with dipAzimuth = f r.dipAzimuth }
                }
            let strikeAzimuth =
                { new Lens<PRo3D.DipAndStrikeResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.strikeAzimuth
                    override x.Set(r,v) = { r with strikeAzimuth = v }
                    override x.Update(r,f) = { r with strikeAzimuth = f r.strikeAzimuth }
                }
            let centerOfMass =
                { new Lens<PRo3D.DipAndStrikeResults, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.centerOfMass
                    override x.Set(r,v) = { r with centerOfMass = v }
                    override x.Update(r,f) = { r with centerOfMass = f r.centerOfMass }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MAnnotation(__initial : PRo3D.Annotation) = 
        let mutable __current = __initial
        let _key = ResetMod.Create(__initial.key)
        let _modelTrafo = ResetMod.Create(__initial.modelTrafo)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _projection = ResetMod.Create(__initial.projection)
        let _semantic = ResetMod.Create(__initial.semantic)
        let _points = MList.Create(__initial.points)
        let _segments = MList.Create(__initial.segments, (fun v -> MList.Create(v)), (fun (m,v) -> MList.Update(m, v)), (fun v -> v :> alist<_>))
        let _color = ResetMod.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        let _dnsResults = MOption.Create(__initial.dnsResults, (fun v -> MDipAndStrikeResults.Create(v)), (fun (m,v) -> MDipAndStrikeResults.Update(m, v)), (fun v -> v))
        let _local2global = MOption.Create(__initial.local2global)
        let _visible = ResetMod.Create(__initial.visible)
        let _text = ResetMod.Create(__initial.text)
        
        member x.key = _key :> IMod<_>
        member x.modelTrafo = _modelTrafo :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.semantic = _semantic :> IMod<_>
        member x.points = _points :> alist<_>
        member x.segments = _segments :> alist<_>
        member x.color = _color :> IMod<_>
        member x.thickness = _thickness
        member x.dnsResults = _dnsResults :> IMod<_>
        member x.local2global = _local2global :> IMod<_>
        member x.visible = _visible :> IMod<_>
        member x.text = _text :> IMod<_>
        
        member x.Update(v : PRo3D.Annotation) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_key,v.key)
                ResetMod.Update(_modelTrafo,v.modelTrafo)
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_semantic,v.semantic)
                MList.Update(_points, v.points)
                MList.Update(_segments, v.segments)
                ResetMod.Update(_color,v.color)
                Aardvark.UI.Mutable.MNumericInput.Update(_thickness, v.thickness)
                MOption.Update(_dnsResults, v.dnsResults)
                MOption.Update(_local2global, v.local2global)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_text,v.text)
        
        static member Create(v : PRo3D.Annotation) = MAnnotation(v)
        static member Update(m : MAnnotation, v : PRo3D.Annotation) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Annotation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let key =
                { new Lens<PRo3D.Annotation, System.Guid>() with
                    override x.Get(r) = r.key
                    override x.Set(r,v) = { r with key = v }
                    override x.Update(r,f) = { r with key = f r.key }
                }
            let modelTrafo =
                { new Lens<PRo3D.Annotation, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.modelTrafo
                    override x.Set(r,v) = { r with modelTrafo = v }
                    override x.Update(r,f) = { r with modelTrafo = f r.modelTrafo }
                }
            let geometry =
                { new Lens<PRo3D.Annotation, PRo3D.Geometry>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let projection =
                { new Lens<PRo3D.Annotation, PRo3D.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let semantic =
                { new Lens<PRo3D.Annotation, PRo3D.Semantic>() with
                    override x.Get(r) = r.semantic
                    override x.Set(r,v) = { r with semantic = v }
                    override x.Update(r,f) = { r with semantic = f r.semantic }
                }
            let points =
                { new Lens<PRo3D.Annotation, Aardvark.Base.plist<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.points
                    override x.Set(r,v) = { r with points = v }
                    override x.Update(r,f) = { r with points = f r.points }
                }
            let segments =
                { new Lens<PRo3D.Annotation, Aardvark.Base.plist<Aardvark.Base.plist<Aardvark.Base.V3d>>>() with
                    override x.Get(r) = r.segments
                    override x.Set(r,v) = { r with segments = v }
                    override x.Update(r,f) = { r with segments = f r.segments }
                }
            let color =
                { new Lens<PRo3D.Annotation, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let thickness =
                { new Lens<PRo3D.Annotation, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.thickness
                    override x.Set(r,v) = { r with thickness = v }
                    override x.Update(r,f) = { r with thickness = f r.thickness }
                }
            let dnsResults =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.Option<PRo3D.DipAndStrikeResults>>() with
                    override x.Get(r) = r.dnsResults
                    override x.Set(r,v) = { r with dnsResults = v }
                    override x.Update(r,f) = { r with dnsResults = f r.dnsResults }
                }
            let local2global =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.Option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.local2global
                    override x.Set(r,v) = { r with local2global = v }
                    override x.Update(r,f) = { r with local2global = f r.local2global }
                }
            let visible =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let text =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MDrawingModel(__initial : PRo3D.DrawingModel) = 
        let mutable __current = __initial
        let _draw = ResetMod.Create(__initial.draw)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _semantic = ResetMod.Create(__initial.semantic)
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _exportPath = ResetMod.Create(__initial.exportPath)
        let _selected = MSet.Create(__initial.selected)
        let _selectedAnnotation = MOption.Create(__initial.selectedAnnotation, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.draw = _draw :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.semantic = _semantic :> IMod<_>
        member x.annotations = _annotations :> alist<_>
        member x.exportPath = _exportPath :> IMod<_>
        member x.selected = _selected :> aset<_>
        member x.selectedAnnotation = _selectedAnnotation :> IMod<_>
        
        member x.Update(v : PRo3D.DrawingModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_draw,v.draw)
                MOption.Update(_hoverPosition, v.hoverPosition)
                MOption.Update(_working, v.working)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_semantic,v.semantic)
                MList.Update(_annotations, v.annotations)
                ResetMod.Update(_exportPath,v.exportPath)
                MSet.Update(_selected, v.selected)
                MOption.Update(_selectedAnnotation, v.selectedAnnotation)
        
        static member Create(v : PRo3D.DrawingModel) = MDrawingModel(v)
        static member Update(m : MDrawingModel, v : PRo3D.DrawingModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DrawingModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let draw =
                { new Lens<PRo3D.DrawingModel, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
            let hoverPosition =
                { new Lens<PRo3D.DrawingModel, Microsoft.FSharp.Core.option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
            let working =
                { new Lens<PRo3D.DrawingModel, Microsoft.FSharp.Core.Option<PRo3D.Annotation>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let projection =
                { new Lens<PRo3D.DrawingModel, PRo3D.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let geometry =
                { new Lens<PRo3D.DrawingModel, PRo3D.Geometry>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let semantic =
                { new Lens<PRo3D.DrawingModel, PRo3D.Semantic>() with
                    override x.Get(r) = r.semantic
                    override x.Set(r,v) = { r with semantic = v }
                    override x.Update(r,f) = { r with semantic = f r.semantic }
                }
            let annotations =
                { new Lens<PRo3D.DrawingModel, Aardvark.Base.plist<PRo3D.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let exportPath =
                { new Lens<PRo3D.DrawingModel, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.exportPath
                    override x.Set(r,v) = { r with exportPath = v }
                    override x.Update(r,f) = { r with exportPath = f r.exportPath }
                }
            let selected =
                { new Lens<PRo3D.DrawingModel, Aardvark.Base.hset<System.Guid>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let selectedAnnotation =
                { new Lens<PRo3D.DrawingModel, Microsoft.FSharp.Core.option<PRo3D.Annotation>>() with
                    override x.Get(r) = r.selectedAnnotation
                    override x.Set(r,v) = { r with selectedAnnotation = v }
                    override x.Update(r,f) = { r with selectedAnnotation = f r.selectedAnnotation }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MMeasurementsImporterModel(__initial : PRo3D.MeasurementsImporterModel) = 
        let mutable __current = __initial
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.annotations = _annotations :> alist<_>
        
        member x.Update(v : PRo3D.MeasurementsImporterModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MList.Update(_annotations, v.annotations)
        
        static member Create(v : PRo3D.MeasurementsImporterModel) = MMeasurementsImporterModel(v)
        static member Update(m : MMeasurementsImporterModel, v : PRo3D.MeasurementsImporterModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MeasurementsImporterModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let annotations =
                { new Lens<PRo3D.MeasurementsImporterModel, Aardvark.Base.plist<PRo3D.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MSurface(__initial : PRo3D.Surface) = 
        let mutable __current = __initial
        let _guid = ResetMod.Create(__initial.guid)
        let _name = ResetMod.Create(__initial.name)
        let _importPath = ResetMod.Create(__initial.importPath)
        let _opcPaths = ResetMod.Create(__initial.opcPaths)
        
        member x.guid = _guid :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.importPath = _importPath :> IMod<_>
        member x.opcPaths = _opcPaths :> IMod<_>
        
        member x.Update(v : PRo3D.Surface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_guid,v.guid)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_importPath,v.importPath)
                ResetMod.Update(_opcPaths,v.opcPaths)
        
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
    [<StructuredFormatDisplay("{AsString}")>]
    type MSgSurface(__initial : PRo3D.SgSurface) = 
        let mutable __current = __initial
        let _surface = MSurface.Create(__initial.surface)
        let _globalBB = ResetMod.Create(__initial.globalBB)
        let _sceneGraph = ResetMod.Create(__initial.sceneGraph)
        let _kdTree = ResetMod.Create(__initial.kdTree)
        
        member x.surface = _surface
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.kdTree = _kdTree :> IMod<_>
        
        member x.Update(v : PRo3D.SgSurface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MSurface.Update(_surface, v.surface)
                ResetMod.Update(_globalBB,v.globalBB)
                ResetMod.Update(_sceneGraph,v.sceneGraph)
                ResetMod.Update(_kdTree,v.kdTree)
        
        static member Create(v : PRo3D.SgSurface) = MSgSurface(v)
        static member Update(m : MSgSurface, v : PRo3D.SgSurface) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
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
        let _drawing = MDrawingModel.Create(__initial.drawing)
        let _surfaces = MSet.Create(unbox, __initial.surfaces, (fun v -> MSgSurface.Create(v)), (fun (m,v) -> MSgSurface.Update(m, v)), (fun v -> v))
        let _properties = ResetMod.Create(__initial.properties)
        
        member x.scene = _scene
        member x.drawing = _drawing
        member x.surfaces = _surfaces :> aset<_>
        member x.properties = _properties :> IMod<_>
        
        member x.Update(v : PRo3D.Model) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MScene.Update(_scene, v.scene)
                MDrawingModel.Update(_drawing, v.drawing)
                MSet.Update(_surfaces, v.surfaces)
                ResetMod.Update(_properties,v.properties)
        
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
            let drawing =
                { new Lens<PRo3D.Model, PRo3D.DrawingModel>() with
                    override x.Get(r) = r.drawing
                    override x.Set(r,v) = { r with drawing = v }
                    override x.Update(r,f) = { r with drawing = f r.drawing }
                }
            let surfaces =
                { new Lens<PRo3D.Model, Aardvark.Base.hset<PRo3D.SgSurface>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
            let properties =
                { new Lens<PRo3D.Model, PRo3D.Properties>() with
                    override x.Get(r) = r.properties
                    override x.Set(r,v) = { r with properties = v }
                    override x.Update(r,f) = { r with properties = f r.properties }
                }
