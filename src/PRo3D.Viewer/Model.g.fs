namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    
    
    type MDipAndStrikeResults(__initial : PRo3D.DipAndStrikeResults) =
        inherit obj()
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
                
        
        static member Create(__initial : PRo3D.DipAndStrikeResults) : MDipAndStrikeResults = MDipAndStrikeResults(__initial)
        static member Update(m : MDipAndStrikeResults, v : PRo3D.DipAndStrikeResults) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.DipAndStrikeResults> with
            member x.Update v = x.Update v
    
    
    
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
    
    
    type MAnnotationResults(__initial : PRo3D.AnnotationResults) =
        inherit obj()
        let mutable __current = __initial
        let _height = ResetMod.Create(__initial.height)
        let _heightDelta = ResetMod.Create(__initial.heightDelta)
        let _length = ResetMod.Create(__initial.length)
        let _wayLength = ResetMod.Create(__initial.wayLength)
        
        member x.height = _height :> IMod<_>
        member x.heightDelta = _heightDelta :> IMod<_>
        member x.length = _length :> IMod<_>
        member x.wayLength = _wayLength :> IMod<_>
        
        member x.Update(v : PRo3D.AnnotationResults) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_height,v.height)
                ResetMod.Update(_heightDelta,v.heightDelta)
                ResetMod.Update(_length,v.length)
                ResetMod.Update(_wayLength,v.wayLength)
                
        
        static member Create(__initial : PRo3D.AnnotationResults) : MAnnotationResults = MAnnotationResults(__initial)
        static member Update(m : MAnnotationResults, v : PRo3D.AnnotationResults) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.AnnotationResults> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationResults =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let height =
                { new Lens<PRo3D.AnnotationResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.height
                    override x.Set(r,v) = { r with height = v }
                    override x.Update(r,f) = { r with height = f r.height }
                }
            let heightDelta =
                { new Lens<PRo3D.AnnotationResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.heightDelta
                    override x.Set(r,v) = { r with heightDelta = v }
                    override x.Update(r,f) = { r with heightDelta = f r.heightDelta }
                }
            let length =
                { new Lens<PRo3D.AnnotationResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.length
                    override x.Set(r,v) = { r with length = v }
                    override x.Update(r,f) = { r with length = f r.length }
                }
            let wayLength =
                { new Lens<PRo3D.AnnotationResults, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.wayLength
                    override x.Set(r,v) = { r with wayLength = v }
                    override x.Update(r,f) = { r with wayLength = f r.wayLength }
                }
    
    
    type MSegment(__initial : PRo3D.Segment) =
        inherit obj()
        let mutable __current = __initial
        let _startPoint = ResetMod.Create(__initial.startPoint)
        let _endPoint = ResetMod.Create(__initial.endPoint)
        let _points = MList.Create(__initial.points)
        
        member x.startPoint = _startPoint :> IMod<_>
        member x.endPoint = _endPoint :> IMod<_>
        member x.points = _points :> alist<_>
        
        member x.Update(v : PRo3D.Segment) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_startPoint,v.startPoint)
                ResetMod.Update(_endPoint,v.endPoint)
                MList.Update(_points, v.points)
                
        
        static member Create(__initial : PRo3D.Segment) : MSegment = MSegment(__initial)
        static member Update(m : MSegment, v : PRo3D.Segment) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Segment> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Segment =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let startPoint =
                { new Lens<PRo3D.Segment, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.startPoint
                    override x.Set(r,v) = { r with startPoint = v }
                    override x.Update(r,f) = { r with startPoint = f r.startPoint }
                }
            let endPoint =
                { new Lens<PRo3D.Segment, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.endPoint
                    override x.Set(r,v) = { r with endPoint = v }
                    override x.Update(r,f) = { r with endPoint = f r.endPoint }
                }
            let points =
                { new Lens<PRo3D.Segment, Aardvark.Base.plist<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.points
                    override x.Set(r,v) = { r with points = v }
                    override x.Update(r,f) = { r with points = f r.points }
                }
    
    
    type MAnnotation(__initial : PRo3D.Annotation) =
        inherit obj()
        let mutable __current = __initial
        let _key = ResetMod.Create(__initial.key)
        let _modelTrafo = ResetMod.Create(__initial.modelTrafo)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _projection = ResetMod.Create(__initial.projection)
        let _semantic = ResetMod.Create(__initial.semantic)
        let _points = MList.Create(__initial.points)
        let _segments = MList.Create(__initial.segments, (fun v -> MSegment.Create(v)), (fun (m,v) -> MSegment.Update(m, v)), (fun v -> v))
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        let _results = MOption.Create(__initial.results, (fun v -> MAnnotationResults.Create(v)), (fun (m,v) -> MAnnotationResults.Update(m, v)), (fun v -> v))
        let _dnsResults = MOption.Create(__initial.dnsResults, (fun v -> MDipAndStrikeResults.Create(v)), (fun (m,v) -> MDipAndStrikeResults.Update(m, v)), (fun v -> v))
        let _visible = ResetMod.Create(__initial.visible)
        let _showDns = ResetMod.Create(__initial.showDns)
        let _text = ResetMod.Create(__initial.text)
        let _surfaceName = ResetMod.Create(__initial.surfaceName)
        
        member x.key = _key :> IMod<_>
        member x.modelTrafo = _modelTrafo :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.semantic = _semantic :> IMod<_>
        member x.points = _points :> alist<_>
        member x.segments = _segments :> alist<_>
        member x.color = _color
        member x.thickness = _thickness
        member x.results = _results :> IMod<_>
        member x.dnsResults = _dnsResults :> IMod<_>
        member x.visible = _visible :> IMod<_>
        member x.showDns = _showDns :> IMod<_>
        member x.text = _text :> IMod<_>
        member x.surfaceName = _surfaceName :> IMod<_>
        
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
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                Aardvark.UI.Mutable.MNumericInput.Update(_thickness, v.thickness)
                MOption.Update(_results, v.results)
                MOption.Update(_dnsResults, v.dnsResults)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_showDns,v.showDns)
                ResetMod.Update(_text,v.text)
                ResetMod.Update(_surfaceName,v.surfaceName)
                
        
        static member Create(__initial : PRo3D.Annotation) : MAnnotation = MAnnotation(__initial)
        static member Update(m : MAnnotation, v : PRo3D.Annotation) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Annotation> with
            member x.Update v = x.Update v
    
    
    
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
                { new Lens<PRo3D.Annotation, Aardvark.Base.plist<PRo3D.Segment>>() with
                    override x.Get(r) = r.segments
                    override x.Set(r,v) = { r with segments = v }
                    override x.Update(r,f) = { r with segments = f r.segments }
                }
            let color =
                { new Lens<PRo3D.Annotation, Aardvark.UI.ColorInput>() with
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
            let results =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.Option<PRo3D.AnnotationResults>>() with
                    override x.Get(r) = r.results
                    override x.Set(r,v) = { r with results = v }
                    override x.Update(r,f) = { r with results = f r.results }
                }
            let dnsResults =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.Option<PRo3D.DipAndStrikeResults>>() with
                    override x.Get(r) = r.dnsResults
                    override x.Set(r,v) = { r with dnsResults = v }
                    override x.Update(r,f) = { r with dnsResults = f r.dnsResults }
                }
            let visible =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let showDns =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.showDns
                    override x.Set(r,v) = { r with showDns = v }
                    override x.Update(r,f) = { r with showDns = f r.showDns }
                }
            let text =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let surfaceName =
                { new Lens<PRo3D.Annotation, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.surfaceName
                    override x.Set(r,v) = { r with surfaceName = v }
                    override x.Update(r,f) = { r with surfaceName = f r.surfaceName }
                }
    
    
    type MMeasurementsImporterModel(__initial : PRo3D.MeasurementsImporterModel) =
        inherit obj()
        let mutable __current = __initial
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.annotations = _annotations :> alist<_>
        
        member x.Update(v : PRo3D.MeasurementsImporterModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MList.Update(_annotations, v.annotations)
                
        
        static member Create(__initial : PRo3D.MeasurementsImporterModel) : MMeasurementsImporterModel = MMeasurementsImporterModel(__initial)
        static member Update(m : MMeasurementsImporterModel, v : PRo3D.MeasurementsImporterModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.MeasurementsImporterModel> with
            member x.Update v = x.Update v
    
    
    
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
    
    
    type MSurface(__initial : PRo3D.Surface) =
        inherit obj()
        let mutable __current = __initial
        let _guid = ResetMod.Create(__initial.guid)
        let _name = ResetMod.Create(__initial.name)
        let _importPath = ResetMod.Create(__initial.importPath)
        let _opcPaths = ResetMod.Create(__initial.opcPaths)
        let _fillMode = ResetMod.Create(__initial.fillMode)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        
        member x.guid = _guid :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.importPath = _importPath :> IMod<_>
        member x.opcPaths = _opcPaths :> IMod<_>
        member x.fillMode = _fillMode :> IMod<_>
        member x.cullMode = _cullMode :> IMod<_>
        member x.isVisible = _isVisible :> IMod<_>
        
        member x.Update(v : PRo3D.Surface) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_guid,v.guid)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_importPath,v.importPath)
                ResetMod.Update(_opcPaths,v.opcPaths)
                ResetMod.Update(_fillMode,v.fillMode)
                ResetMod.Update(_cullMode,v.cullMode)
                ResetMod.Update(_isVisible,v.isVisible)
                
        
        static member Create(__initial : PRo3D.Surface) : MSurface = MSurface(__initial)
        static member Update(m : MSurface, v : PRo3D.Surface) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Surface> with
            member x.Update v = x.Update v
    
    
    
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
            let fillMode =
                { new Lens<PRo3D.Surface, Aardvark.Base.Rendering.FillMode>() with
                    override x.Get(r) = r.fillMode
                    override x.Set(r,v) = { r with fillMode = v }
                    override x.Update(r,f) = { r with fillMode = f r.fillMode }
                }
            let cullMode =
                { new Lens<PRo3D.Surface, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
            let isVisible =
                { new Lens<PRo3D.Surface, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
    
    
    type MReferenceSystem(__initial : PRo3D.ReferenceSystem) =
        inherit obj()
        let mutable __current = __initial
        let _origin = ResetMod.Create(__initial.origin)
        let _north = ResetMod.Create(__initial.north)
        let _up = Aardvark.UI.Mutable.MV3dInput.Create(__initial.up)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _arrowScale = Aardvark.UI.Mutable.MNumericInput.Create(__initial.arrowScale)
        
        member x.origin = _origin :> IMod<_>
        member x.north = _north :> IMod<_>
        member x.up = _up
        member x.isVisible = _isVisible :> IMod<_>
        member x.arrowScale = _arrowScale
        
        member x.Update(v : PRo3D.ReferenceSystem) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_origin,v.origin)
                ResetMod.Update(_north,v.north)
                Aardvark.UI.Mutable.MV3dInput.Update(_up, v.up)
                ResetMod.Update(_isVisible,v.isVisible)
                Aardvark.UI.Mutable.MNumericInput.Update(_arrowScale, v.arrowScale)
                
        
        static member Create(__initial : PRo3D.ReferenceSystem) : MReferenceSystem = MReferenceSystem(__initial)
        static member Update(m : MReferenceSystem, v : PRo3D.ReferenceSystem) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.ReferenceSystem> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ReferenceSystem =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let origin =
                { new Lens<PRo3D.ReferenceSystem, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.origin
                    override x.Set(r,v) = { r with origin = v }
                    override x.Update(r,f) = { r with origin = f r.origin }
                }
            let north =
                { new Lens<PRo3D.ReferenceSystem, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.north
                    override x.Set(r,v) = { r with north = v }
                    override x.Update(r,f) = { r with north = f r.north }
                }
            let up =
                { new Lens<PRo3D.ReferenceSystem, Aardvark.UI.V3dInput>() with
                    override x.Get(r) = r.up
                    override x.Set(r,v) = { r with up = v }
                    override x.Update(r,f) = { r with up = f r.up }
                }
            let isVisible =
                { new Lens<PRo3D.ReferenceSystem, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
            let arrowScale =
                { new Lens<PRo3D.ReferenceSystem, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.arrowScale
                    override x.Set(r,v) = { r with arrowScale = v }
                    override x.Update(r,f) = { r with arrowScale = f r.arrowScale }
                }
    
    
    type MViewConfigModel(__initial : PRo3D.ViewConfigModel) =
        inherit obj()
        let mutable __current = __initial
        let _nearPlane = Aardvark.UI.Mutable.MNumericInput.Create(__initial.nearPlane)
        let _farPlane = Aardvark.UI.Mutable.MNumericInput.Create(__initial.farPlane)
        let _navigationSensitivity = Aardvark.UI.Mutable.MNumericInput.Create(__initial.navigationSensitivity)
        let _arrowLength = Aardvark.UI.Mutable.MNumericInput.Create(__initial.arrowLength)
        let _arrowThickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.arrowThickness)
        let _dnsPlaneSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.dnsPlaneSize)
        let _minDipAngle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.minDipAngle)
        let _maxDipAngle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.maxDipAngle)
        let _coordinates = MReferenceSystem.Create(__initial.coordinates)
        
        member x.nearPlane = _nearPlane
        member x.farPlane = _farPlane
        member x.navigationSensitivity = _navigationSensitivity
        member x.arrowLength = _arrowLength
        member x.arrowThickness = _arrowThickness
        member x.dnsPlaneSize = _dnsPlaneSize
        member x.minDipAngle = _minDipAngle
        member x.maxDipAngle = _maxDipAngle
        member x.coordinates = _coordinates
        
        member x.Update(v : PRo3D.ViewConfigModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                Aardvark.UI.Mutable.MNumericInput.Update(_nearPlane, v.nearPlane)
                Aardvark.UI.Mutable.MNumericInput.Update(_farPlane, v.farPlane)
                Aardvark.UI.Mutable.MNumericInput.Update(_navigationSensitivity, v.navigationSensitivity)
                Aardvark.UI.Mutable.MNumericInput.Update(_arrowLength, v.arrowLength)
                Aardvark.UI.Mutable.MNumericInput.Update(_arrowThickness, v.arrowThickness)
                Aardvark.UI.Mutable.MNumericInput.Update(_dnsPlaneSize, v.dnsPlaneSize)
                Aardvark.UI.Mutable.MNumericInput.Update(_minDipAngle, v.minDipAngle)
                Aardvark.UI.Mutable.MNumericInput.Update(_maxDipAngle, v.maxDipAngle)
                MReferenceSystem.Update(_coordinates, v.coordinates)
                
        
        static member Create(__initial : PRo3D.ViewConfigModel) : MViewConfigModel = MViewConfigModel(__initial)
        static member Update(m : MViewConfigModel, v : PRo3D.ViewConfigModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.ViewConfigModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ViewConfigModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let nearPlane =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.nearPlane
                    override x.Set(r,v) = { r with nearPlane = v }
                    override x.Update(r,f) = { r with nearPlane = f r.nearPlane }
                }
            let farPlane =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.farPlane
                    override x.Set(r,v) = { r with farPlane = v }
                    override x.Update(r,f) = { r with farPlane = f r.farPlane }
                }
            let navigationSensitivity =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.navigationSensitivity
                    override x.Set(r,v) = { r with navigationSensitivity = v }
                    override x.Update(r,f) = { r with navigationSensitivity = f r.navigationSensitivity }
                }
            let arrowLength =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.arrowLength
                    override x.Set(r,v) = { r with arrowLength = v }
                    override x.Update(r,f) = { r with arrowLength = f r.arrowLength }
                }
            let arrowThickness =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.arrowThickness
                    override x.Set(r,v) = { r with arrowThickness = v }
                    override x.Update(r,f) = { r with arrowThickness = f r.arrowThickness }
                }
            let dnsPlaneSize =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.dnsPlaneSize
                    override x.Set(r,v) = { r with dnsPlaneSize = v }
                    override x.Update(r,f) = { r with dnsPlaneSize = f r.dnsPlaneSize }
                }
            let minDipAngle =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.minDipAngle
                    override x.Set(r,v) = { r with minDipAngle = v }
                    override x.Update(r,f) = { r with minDipAngle = f r.minDipAngle }
                }
            let maxDipAngle =
                { new Lens<PRo3D.ViewConfigModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.maxDipAngle
                    override x.Set(r,v) = { r with maxDipAngle = v }
                    override x.Update(r,f) = { r with maxDipAngle = f r.maxDipAngle }
                }
            let coordinates =
                { new Lens<PRo3D.ViewConfigModel, PRo3D.ReferenceSystem>() with
                    override x.Get(r) = r.coordinates
                    override x.Set(r,v) = { r with coordinates = v }
                    override x.Update(r,f) = { r with coordinates = f r.coordinates }
                }
    
    
    type MNavigationModel(__initial : PRo3D.NavigationModel) =
        inherit obj()
        let mutable __current = __initial
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _navigationMode = ResetMod.Create(__initial.navigationMode)
        let _coordinates = MReferenceSystem.Create(__initial.coordinates)
        let _exploreCenter = ResetMod.Create(__initial.exploreCenter)
        let _config = MViewConfigModel.Create(__initial.config)
        
        member x.camera = _camera
        member x.navigationMode = _navigationMode :> IMod<_>
        member x.coordinates = _coordinates
        member x.exploreCenter = _exploreCenter :> IMod<_>
        member x.config = _config
        
        member x.Update(v : PRo3D.NavigationModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                ResetMod.Update(_navigationMode,v.navigationMode)
                MReferenceSystem.Update(_coordinates, v.coordinates)
                ResetMod.Update(_exploreCenter,v.exploreCenter)
                MViewConfigModel.Update(_config, v.config)
                
        
        static member Create(__initial : PRo3D.NavigationModel) : MNavigationModel = MNavigationModel(__initial)
        static member Update(m : MNavigationModel, v : PRo3D.NavigationModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.NavigationModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NavigationModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let camera =
                { new Lens<PRo3D.NavigationModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
            let navigationMode =
                { new Lens<PRo3D.NavigationModel, PRo3D.NavigationMode>() with
                    override x.Get(r) = r.navigationMode
                    override x.Set(r,v) = { r with navigationMode = v }
                    override x.Update(r,f) = { r with navigationMode = f r.navigationMode }
                }
            let coordinates =
                { new Lens<PRo3D.NavigationModel, PRo3D.ReferenceSystem>() with
                    override x.Get(r) = r.coordinates
                    override x.Set(r,v) = { r with coordinates = v }
                    override x.Update(r,f) = { r with coordinates = f r.coordinates }
                }
            let exploreCenter =
                { new Lens<PRo3D.NavigationModel, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.exploreCenter
                    override x.Set(r,v) = { r with exploreCenter = v }
                    override x.Update(r,f) = { r with exploreCenter = f r.exploreCenter }
                }
            let config =
                { new Lens<PRo3D.NavigationModel, PRo3D.ViewConfigModel>() with
                    override x.Get(r) = r.config
                    override x.Set(r,v) = { r with config = v }
                    override x.Update(r,f) = { r with config = f r.config }
                }
