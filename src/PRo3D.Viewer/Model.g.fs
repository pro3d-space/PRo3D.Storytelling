namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    
    
    type MDipAndStrikeResults(__initial : PRo3D.DipAndStrikeResults) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.DipAndStrikeResults> = Aardvark.Base.Incremental.EqModRef<PRo3D.DipAndStrikeResults>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.DipAndStrikeResults>
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
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.DipAndStrikeResults) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_plane,v.plane)
                ResetMod.Update(_dipAngle,v.dipAngle)
                ResetMod.Update(_dipDirection,v.dipDirection)
                ResetMod.Update(_strikeDirection,v.strikeDirection)
                ResetMod.Update(_dipAzimuth,v.dipAzimuth)
                ResetMod.Update(_strikeAzimuth,v.strikeAzimuth)
                ResetMod.Update(_centerOfMass,v.centerOfMass)
                
        
        static member Create(__initial : PRo3D.DipAndStrikeResults) : MDipAndStrikeResults = MDipAndStrikeResults(__initial)
        static member Update(m : MDipAndStrikeResults, v : PRo3D.DipAndStrikeResults) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationResults> = Aardvark.Base.Incremental.EqModRef<PRo3D.AnnotationResults>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationResults>
        let _height = ResetMod.Create(__initial.height)
        let _heightDelta = ResetMod.Create(__initial.heightDelta)
        let _length = ResetMod.Create(__initial.length)
        let _wayLength = ResetMod.Create(__initial.wayLength)
        
        member x.height = _height :> IMod<_>
        member x.heightDelta = _heightDelta :> IMod<_>
        member x.length = _length :> IMod<_>
        member x.wayLength = _wayLength :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.AnnotationResults) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_height,v.height)
                ResetMod.Update(_heightDelta,v.heightDelta)
                ResetMod.Update(_length,v.length)
                ResetMod.Update(_wayLength,v.wayLength)
                
        
        static member Create(__initial : PRo3D.AnnotationResults) : MAnnotationResults = MAnnotationResults(__initial)
        static member Update(m : MAnnotationResults, v : PRo3D.AnnotationResults) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Segment> = Aardvark.Base.Incremental.EqModRef<PRo3D.Segment>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Segment>
        let _startPoint = ResetMod.Create(__initial.startPoint)
        let _endPoint = ResetMod.Create(__initial.endPoint)
        let _points = MList.Create(__initial.points)
        
        member x.startPoint = _startPoint :> IMod<_>
        member x.endPoint = _endPoint :> IMod<_>
        member x.points = _points :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Segment) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_startPoint,v.startPoint)
                ResetMod.Update(_endPoint,v.endPoint)
                MList.Update(_points, v.points)
                
        
        static member Create(__initial : PRo3D.Segment) : MSegment = MSegment(__initial)
        static member Update(m : MSegment, v : PRo3D.Segment) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Annotation> = Aardvark.Base.Incremental.EqModRef<PRo3D.Annotation>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Annotation>
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
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Annotation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
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
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
    
    
    type MPathProxy(__initial : PRo3D.PathProxy) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.PathProxy> = Aardvark.Base.Incremental.EqModRef<PRo3D.PathProxy>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.PathProxy>
        let _absolutePath = MOption.Create(__initial.absolutePath)
        let _relativePath = MOption.Create(__initial.relativePath)
        
        member x.absolutePath = _absolutePath :> IMod<_>
        member x.relativePath = _relativePath :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.PathProxy) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MOption.Update(_absolutePath, v.absolutePath)
                MOption.Update(_relativePath, v.relativePath)
                
        
        static member Create(__initial : PRo3D.PathProxy) : MPathProxy = MPathProxy(__initial)
        static member Update(m : MPathProxy, v : PRo3D.PathProxy) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.PathProxy> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PathProxy =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let absolutePath =
                { new Lens<PRo3D.PathProxy, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.absolutePath
                    override x.Set(r,v) = { r with absolutePath = v }
                    override x.Update(r,f) = { r with absolutePath = f r.absolutePath }
                }
            let relativePath =
                { new Lens<PRo3D.PathProxy, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.relativePath
                    override x.Set(r,v) = { r with relativePath = v }
                    override x.Update(r,f) = { r with relativePath = f r.relativePath }
                }
    
    
    type MMeasurementsImporterModel(__initial : PRo3D.MeasurementsImporterModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.MeasurementsImporterModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.MeasurementsImporterModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.MeasurementsImporterModel>
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.annotations = _annotations :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.MeasurementsImporterModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_annotations, v.annotations)
                
        
        static member Create(__initial : PRo3D.MeasurementsImporterModel) : MMeasurementsImporterModel = MMeasurementsImporterModel(__initial)
        static member Update(m : MMeasurementsImporterModel, v : PRo3D.MeasurementsImporterModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
    
    
    type MViewConfigModel(__initial : PRo3D.ViewConfigModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.ViewConfigModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.ViewConfigModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.ViewConfigModel>
        let _nearPlane = Aardvark.UI.Mutable.MNumericInput.Create(__initial.nearPlane)
        let _farPlane = Aardvark.UI.Mutable.MNumericInput.Create(__initial.farPlane)
        let _navigationSensitivity = Aardvark.UI.Mutable.MNumericInput.Create(__initial.navigationSensitivity)
        let _arrowLength = Aardvark.UI.Mutable.MNumericInput.Create(__initial.arrowLength)
        let _arrowThickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.arrowThickness)
        let _dnsPlaneSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.dnsPlaneSize)
        let _minDipAngle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.minDipAngle)
        let _maxDipAngle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.maxDipAngle)
        let _lodColoring = ResetMod.Create(__initial.lodColoring)
        
        member x.nearPlane = _nearPlane
        member x.farPlane = _farPlane
        member x.navigationSensitivity = _navigationSensitivity
        member x.arrowLength = _arrowLength
        member x.arrowThickness = _arrowThickness
        member x.dnsPlaneSize = _dnsPlaneSize
        member x.minDipAngle = _minDipAngle
        member x.maxDipAngle = _maxDipAngle
        member x.lodColoring = _lodColoring :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.ViewConfigModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MNumericInput.Update(_nearPlane, v.nearPlane)
                Aardvark.UI.Mutable.MNumericInput.Update(_farPlane, v.farPlane)
                Aardvark.UI.Mutable.MNumericInput.Update(_navigationSensitivity, v.navigationSensitivity)
                Aardvark.UI.Mutable.MNumericInput.Update(_arrowLength, v.arrowLength)
                Aardvark.UI.Mutable.MNumericInput.Update(_arrowThickness, v.arrowThickness)
                Aardvark.UI.Mutable.MNumericInput.Update(_dnsPlaneSize, v.dnsPlaneSize)
                Aardvark.UI.Mutable.MNumericInput.Update(_minDipAngle, v.minDipAngle)
                Aardvark.UI.Mutable.MNumericInput.Update(_maxDipAngle, v.maxDipAngle)
                ResetMod.Update(_lodColoring,v.lodColoring)
                
        
        static member Create(__initial : PRo3D.ViewConfigModel) : MViewConfigModel = MViewConfigModel(__initial)
        static member Update(m : MViewConfigModel, v : PRo3D.ViewConfigModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
            let lodColoring =
                { new Lens<PRo3D.ViewConfigModel, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.lodColoring
                    override x.Set(r,v) = { r with lodColoring = v }
                    override x.Update(r,f) = { r with lodColoring = f r.lodColoring }
                }
    
    
    type MAnnotationGroup(__initial : PRo3D.AnnotationGroup) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationGroup> = Aardvark.Base.Incremental.EqModRef<PRo3D.AnnotationGroup>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationGroup>
        let _key = ResetMod.Create(__initial.key)
        let _name = ResetMod.Create(__initial.name)
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _subGroups = MList.Create(__initial.subGroups, (fun v -> MAnnotationGroup.Create(v)), (fun (m,v) -> MAnnotationGroup.Update(m, v)), (fun v -> v))
        let _visible = ResetMod.Create(__initial.visible)
        let _expanded = ResetMod.Create(__initial.expanded)
        
        member x.key = _key :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.annotations = _annotations :> alist<_>
        member x.subGroups = _subGroups :> alist<_>
        member x.visible = _visible :> IMod<_>
        member x.expanded = _expanded :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.AnnotationGroup) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_key,v.key)
                ResetMod.Update(_name,v.name)
                MList.Update(_annotations, v.annotations)
                MList.Update(_subGroups, v.subGroups)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_expanded,v.expanded)
                
        
        static member Create(__initial : PRo3D.AnnotationGroup) : MAnnotationGroup = MAnnotationGroup(__initial)
        static member Update(m : MAnnotationGroup, v : PRo3D.AnnotationGroup) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.AnnotationGroup> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationGroup =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let key =
                { new Lens<PRo3D.AnnotationGroup, System.Guid>() with
                    override x.Get(r) = r.key
                    override x.Set(r,v) = { r with key = v }
                    override x.Update(r,f) = { r with key = f r.key }
                }
            let name =
                { new Lens<PRo3D.AnnotationGroup, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let annotations =
                { new Lens<PRo3D.AnnotationGroup, Aardvark.Base.plist<PRo3D.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let subGroups =
                { new Lens<PRo3D.AnnotationGroup, Aardvark.Base.plist<PRo3D.AnnotationGroup>>() with
                    override x.Get(r) = r.subGroups
                    override x.Set(r,v) = { r with subGroups = v }
                    override x.Update(r,f) = { r with subGroups = f r.subGroups }
                }
            let visible =
                { new Lens<PRo3D.AnnotationGroup, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let expanded =
                { new Lens<PRo3D.AnnotationGroup, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.expanded
                    override x.Set(r,v) = { r with expanded = v }
                    override x.Update(r,f) = { r with expanded = f r.expanded }
                }
    
    
    type MAnnotationGroupsModel(__initial : PRo3D.AnnotationGroupsModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationGroupsModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.AnnotationGroupsModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationGroupsModel>
        let _rootGroup = MAnnotationGroup.Create(__initial.rootGroup)
        let _activeGroupKey = ResetMod.Create(__initial.activeGroupKey)
        let _activeGroupPath = ResetMod.Create(__initial.activeGroupPath)
        let _activeGroupName = ResetMod.Create(__initial.activeGroupName)
        let _activeChildKey = ResetMod.Create(__initial.activeChildKey)
        let _activeChildPath = ResetMod.Create(__initial.activeChildPath)
        let _flattenChild = MList.Create(__initial.flattenChild, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.rootGroup = _rootGroup
        member x.activeGroupKey = _activeGroupKey :> IMod<_>
        member x.activeGroupPath = _activeGroupPath :> IMod<_>
        member x.activeGroupName = _activeGroupName :> IMod<_>
        member x.activeChildKey = _activeChildKey :> IMod<_>
        member x.activeChildPath = _activeChildPath :> IMod<_>
        member x.flattenChild = _flattenChild :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.AnnotationGroupsModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MAnnotationGroup.Update(_rootGroup, v.rootGroup)
                ResetMod.Update(_activeGroupKey,v.activeGroupKey)
                ResetMod.Update(_activeGroupPath,v.activeGroupPath)
                ResetMod.Update(_activeGroupName,v.activeGroupName)
                ResetMod.Update(_activeChildKey,v.activeChildKey)
                ResetMod.Update(_activeChildPath,v.activeChildPath)
                MList.Update(_flattenChild, v.flattenChild)
                
        
        static member Create(__initial : PRo3D.AnnotationGroupsModel) : MAnnotationGroupsModel = MAnnotationGroupsModel(__initial)
        static member Update(m : MAnnotationGroupsModel, v : PRo3D.AnnotationGroupsModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.AnnotationGroupsModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationGroupsModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rootGroup =
                { new Lens<PRo3D.AnnotationGroupsModel, PRo3D.AnnotationGroup>() with
                    override x.Get(r) = r.rootGroup
                    override x.Set(r,v) = { r with rootGroup = v }
                    override x.Update(r,f) = { r with rootGroup = f r.rootGroup }
                }
            let activeGroupKey =
                { new Lens<PRo3D.AnnotationGroupsModel, System.Guid>() with
                    override x.Get(r) = r.activeGroupKey
                    override x.Set(r,v) = { r with activeGroupKey = v }
                    override x.Update(r,f) = { r with activeGroupKey = f r.activeGroupKey }
                }
            let activeGroupPath =
                { new Lens<PRo3D.AnnotationGroupsModel, Microsoft.FSharp.Collections.list<Aardvark.Base.Index>>() with
                    override x.Get(r) = r.activeGroupPath
                    override x.Set(r,v) = { r with activeGroupPath = v }
                    override x.Update(r,f) = { r with activeGroupPath = f r.activeGroupPath }
                }
            let activeGroupName =
                { new Lens<PRo3D.AnnotationGroupsModel, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.activeGroupName
                    override x.Set(r,v) = { r with activeGroupName = v }
                    override x.Update(r,f) = { r with activeGroupName = f r.activeGroupName }
                }
            let activeChildKey =
                { new Lens<PRo3D.AnnotationGroupsModel, System.Guid>() with
                    override x.Get(r) = r.activeChildKey
                    override x.Set(r,v) = { r with activeChildKey = v }
                    override x.Update(r,f) = { r with activeChildKey = f r.activeChildKey }
                }
            let activeChildPath =
                { new Lens<PRo3D.AnnotationGroupsModel, Microsoft.FSharp.Collections.list<Aardvark.Base.Index>>() with
                    override x.Get(r) = r.activeChildPath
                    override x.Set(r,v) = { r with activeChildPath = v }
                    override x.Update(r,f) = { r with activeChildPath = f r.activeChildPath }
                }
            let flattenChild =
                { new Lens<PRo3D.AnnotationGroupsModel, Aardvark.Base.plist<PRo3D.Annotation>>() with
                    override x.Get(r) = r.flattenChild
                    override x.Set(r,v) = { r with flattenChild = v }
                    override x.Update(r,f) = { r with flattenChild = f r.flattenChild }
                }
