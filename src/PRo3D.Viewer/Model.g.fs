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
        let _color = ResetMod.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        let _dnsResults = MOption.Create(__initial.dnsResults, (fun v -> MDipAndStrikeResults.Create(v)), (fun (m,v) -> MDipAndStrikeResults.Update(m, v)), (fun v -> v))
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
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_text,v.text)
                
        
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
