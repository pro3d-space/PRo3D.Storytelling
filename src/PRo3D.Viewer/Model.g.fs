namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    
    
    type MStatistics(__initial : PRo3D.Statistics) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Statistics> = Aardvark.Base.Incremental.EqModRef<PRo3D.Statistics>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Statistics>
        let _average = ResetMod.Create(__initial.average)
        let _min = ResetMod.Create(__initial.min)
        let _max = ResetMod.Create(__initial.max)
        let _stdev = ResetMod.Create(__initial.stdev)
        let _sumOfSquares = ResetMod.Create(__initial.sumOfSquares)
        
        member x.average = _average :> IMod<_>
        member x.min = _min :> IMod<_>
        member x.max = _max :> IMod<_>
        member x.stdev = _stdev :> IMod<_>
        member x.sumOfSquares = _sumOfSquares :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Statistics) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_average,v.average)
                ResetMod.Update(_min,v.min)
                ResetMod.Update(_max,v.max)
                ResetMod.Update(_stdev,v.stdev)
                ResetMod.Update(_sumOfSquares,v.sumOfSquares)
                
        
        static member Create(__initial : PRo3D.Statistics) : MStatistics = MStatistics(__initial)
        static member Update(m : MStatistics, v : PRo3D.Statistics) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Statistics> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Statistics =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let average =
                { new Lens<PRo3D.Statistics, System.Double>() with
                    override x.Get(r) = r.average
                    override x.Set(r,v) = { r with average = v }
                    override x.Update(r,f) = { r with average = f r.average }
                }
            let min =
                { new Lens<PRo3D.Statistics, System.Double>() with
                    override x.Get(r) = r.min
                    override x.Set(r,v) = { r with min = v }
                    override x.Update(r,f) = { r with min = f r.min }
                }
            let max =
                { new Lens<PRo3D.Statistics, System.Double>() with
                    override x.Get(r) = r.max
                    override x.Set(r,v) = { r with max = v }
                    override x.Update(r,f) = { r with max = f r.max }
                }
            let stdev =
                { new Lens<PRo3D.Statistics, System.Double>() with
                    override x.Get(r) = r.stdev
                    override x.Set(r,v) = { r with stdev = v }
                    override x.Update(r,f) = { r with stdev = f r.stdev }
                }
            let sumOfSquares =
                { new Lens<PRo3D.Statistics, System.Double>() with
                    override x.Get(r) = r.sumOfSquares
                    override x.Set(r,v) = { r with sumOfSquares = v }
                    override x.Update(r,f) = { r with sumOfSquares = f r.sumOfSquares }
                }
    
    
    type MFalseColorsModel(__initial : PRo3D.FalseColorsModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.FalseColorsModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.FalseColorsModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.FalseColorsModel>
        let _useFalseColors = ResetMod.Create(__initial.useFalseColors)
        let _lowerBound = Aardvark.UI.Mutable.MNumericInput.Create(__initial.lowerBound)
        let _upperBound = Aardvark.UI.Mutable.MNumericInput.Create(__initial.upperBound)
        let _interval = Aardvark.UI.Mutable.MNumericInput.Create(__initial.interval)
        let _invertMapping = ResetMod.Create(__initial.invertMapping)
        let _lowerColor = Aardvark.UI.Mutable.MColorInput.Create(__initial.lowerColor)
        let _upperColor = Aardvark.UI.Mutable.MColorInput.Create(__initial.upperColor)
        
        member x.useFalseColors = _useFalseColors :> IMod<_>
        member x.lowerBound = _lowerBound
        member x.upperBound = _upperBound
        member x.interval = _interval
        member x.invertMapping = _invertMapping :> IMod<_>
        member x.lowerColor = _lowerColor
        member x.upperColor = _upperColor
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.FalseColorsModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_useFalseColors,v.useFalseColors)
                Aardvark.UI.Mutable.MNumericInput.Update(_lowerBound, v.lowerBound)
                Aardvark.UI.Mutable.MNumericInput.Update(_upperBound, v.upperBound)
                Aardvark.UI.Mutable.MNumericInput.Update(_interval, v.interval)
                ResetMod.Update(_invertMapping,v.invertMapping)
                Aardvark.UI.Mutable.MColorInput.Update(_lowerColor, v.lowerColor)
                Aardvark.UI.Mutable.MColorInput.Update(_upperColor, v.upperColor)
                
        
        static member Create(__initial : PRo3D.FalseColorsModel) : MFalseColorsModel = MFalseColorsModel(__initial)
        static member Update(m : MFalseColorsModel, v : PRo3D.FalseColorsModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.FalseColorsModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FalseColorsModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let useFalseColors =
                { new Lens<PRo3D.FalseColorsModel, System.Boolean>() with
                    override x.Get(r) = r.useFalseColors
                    override x.Set(r,v) = { r with useFalseColors = v }
                    override x.Update(r,f) = { r with useFalseColors = f r.useFalseColors }
                }
            let lowerBound =
                { new Lens<PRo3D.FalseColorsModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.lowerBound
                    override x.Set(r,v) = { r with lowerBound = v }
                    override x.Update(r,f) = { r with lowerBound = f r.lowerBound }
                }
            let upperBound =
                { new Lens<PRo3D.FalseColorsModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.upperBound
                    override x.Set(r,v) = { r with upperBound = v }
                    override x.Update(r,f) = { r with upperBound = f r.upperBound }
                }
            let interval =
                { new Lens<PRo3D.FalseColorsModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.interval
                    override x.Set(r,v) = { r with interval = v }
                    override x.Update(r,f) = { r with interval = f r.interval }
                }
            let invertMapping =
                { new Lens<PRo3D.FalseColorsModel, System.Boolean>() with
                    override x.Get(r) = r.invertMapping
                    override x.Set(r,v) = { r with invertMapping = v }
                    override x.Update(r,f) = { r with invertMapping = f r.invertMapping }
                }
            let lowerColor =
                { new Lens<PRo3D.FalseColorsModel, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.lowerColor
                    override x.Set(r,v) = { r with lowerColor = v }
                    override x.Update(r,f) = { r with lowerColor = f r.lowerColor }
                }
            let upperColor =
                { new Lens<PRo3D.FalseColorsModel, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.upperColor
                    override x.Set(r,v) = { r with upperColor = v }
                    override x.Update(r,f) = { r with upperColor = f r.upperColor }
                }
    
    
    type MOrientationCubeModel(__initial : PRo3D.OrientationCubeModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.OrientationCubeModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.OrientationCubeModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.OrientationCubeModel>
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        
        member x.camera = _camera
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.OrientationCubeModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                
        
        static member Create(__initial : PRo3D.OrientationCubeModel) : MOrientationCubeModel = MOrientationCubeModel(__initial)
        static member Update(m : MOrientationCubeModel, v : PRo3D.OrientationCubeModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.OrientationCubeModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module OrientationCubeModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let camera =
                { new Lens<PRo3D.OrientationCubeModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
    
    
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
        let _error = MStatistics.Create(__initial.error)
        
        member x.plane = _plane :> IMod<_>
        member x.dipAngle = _dipAngle :> IMod<_>
        member x.dipDirection = _dipDirection :> IMod<_>
        member x.strikeDirection = _strikeDirection :> IMod<_>
        member x.dipAzimuth = _dipAzimuth :> IMod<_>
        member x.strikeAzimuth = _strikeAzimuth :> IMod<_>
        member x.centerOfMass = _centerOfMass :> IMod<_>
        member x.error = _error
        
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
                MStatistics.Update(_error, v.error)
                
        
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
                { new Lens<PRo3D.DipAndStrikeResults, System.Double>() with
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
                { new Lens<PRo3D.DipAndStrikeResults, System.Double>() with
                    override x.Get(r) = r.dipAzimuth
                    override x.Set(r,v) = { r with dipAzimuth = v }
                    override x.Update(r,f) = { r with dipAzimuth = f r.dipAzimuth }
                }
            let strikeAzimuth =
                { new Lens<PRo3D.DipAndStrikeResults, System.Double>() with
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
            let error =
                { new Lens<PRo3D.DipAndStrikeResults, PRo3D.Statistics>() with
                    override x.Get(r) = r.error
                    override x.Set(r,v) = { r with error = v }
                    override x.Update(r,f) = { r with error = f r.error }
                }
    
    
    type MAnnotationResults(__initial : PRo3D.AnnotationResults) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationResults> = Aardvark.Base.Incremental.EqModRef<PRo3D.AnnotationResults>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.AnnotationResults>
        let _height = ResetMod.Create(__initial.height)
        let _heightDelta = ResetMod.Create(__initial.heightDelta)
        let _avgAltitude = ResetMod.Create(__initial.avgAltitude)
        let _length = ResetMod.Create(__initial.length)
        let _wayLength = ResetMod.Create(__initial.wayLength)
        let _bearing = ResetMod.Create(__initial.bearing)
        let _slope = ResetMod.Create(__initial.slope)
        
        member x.height = _height :> IMod<_>
        member x.heightDelta = _heightDelta :> IMod<_>
        member x.avgAltitude = _avgAltitude :> IMod<_>
        member x.length = _length :> IMod<_>
        member x.wayLength = _wayLength :> IMod<_>
        member x.bearing = _bearing :> IMod<_>
        member x.slope = _slope :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.AnnotationResults) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_height,v.height)
                ResetMod.Update(_heightDelta,v.heightDelta)
                ResetMod.Update(_avgAltitude,v.avgAltitude)
                ResetMod.Update(_length,v.length)
                ResetMod.Update(_wayLength,v.wayLength)
                ResetMod.Update(_bearing,v.bearing)
                ResetMod.Update(_slope,v.slope)
                
        
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
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.height
                    override x.Set(r,v) = { r with height = v }
                    override x.Update(r,f) = { r with height = f r.height }
                }
            let heightDelta =
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.heightDelta
                    override x.Set(r,v) = { r with heightDelta = v }
                    override x.Update(r,f) = { r with heightDelta = f r.heightDelta }
                }
            let avgAltitude =
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.avgAltitude
                    override x.Set(r,v) = { r with avgAltitude = v }
                    override x.Update(r,f) = { r with avgAltitude = f r.avgAltitude }
                }
            let length =
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.length
                    override x.Set(r,v) = { r with length = v }
                    override x.Update(r,f) = { r with length = f r.length }
                }
            let wayLength =
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.wayLength
                    override x.Set(r,v) = { r with wayLength = v }
                    override x.Update(r,f) = { r with wayLength = f r.wayLength }
                }
            let bearing =
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.bearing
                    override x.Set(r,v) = { r with bearing = v }
                    override x.Update(r,f) = { r with bearing = f r.bearing }
                }
            let slope =
                { new Lens<PRo3D.AnnotationResults, System.Double>() with
                    override x.Get(r) = r.slope
                    override x.Set(r,v) = { r with slope = v }
                    override x.Update(r,f) = { r with slope = f r.slope }
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
        let _textsize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.textsize)
        let _surfaceName = ResetMod.Create(__initial.surfaceName)
        
        member x.key = __current.Value.key
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
        member x.textsize = _textsize
        member x.surfaceName = _surfaceName :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Annotation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
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
                Aardvark.UI.Mutable.MNumericInput.Update(_textsize, v.textsize)
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
                { new Lens<PRo3D.Annotation, System.Boolean>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let showDns =
                { new Lens<PRo3D.Annotation, System.Boolean>() with
                    override x.Get(r) = r.showDns
                    override x.Set(r,v) = { r with showDns = v }
                    override x.Update(r,f) = { r with showDns = f r.showDns }
                }
            let text =
                { new Lens<PRo3D.Annotation, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let textsize =
                { new Lens<PRo3D.Annotation, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.textsize
                    override x.Set(r,v) = { r with textsize = v }
                    override x.Update(r,f) = { r with textsize = f r.textsize }
                }
            let surfaceName =
                { new Lens<PRo3D.Annotation, System.String>() with
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
                { new Lens<PRo3D.PathProxy, Microsoft.FSharp.Core.Option<System.String>>() with
                    override x.Get(r) = r.absolutePath
                    override x.Set(r,v) = { r with absolutePath = v }
                    override x.Update(r,f) = { r with absolutePath = f r.absolutePath }
                }
            let relativePath =
                { new Lens<PRo3D.PathProxy, Microsoft.FSharp.Core.Option<System.String>>() with
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
    
    
    type MSurfaceTrafoImporterModel(__initial : PRo3D.SurfaceTrafoImporterModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.SurfaceTrafoImporterModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.SurfaceTrafoImporterModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.SurfaceTrafoImporterModel>
        let _trafos = MList.Create(__initial.trafos)
        
        member x.trafos = _trafos :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.SurfaceTrafoImporterModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_trafos, v.trafos)
                
        
        static member Create(__initial : PRo3D.SurfaceTrafoImporterModel) : MSurfaceTrafoImporterModel = MSurfaceTrafoImporterModel(__initial)
        static member Update(m : MSurfaceTrafoImporterModel, v : PRo3D.SurfaceTrafoImporterModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.SurfaceTrafoImporterModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SurfaceTrafoImporterModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let trafos =
                { new Lens<PRo3D.SurfaceTrafoImporterModel, Aardvark.Base.plist<PRo3D.SurfaceTrafo>>() with
                    override x.Get(r) = r.trafos
                    override x.Set(r,v) = { r with trafos = v }
                    override x.Update(r,f) = { r with trafos = f r.trafos }
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
        let _lodColoring = ResetMod.Create(__initial.lodColoring)
        let _drawOrientationCube = ResetMod.Create(__initial.drawOrientationCube)
        
        member x.nearPlane = _nearPlane
        member x.farPlane = _farPlane
        member x.navigationSensitivity = _navigationSensitivity
        member x.arrowLength = _arrowLength
        member x.arrowThickness = _arrowThickness
        member x.dnsPlaneSize = _dnsPlaneSize
        member x.lodColoring = _lodColoring :> IMod<_>
        member x.drawOrientationCube = _drawOrientationCube :> IMod<_>
        
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
                ResetMod.Update(_lodColoring,v.lodColoring)
                ResetMod.Update(_drawOrientationCube,v.drawOrientationCube)
                
        
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
            let lodColoring =
                { new Lens<PRo3D.ViewConfigModel, System.Boolean>() with
                    override x.Get(r) = r.lodColoring
                    override x.Set(r,v) = { r with lodColoring = v }
                    override x.Update(r,f) = { r with lodColoring = f r.lodColoring }
                }
            let drawOrientationCube =
                { new Lens<PRo3D.ViewConfigModel, System.Boolean>() with
                    override x.Get(r) = r.drawOrientationCube
                    override x.Set(r,v) = { r with drawOrientationCube = v }
                    override x.Update(r,f) = { r with drawOrientationCube = f r.drawOrientationCube }
                }
