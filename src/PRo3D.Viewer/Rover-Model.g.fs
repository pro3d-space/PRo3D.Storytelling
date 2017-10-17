namespace PRo3d

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3d

[<AutoOpen>]
module Mutable =

    
    
    type MExtrinsics(__initial : PRo3d.Extrinsics) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.Extrinsics> = Aardvark.Base.Incremental.EqModRef<PRo3d.Extrinsics>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.Extrinsics>
        let _position = ResetMod.Create(__initial.position)
        let _camUp = ResetMod.Create(__initial.camUp)
        let _camLookAt = ResetMod.Create(__initial.camLookAt)
        let _box = ResetMod.Create(__initial.box)
        
        member x.position = _position :> IMod<_>
        member x.camUp = _camUp :> IMod<_>
        member x.camLookAt = _camLookAt :> IMod<_>
        member x.box = _box :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Extrinsics) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_position,v.position)
                ResetMod.Update(_camUp,v.camUp)
                ResetMod.Update(_camLookAt,v.camLookAt)
                ResetMod.Update(_box,v.box)
                
        
        static member Create(__initial : PRo3d.Extrinsics) : MExtrinsics = MExtrinsics(__initial)
        static member Update(m : MExtrinsics, v : PRo3d.Extrinsics) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.Extrinsics> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Extrinsics =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let position =
                { new Lens<PRo3d.Extrinsics, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.position
                    override x.Set(r,v) = { r with position = v }
                    override x.Update(r,f) = { r with position = f r.position }
                }
            let camUp =
                { new Lens<PRo3d.Extrinsics, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.camUp
                    override x.Set(r,v) = { r with camUp = v }
                    override x.Update(r,f) = { r with camUp = f r.camUp }
                }
            let camLookAt =
                { new Lens<PRo3d.Extrinsics, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.camLookAt
                    override x.Set(r,v) = { r with camLookAt = v }
                    override x.Update(r,f) = { r with camLookAt = f r.camLookAt }
                }
            let box =
                { new Lens<PRo3d.Extrinsics, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.box
                    override x.Set(r,v) = { r with box = v }
                    override x.Update(r,f) = { r with box = f r.box }
                }
    
    
    type MInstrument(__initial : PRo3d.Instrument) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.Instrument> = Aardvark.Base.Incremental.EqModRef<PRo3d.Instrument>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.Instrument>
        let _id = ResetMod.Create(__initial.id)
        let _iType = ResetMod.Create(__initial.iType)
        let _calibratedFocalLengths = ResetMod.Create(__initial.calibratedFocalLengths)
        let _currentFocalLength = ResetMod.Create(__initial.currentFocalLength)
        let _intrinsics = ResetMod.Create(__initial.intrinsics)
        let _extrinsics = MExtrinsics.Create(__initial.extrinsics)
        let _index = ResetMod.Create(__initial.index)
        
        member x.id = _id :> IMod<_>
        member x.iType = _iType :> IMod<_>
        member x.calibratedFocalLengths = _calibratedFocalLengths :> IMod<_>
        member x.currentFocalLength = _currentFocalLength :> IMod<_>
        member x.intrinsics = _intrinsics :> IMod<_>
        member x.extrinsics = _extrinsics
        member x.index = _index :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Instrument) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_iType,v.iType)
                ResetMod.Update(_calibratedFocalLengths,v.calibratedFocalLengths)
                ResetMod.Update(_currentFocalLength,v.currentFocalLength)
                ResetMod.Update(_intrinsics,v.intrinsics)
                MExtrinsics.Update(_extrinsics, v.extrinsics)
                ResetMod.Update(_index,v.index)
                
        
        static member Create(__initial : PRo3d.Instrument) : MInstrument = MInstrument(__initial)
        static member Update(m : MInstrument, v : PRo3d.Instrument) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.Instrument> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Instrument =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<PRo3d.Instrument, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let iType =
                { new Lens<PRo3d.Instrument, PRo3d.InstrumentType>() with
                    override x.Get(r) = r.iType
                    override x.Set(r,v) = { r with iType = v }
                    override x.Update(r,f) = { r with iType = f r.iType }
                }
            let calibratedFocalLengths =
                { new Lens<PRo3d.Instrument, Microsoft.FSharp.Collections.list<Microsoft.FSharp.Core.double>>() with
                    override x.Get(r) = r.calibratedFocalLengths
                    override x.Set(r,v) = { r with calibratedFocalLengths = v }
                    override x.Update(r,f) = { r with calibratedFocalLengths = f r.calibratedFocalLengths }
                }
            let currentFocalLength =
                { new Lens<PRo3d.Instrument, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.currentFocalLength
                    override x.Set(r,v) = { r with currentFocalLength = v }
                    override x.Update(r,f) = { r with currentFocalLength = f r.currentFocalLength }
                }
            let intrinsics =
                { new Lens<PRo3d.Instrument, PRo3d.Intrinsics>() with
                    override x.Get(r) = r.intrinsics
                    override x.Set(r,v) = { r with intrinsics = v }
                    override x.Update(r,f) = { r with intrinsics = f r.intrinsics }
                }
            let extrinsics =
                { new Lens<PRo3d.Instrument, PRo3d.Extrinsics>() with
                    override x.Get(r) = r.extrinsics
                    override x.Set(r,v) = { r with extrinsics = v }
                    override x.Update(r,f) = { r with extrinsics = f r.extrinsics }
                }
            let index =
                { new Lens<PRo3d.Instrument, Microsoft.FSharp.Core.int>() with
                    override x.Get(r) = r.index
                    override x.Set(r,v) = { r with index = v }
                    override x.Update(r,f) = { r with index = f r.index }
                }
    
    
    type MAxis(__initial : PRo3d.Axis) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.Axis> = Aardvark.Base.Incremental.EqModRef<PRo3d.Axis>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.Axis>
        let _id = ResetMod.Create(__initial.id)
        let _description = ResetMod.Create(__initial.description)
        let _startPoint = ResetMod.Create(__initial.startPoint)
        let _endPoint = ResetMod.Create(__initial.endPoint)
        let _minAngle = ResetMod.Create(__initial.minAngle)
        let _maxAngle = ResetMod.Create(__initial.maxAngle)
        let _currentAngle = ResetMod.Create(__initial.currentAngle)
        let _index = ResetMod.Create(__initial.index)
        
        member x.id = _id :> IMod<_>
        member x.description = _description :> IMod<_>
        member x.startPoint = _startPoint :> IMod<_>
        member x.endPoint = _endPoint :> IMod<_>
        member x.minAngle = _minAngle :> IMod<_>
        member x.maxAngle = _maxAngle :> IMod<_>
        member x.currentAngle = _currentAngle :> IMod<_>
        member x.index = _index :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Axis) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_description,v.description)
                ResetMod.Update(_startPoint,v.startPoint)
                ResetMod.Update(_endPoint,v.endPoint)
                ResetMod.Update(_minAngle,v.minAngle)
                ResetMod.Update(_maxAngle,v.maxAngle)
                ResetMod.Update(_currentAngle,v.currentAngle)
                ResetMod.Update(_index,v.index)
                
        
        static member Create(__initial : PRo3d.Axis) : MAxis = MAxis(__initial)
        static member Update(m : MAxis, v : PRo3d.Axis) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.Axis> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Axis =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<PRo3d.Axis, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let description =
                { new Lens<PRo3d.Axis, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.description
                    override x.Set(r,v) = { r with description = v }
                    override x.Update(r,f) = { r with description = f r.description }
                }
            let startPoint =
                { new Lens<PRo3d.Axis, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.startPoint
                    override x.Set(r,v) = { r with startPoint = v }
                    override x.Update(r,f) = { r with startPoint = f r.startPoint }
                }
            let endPoint =
                { new Lens<PRo3d.Axis, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.endPoint
                    override x.Set(r,v) = { r with endPoint = v }
                    override x.Update(r,f) = { r with endPoint = f r.endPoint }
                }
            let minAngle =
                { new Lens<PRo3d.Axis, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.minAngle
                    override x.Set(r,v) = { r with minAngle = v }
                    override x.Update(r,f) = { r with minAngle = f r.minAngle }
                }
            let maxAngle =
                { new Lens<PRo3d.Axis, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.maxAngle
                    override x.Set(r,v) = { r with maxAngle = v }
                    override x.Update(r,f) = { r with maxAngle = f r.maxAngle }
                }
            let currentAngle =
                { new Lens<PRo3d.Axis, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.currentAngle
                    override x.Set(r,v) = { r with currentAngle = v }
                    override x.Update(r,f) = { r with currentAngle = f r.currentAngle }
                }
            let index =
                { new Lens<PRo3d.Axis, Microsoft.FSharp.Core.int>() with
                    override x.Get(r) = r.index
                    override x.Set(r,v) = { r with index = v }
                    override x.Update(r,f) = { r with index = f r.index }
                }
    
    
    type MRover(__initial : PRo3d.Rover) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.Rover> = Aardvark.Base.Incremental.EqModRef<PRo3d.Rover>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.Rover>
        let _id = ResetMod.Create(__initial.id)
        let _platform2Ground = ResetMod.Create(__initial.platform2Ground)
        let _wheelPositions = ResetMod.Create(__initial.wheelPositions)
        let _instruments = MMap.Create(__initial.instruments, (fun v -> MInstrument.Create(v)), (fun (m,v) -> MInstrument.Update(m, v)), (fun v -> v))
        let _axes = MMap.Create(__initial.axes, (fun v -> MAxis.Create(v)), (fun (m,v) -> MAxis.Update(m, v)), (fun v -> v))
        let _box = ResetMod.Create(__initial.box)
        
        member x.id = _id :> IMod<_>
        member x.platform2Ground = _platform2Ground :> IMod<_>
        member x.wheelPositions = _wheelPositions :> IMod<_>
        member x.instruments = _instruments :> amap<_,_>
        member x.axes = _axes :> amap<_,_>
        member x.box = _box :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Rover) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_platform2Ground,v.platform2Ground)
                ResetMod.Update(_wheelPositions,v.wheelPositions)
                MMap.Update(_instruments, v.instruments)
                MMap.Update(_axes, v.axes)
                ResetMod.Update(_box,v.box)
                
        
        static member Create(__initial : PRo3d.Rover) : MRover = MRover(__initial)
        static member Update(m : MRover, v : PRo3d.Rover) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.Rover> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rover =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<PRo3d.Rover, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let platform2Ground =
                { new Lens<PRo3d.Rover, Aardvark.Base.M44d>() with
                    override x.Get(r) = r.platform2Ground
                    override x.Set(r,v) = { r with platform2Ground = v }
                    override x.Update(r,f) = { r with platform2Ground = f r.platform2Ground }
                }
            let wheelPositions =
                { new Lens<PRo3d.Rover, Microsoft.FSharp.Collections.list<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.wheelPositions
                    override x.Set(r,v) = { r with wheelPositions = v }
                    override x.Update(r,f) = { r with wheelPositions = f r.wheelPositions }
                }
            let instruments =
                { new Lens<PRo3d.Rover, Aardvark.Base.hmap<Microsoft.FSharp.Core.string,PRo3d.Instrument>>() with
                    override x.Get(r) = r.instruments
                    override x.Set(r,v) = { r with instruments = v }
                    override x.Update(r,f) = { r with instruments = f r.instruments }
                }
            let axes =
                { new Lens<PRo3d.Rover, Aardvark.Base.hmap<Microsoft.FSharp.Core.string,PRo3d.Axis>>() with
                    override x.Get(r) = r.axes
                    override x.Set(r,v) = { r with axes = v }
                    override x.Update(r,f) = { r with axes = f r.axes }
                }
            let box =
                { new Lens<PRo3d.Rover, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.box
                    override x.Set(r,v) = { r with box = v }
                    override x.Update(r,f) = { r with box = f r.box }
                }
    
    
    type MRoverModel(__initial : PRo3d.RoverModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.RoverModel> = Aardvark.Base.Incremental.EqModRef<PRo3d.RoverModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.RoverModel>
        let _rovers = MMap.Create(__initial.rovers, (fun v -> MRover.Create(v)), (fun (m,v) -> MRover.Update(m, v)), (fun v -> v))
        let _selectedRover = MOption.Create(__initial.selectedRover, (fun v -> MRover.Create(v)), (fun (m,v) -> MRover.Update(m, v)), (fun v -> v))
        let _selectedInstrument = MOption.Create(__initial.selectedInstrument, (fun v -> MInstrument.Create(v)), (fun (m,v) -> MInstrument.Update(m, v)), (fun v -> v))
        let _selectedAxis = MOption.Create(__initial.selectedAxis, (fun v -> MAxis.Create(v)), (fun (m,v) -> MAxis.Update(m, v)), (fun v -> v))
        let _currentAngle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.currentAngle)
        
        member x.rovers = _rovers :> amap<_,_>
        member x.selectedRover = _selectedRover :> IMod<_>
        member x.selectedInstrument = _selectedInstrument :> IMod<_>
        member x.selectedAxis = _selectedAxis :> IMod<_>
        member x.currentAngle = _currentAngle
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.RoverModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_rovers, v.rovers)
                MOption.Update(_selectedRover, v.selectedRover)
                MOption.Update(_selectedInstrument, v.selectedInstrument)
                MOption.Update(_selectedAxis, v.selectedAxis)
                Aardvark.UI.Mutable.MNumericInput.Update(_currentAngle, v.currentAngle)
                
        
        static member Create(__initial : PRo3d.RoverModel) : MRoverModel = MRoverModel(__initial)
        static member Update(m : MRoverModel, v : PRo3d.RoverModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.RoverModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RoverModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rovers =
                { new Lens<PRo3d.RoverModel, Aardvark.Base.hmap<Microsoft.FSharp.Core.string,PRo3d.Rover>>() with
                    override x.Get(r) = r.rovers
                    override x.Set(r,v) = { r with rovers = v }
                    override x.Update(r,f) = { r with rovers = f r.rovers }
                }
            let selectedRover =
                { new Lens<PRo3d.RoverModel, Microsoft.FSharp.Core.option<PRo3d.Rover>>() with
                    override x.Get(r) = r.selectedRover
                    override x.Set(r,v) = { r with selectedRover = v }
                    override x.Update(r,f) = { r with selectedRover = f r.selectedRover }
                }
            let selectedInstrument =
                { new Lens<PRo3d.RoverModel, Microsoft.FSharp.Core.option<PRo3d.Instrument>>() with
                    override x.Get(r) = r.selectedInstrument
                    override x.Set(r,v) = { r with selectedInstrument = v }
                    override x.Update(r,f) = { r with selectedInstrument = f r.selectedInstrument }
                }
            let selectedAxis =
                { new Lens<PRo3d.RoverModel, Microsoft.FSharp.Core.option<PRo3d.Axis>>() with
                    override x.Get(r) = r.selectedAxis
                    override x.Set(r,v) = { r with selectedAxis = v }
                    override x.Update(r,f) = { r with selectedAxis = f r.selectedAxis }
                }
            let currentAngle =
                { new Lens<PRo3d.RoverModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.currentAngle
                    override x.Set(r,v) = { r with currentAngle = v }
                    override x.Update(r,f) = { r with currentAngle = f r.currentAngle }
                }
