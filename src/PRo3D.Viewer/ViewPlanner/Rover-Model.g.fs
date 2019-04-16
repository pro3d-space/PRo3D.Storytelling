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
        let _focal = Aardvark.UI.Mutable.MNumericInput.Create(__initial.focal)
        let _intrinsics = ResetMod.Create(__initial.intrinsics)
        let _extrinsics = MExtrinsics.Create(__initial.extrinsics)
        let _index = ResetMod.Create(__initial.index)
        
        member x.id = _id :> IMod<_>
        member x.iType = _iType :> IMod<_>
        member x.calibratedFocalLengths = _calibratedFocalLengths :> IMod<_>
        member x.focal = _focal
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
                Aardvark.UI.Mutable.MNumericInput.Update(_focal, v.focal)
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
                { new Lens<PRo3d.Instrument, System.String>() with
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
                { new Lens<PRo3d.Instrument, Microsoft.FSharp.Collections.List<System.Double>>() with
                    override x.Get(r) = r.calibratedFocalLengths
                    override x.Set(r,v) = { r with calibratedFocalLengths = v }
                    override x.Update(r,f) = { r with calibratedFocalLengths = f r.calibratedFocalLengths }
                }
            let focal =
                { new Lens<PRo3d.Instrument, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.focal
                    override x.Set(r,v) = { r with focal = v }
                    override x.Update(r,f) = { r with focal = f r.focal }
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
                { new Lens<PRo3d.Instrument, System.Int32>() with
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
        let _index = ResetMod.Create(__initial.index)
        let _angle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.angle)
        
        member x.id = _id :> IMod<_>
        member x.description = _description :> IMod<_>
        member x.startPoint = _startPoint :> IMod<_>
        member x.endPoint = _endPoint :> IMod<_>
        member x.index = _index :> IMod<_>
        member x.angle = _angle
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Axis) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_description,v.description)
                ResetMod.Update(_startPoint,v.startPoint)
                ResetMod.Update(_endPoint,v.endPoint)
                ResetMod.Update(_index,v.index)
                Aardvark.UI.Mutable.MNumericInput.Update(_angle, v.angle)
                
        
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
                { new Lens<PRo3d.Axis, System.String>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let description =
                { new Lens<PRo3d.Axis, System.String>() with
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
            let index =
                { new Lens<PRo3d.Axis, System.Int32>() with
                    override x.Get(r) = r.index
                    override x.Set(r,v) = { r with index = v }
                    override x.Update(r,f) = { r with index = f r.index }
                }
            let angle =
                { new Lens<PRo3d.Axis, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.angle
                    override x.Set(r,v) = { r with angle = v }
                    override x.Update(r,f) = { r with angle = f r.angle }
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
                { new Lens<PRo3d.Rover, System.String>() with
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
                { new Lens<PRo3d.Rover, Microsoft.FSharp.Collections.List<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.wheelPositions
                    override x.Set(r,v) = { r with wheelPositions = v }
                    override x.Update(r,f) = { r with wheelPositions = f r.wheelPositions }
                }
            let instruments =
                { new Lens<PRo3d.Rover, Aardvark.Base.hmap<System.String,PRo3d.Instrument>>() with
                    override x.Get(r) = r.instruments
                    override x.Set(r,v) = { r with instruments = v }
                    override x.Update(r,f) = { r with instruments = f r.instruments }
                }
            let axes =
                { new Lens<PRo3d.Rover, Aardvark.Base.hmap<System.String,PRo3d.Axis>>() with
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
        let _platforms = MMap.Create(__initial.platforms)
        let _selectedRover = MOption.Create(__initial.selectedRover, (fun v -> MRover.Create(v)), (fun (m,v) -> MRover.Update(m, v)), (fun v -> v))
        
        member x.rovers = _rovers :> amap<_,_>
        member x.platforms = _platforms :> amap<_,_>
        member x.selectedRover = _selectedRover :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.RoverModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_rovers, v.rovers)
                MMap.Update(_platforms, v.platforms)
                MOption.Update(_selectedRover, v.selectedRover)
                
        
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
                { new Lens<PRo3d.RoverModel, Aardvark.Base.hmap<System.String,PRo3d.Rover>>() with
                    override x.Get(r) = r.rovers
                    override x.Set(r,v) = { r with rovers = v }
                    override x.Update(r,f) = { r with rovers = f r.rovers }
                }
            let platforms =
                { new Lens<PRo3d.RoverModel, Aardvark.Base.hmap<System.String,ViewPlanner.IPWrapper.SPlatform>>() with
                    override x.Get(r) = r.platforms
                    override x.Set(r,v) = { r with platforms = v }
                    override x.Update(r,f) = { r with platforms = f r.platforms }
                }
            let selectedRover =
                { new Lens<PRo3d.RoverModel, Microsoft.FSharp.Core.Option<PRo3d.Rover>>() with
                    override x.Get(r) = r.selectedRover
                    override x.Set(r,v) = { r with selectedRover = v }
                    override x.Update(r,f) = { r with selectedRover = f r.selectedRover }
                }
    
    
    type MSimulatedViewData(__initial : PRo3d.SimulatedViewData) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.SimulatedViewData> = Aardvark.Base.Incremental.EqModRef<PRo3d.SimulatedViewData>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.SimulatedViewData>
        let _fileInfo = ResetMod.Create(__initial.fileInfo)
        let _calibration = ResetMod.Create(__initial.calibration)
        let _acquisition = ResetMod.Create(__initial.acquisition)
        
        member x.fileInfo = _fileInfo :> IMod<_>
        member x.calibration = _calibration :> IMod<_>
        member x.acquisition = _acquisition :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.SimulatedViewData) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_fileInfo,v.fileInfo)
                ResetMod.Update(_calibration,v.calibration)
                ResetMod.Update(_acquisition,v.acquisition)
                
        
        static member Create(__initial : PRo3d.SimulatedViewData) : MSimulatedViewData = MSimulatedViewData(__initial)
        static member Update(m : MSimulatedViewData, v : PRo3d.SimulatedViewData) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.SimulatedViewData> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SimulatedViewData =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let fileInfo =
                { new Lens<PRo3d.SimulatedViewData, PRo3d.FileInfo>() with
                    override x.Get(r) = r.fileInfo
                    override x.Set(r,v) = { r with fileInfo = v }
                    override x.Update(r,f) = { r with fileInfo = f r.fileInfo }
                }
            let calibration =
                { new Lens<PRo3d.SimulatedViewData, PRo3d.Calibration>() with
                    override x.Get(r) = r.calibration
                    override x.Set(r,v) = { r with calibration = v }
                    override x.Update(r,f) = { r with calibration = f r.calibration }
                }
            let acquisition =
                { new Lens<PRo3d.SimulatedViewData, PRo3d.Acquisition>() with
                    override x.Get(r) = r.acquisition
                    override x.Set(r,v) = { r with acquisition = v }
                    override x.Update(r,f) = { r with acquisition = f r.acquisition }
                }
    
    
    type MFootPrint(__initial : PRo3d.FootPrint) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.FootPrint> = Aardvark.Base.Incremental.EqModRef<PRo3d.FootPrint>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.FootPrint>
        let _vpId = MOption.Create(__initial.vpId)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _projectionMatrix = ResetMod.Create(__initial.projectionMatrix)
        let _instViewMatrix = ResetMod.Create(__initial.instViewMatrix)
        let _projTex = ResetMod.Create(__initial.projTex)
        let _globalToLocalPos = ResetMod.Create(__initial.globalToLocalPos)
        
        member x.vpId = _vpId :> IMod<_>
        member x.isVisible = _isVisible :> IMod<_>
        member x.projectionMatrix = _projectionMatrix :> IMod<_>
        member x.instViewMatrix = _instViewMatrix :> IMod<_>
        member x.projTex = _projTex :> IMod<_>
        member x.globalToLocalPos = _globalToLocalPos :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.FootPrint) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MOption.Update(_vpId, v.vpId)
                ResetMod.Update(_isVisible,v.isVisible)
                ResetMod.Update(_projectionMatrix,v.projectionMatrix)
                ResetMod.Update(_instViewMatrix,v.instViewMatrix)
                ResetMod.Update(_projTex,v.projTex)
                ResetMod.Update(_globalToLocalPos,v.globalToLocalPos)
                
        
        static member Create(__initial : PRo3d.FootPrint) : MFootPrint = MFootPrint(__initial)
        static member Update(m : MFootPrint, v : PRo3d.FootPrint) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.FootPrint> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FootPrint =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let vpId =
                { new Lens<PRo3d.FootPrint, Microsoft.FSharp.Core.Option<System.Guid>>() with
                    override x.Get(r) = r.vpId
                    override x.Set(r,v) = { r with vpId = v }
                    override x.Update(r,f) = { r with vpId = f r.vpId }
                }
            let isVisible =
                { new Lens<PRo3d.FootPrint, System.Boolean>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
            let projectionMatrix =
                { new Lens<PRo3d.FootPrint, Aardvark.Base.M44d>() with
                    override x.Get(r) = r.projectionMatrix
                    override x.Set(r,v) = { r with projectionMatrix = v }
                    override x.Update(r,f) = { r with projectionMatrix = f r.projectionMatrix }
                }
            let instViewMatrix =
                { new Lens<PRo3d.FootPrint, Aardvark.Base.M44d>() with
                    override x.Get(r) = r.instViewMatrix
                    override x.Set(r,v) = { r with instViewMatrix = v }
                    override x.Update(r,f) = { r with instViewMatrix = f r.instViewMatrix }
                }
            let projTex =
                { new Lens<PRo3d.FootPrint, Aardvark.Base.ITexture>() with
                    override x.Get(r) = r.projTex
                    override x.Set(r,v) = { r with projTex = v }
                    override x.Update(r,f) = { r with projTex = f r.projTex }
                }
            let globalToLocalPos =
                { new Lens<PRo3d.FootPrint, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.globalToLocalPos
                    override x.Set(r,v) = { r with globalToLocalPos = v }
                    override x.Update(r,f) = { r with globalToLocalPos = f r.globalToLocalPos }
                }
    
    
    type MViewPlan(__initial : PRo3d.ViewPlan) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.ViewPlan> = Aardvark.Base.Incremental.EqModRef<PRo3d.ViewPlan>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.ViewPlan>
        let _id = ResetMod.Create(__initial.id)
        let _name = ResetMod.Create(__initial.name)
        let _position = ResetMod.Create(__initial.position)
        let _lookAt = ResetMod.Create(__initial.lookAt)
        let _viewerState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.viewerState)
        let _vectorsVisible = ResetMod.Create(__initial.vectorsVisible)
        let _rover = MRover.Create(__initial.rover)
        let _roverTrafo = ResetMod.Create(__initial.roverTrafo)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _selectedInstrument = MOption.Create(__initial.selectedInstrument, (fun v -> MInstrument.Create(v)), (fun (m,v) -> MInstrument.Update(m, v)), (fun v -> v))
        let _selectedAxis = MOption.Create(__initial.selectedAxis, (fun v -> MAxis.Create(v)), (fun (m,v) -> MAxis.Update(m, v)), (fun v -> v))
        let _currentAngle = Aardvark.UI.Mutable.MNumericInput.Create(__initial.currentAngle)
        
        member x.id = _id :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.position = _position :> IMod<_>
        member x.lookAt = _lookAt :> IMod<_>
        member x.viewerState = _viewerState
        member x.vectorsVisible = _vectorsVisible :> IMod<_>
        member x.rover = _rover
        member x.roverTrafo = _roverTrafo :> IMod<_>
        member x.isVisible = _isVisible :> IMod<_>
        member x.selectedInstrument = _selectedInstrument :> IMod<_>
        member x.selectedAxis = _selectedAxis :> IMod<_>
        member x.currentAngle = _currentAngle
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.ViewPlan) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_position,v.position)
                ResetMod.Update(_lookAt,v.lookAt)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_viewerState, v.viewerState)
                ResetMod.Update(_vectorsVisible,v.vectorsVisible)
                MRover.Update(_rover, v.rover)
                ResetMod.Update(_roverTrafo,v.roverTrafo)
                ResetMod.Update(_isVisible,v.isVisible)
                MOption.Update(_selectedInstrument, v.selectedInstrument)
                MOption.Update(_selectedAxis, v.selectedAxis)
                Aardvark.UI.Mutable.MNumericInput.Update(_currentAngle, v.currentAngle)
                
        
        static member Create(__initial : PRo3d.ViewPlan) : MViewPlan = MViewPlan(__initial)
        static member Update(m : MViewPlan, v : PRo3d.ViewPlan) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.ViewPlan> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ViewPlan =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<PRo3d.ViewPlan, System.Guid>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let name =
                { new Lens<PRo3d.ViewPlan, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let position =
                { new Lens<PRo3d.ViewPlan, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.position
                    override x.Set(r,v) = { r with position = v }
                    override x.Update(r,f) = { r with position = f r.position }
                }
            let lookAt =
                { new Lens<PRo3d.ViewPlan, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.lookAt
                    override x.Set(r,v) = { r with lookAt = v }
                    override x.Update(r,f) = { r with lookAt = f r.lookAt }
                }
            let viewerState =
                { new Lens<PRo3d.ViewPlan, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.viewerState
                    override x.Set(r,v) = { r with viewerState = v }
                    override x.Update(r,f) = { r with viewerState = f r.viewerState }
                }
            let vectorsVisible =
                { new Lens<PRo3d.ViewPlan, System.Boolean>() with
                    override x.Get(r) = r.vectorsVisible
                    override x.Set(r,v) = { r with vectorsVisible = v }
                    override x.Update(r,f) = { r with vectorsVisible = f r.vectorsVisible }
                }
            let rover =
                { new Lens<PRo3d.ViewPlan, PRo3d.Rover>() with
                    override x.Get(r) = r.rover
                    override x.Set(r,v) = { r with rover = v }
                    override x.Update(r,f) = { r with rover = f r.rover }
                }
            let roverTrafo =
                { new Lens<PRo3d.ViewPlan, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.roverTrafo
                    override x.Set(r,v) = { r with roverTrafo = v }
                    override x.Update(r,f) = { r with roverTrafo = f r.roverTrafo }
                }
            let isVisible =
                { new Lens<PRo3d.ViewPlan, System.Boolean>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
            let selectedInstrument =
                { new Lens<PRo3d.ViewPlan, Microsoft.FSharp.Core.Option<PRo3d.Instrument>>() with
                    override x.Get(r) = r.selectedInstrument
                    override x.Set(r,v) = { r with selectedInstrument = v }
                    override x.Update(r,f) = { r with selectedInstrument = f r.selectedInstrument }
                }
            let selectedAxis =
                { new Lens<PRo3d.ViewPlan, Microsoft.FSharp.Core.Option<PRo3d.Axis>>() with
                    override x.Get(r) = r.selectedAxis
                    override x.Set(r,v) = { r with selectedAxis = v }
                    override x.Update(r,f) = { r with selectedAxis = f r.selectedAxis }
                }
            let currentAngle =
                { new Lens<PRo3d.ViewPlan, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.currentAngle
                    override x.Set(r,v) = { r with currentAngle = v }
                    override x.Update(r,f) = { r with currentAngle = f r.currentAngle }
                }
    
    
    type MViewPlanModel(__initial : PRo3d.ViewPlanModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.ViewPlanModel> = Aardvark.Base.Incremental.EqModRef<PRo3d.ViewPlanModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.ViewPlanModel>
        let _viewPlans = MSet.Create((fun (v : PRo3d.ViewPlan) -> v.id :> obj), __initial.viewPlans, (fun v -> MViewPlan.Create(v)), (fun (m,v) -> MViewPlan.Update(m, v)), (fun v -> v))
        let _selectedViewPlan = MOption.Create(__initial.selectedViewPlan, (fun v -> MViewPlan.Create(v)), (fun (m,v) -> MViewPlan.Update(m, v)), (fun v -> v))
        let _working = ResetMod.Create(__initial.working)
        let _roverModel = MRoverModel.Create(__initial.roverModel)
        let _instrumentCam = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.instrumentCam)
        let _instrumentFrustum = ResetMod.Create(__initial.instrumentFrustum)
        
        member x.viewPlans = _viewPlans :> aset<_>
        member x.selectedViewPlan = _selectedViewPlan :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.roverModel = _roverModel
        member x.instrumentCam = _instrumentCam
        member x.instrumentFrustum = _instrumentFrustum :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.ViewPlanModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MSet.Update(_viewPlans, v.viewPlans)
                MOption.Update(_selectedViewPlan, v.selectedViewPlan)
                ResetMod.Update(_working,v.working)
                MRoverModel.Update(_roverModel, v.roverModel)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_instrumentCam, v.instrumentCam)
                ResetMod.Update(_instrumentFrustum,v.instrumentFrustum)
                
        
        static member Create(__initial : PRo3d.ViewPlanModel) : MViewPlanModel = MViewPlanModel(__initial)
        static member Update(m : MViewPlanModel, v : PRo3d.ViewPlanModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.ViewPlanModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ViewPlanModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let viewPlans =
                { new Lens<PRo3d.ViewPlanModel, Aardvark.Base.hset<PRo3d.ViewPlan>>() with
                    override x.Get(r) = r.viewPlans
                    override x.Set(r,v) = { r with viewPlans = v }
                    override x.Update(r,f) = { r with viewPlans = f r.viewPlans }
                }
            let selectedViewPlan =
                { new Lens<PRo3d.ViewPlanModel, Microsoft.FSharp.Core.Option<PRo3d.ViewPlan>>() with
                    override x.Get(r) = r.selectedViewPlan
                    override x.Set(r,v) = { r with selectedViewPlan = v }
                    override x.Update(r,f) = { r with selectedViewPlan = f r.selectedViewPlan }
                }
            let working =
                { new Lens<PRo3d.ViewPlanModel, Microsoft.FSharp.Collections.List<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let roverModel =
                { new Lens<PRo3d.ViewPlanModel, PRo3d.RoverModel>() with
                    override x.Get(r) = r.roverModel
                    override x.Set(r,v) = { r with roverModel = v }
                    override x.Update(r,f) = { r with roverModel = f r.roverModel }
                }
            let instrumentCam =
                { new Lens<PRo3d.ViewPlanModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.instrumentCam
                    override x.Set(r,v) = { r with instrumentCam = v }
                    override x.Update(r,f) = { r with instrumentCam = f r.instrumentCam }
                }
            let instrumentFrustum =
                { new Lens<PRo3d.ViewPlanModel, Aardvark.Base.Frustum>() with
                    override x.Get(r) = r.instrumentFrustum
                    override x.Set(r,v) = { r with instrumentFrustum = v }
                    override x.Update(r,f) = { r with instrumentFrustum = f r.instrumentFrustum }
                }
