namespace PRo3D.Minerva

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Minerva

[<AutoOpen>]
module Mutable =

    
    
    type MFeatureCollection(__initial : PRo3D.Minerva.FeatureCollection) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.FeatureCollection> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.FeatureCollection>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.FeatureCollection>
        let _name = ResetMod.Create(__initial.name)
        let _typus = ResetMod.Create(__initial.typus)
        let _boundingBox = ResetMod.Create(__initial.boundingBox)
        let _features = MList.Create(__initial.features)
        
        member x.name = _name :> IMod<_>
        member x.typus = _typus :> IMod<_>
        member x.boundingBox = _boundingBox :> IMod<_>
        member x.features = _features :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.FeatureCollection) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_typus,v.typus)
                ResetMod.Update(_boundingBox,v.boundingBox)
                MList.Update(_features, v.features)
                
        
        static member Create(__initial : PRo3D.Minerva.FeatureCollection) : MFeatureCollection = MFeatureCollection(__initial)
        static member Update(m : MFeatureCollection, v : PRo3D.Minerva.FeatureCollection) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.FeatureCollection> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureCollection =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let name =
                { new Lens<PRo3D.Minerva.FeatureCollection, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let typus =
                { new Lens<PRo3D.Minerva.FeatureCollection, PRo3D.Minerva.Typus>() with
                    override x.Get(r) = r.typus
                    override x.Set(r,v) = { r with typus = v }
                    override x.Update(r,f) = { r with typus = f r.typus }
                }
            let boundingBox =
                { new Lens<PRo3D.Minerva.FeatureCollection, Aardvark.Base.Box2d>() with
                    override x.Get(r) = r.boundingBox
                    override x.Set(r,v) = { r with boundingBox = v }
                    override x.Update(r,f) = { r with boundingBox = f r.boundingBox }
                }
            let features =
                { new Lens<PRo3D.Minerva.FeatureCollection, Aardvark.Base.plist<PRo3D.Minerva.Feature>>() with
                    override x.Get(r) = r.features
                    override x.Set(r,v) = { r with features = v }
                    override x.Update(r,f) = { r with features = f r.features }
                }
    
    
    type MSgFeatures(__initial : PRo3D.Minerva.SgFeatures) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.SgFeatures> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.SgFeatures>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.SgFeatures>
        let _positions = ResetMod.Create(__initial.positions)
        let _colors = ResetMod.Create(__initial.colors)
        
        member x.positions = _positions :> IMod<_>
        member x.colors = _colors :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.SgFeatures) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_positions,v.positions)
                ResetMod.Update(_colors,v.colors)
                
        
        static member Create(__initial : PRo3D.Minerva.SgFeatures) : MSgFeatures = MSgFeatures(__initial)
        static member Update(m : MSgFeatures, v : PRo3D.Minerva.SgFeatures) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.SgFeatures> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SgFeatures =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let positions =
                { new Lens<PRo3D.Minerva.SgFeatures, Aardvark.Base.V3d[]>() with
                    override x.Get(r) = r.positions
                    override x.Set(r,v) = { r with positions = v }
                    override x.Update(r,f) = { r with positions = f r.positions }
                }
            let colors =
                { new Lens<PRo3D.Minerva.SgFeatures, Aardvark.Base.C4b[]>() with
                    override x.Get(r) = r.colors
                    override x.Set(r,v) = { r with colors = v }
                    override x.Update(r,f) = { r with colors = f r.colors }
                }
    
    
    type MQueryModel(__initial : PRo3D.Minerva.QueryModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.QueryModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.QueryModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.QueryModel>
        let _minSol = Aardvark.UI.Mutable.MNumericInput.Create(__initial.minSol)
        let _maxSol = Aardvark.UI.Mutable.MNumericInput.Create(__initial.maxSol)
        let _instrument = ResetMod.Create(__initial.instrument)
        let _distance = Aardvark.UI.Mutable.MNumericInput.Create(__initial.distance)
        let _distFilteredFeatures = MList.Create(__initial.distFilteredFeatures)
        let _solLables = MMap.Create(__initial.solLables)
        let _checkMAHLI = ResetMod.Create(__initial.checkMAHLI)
        let _checkFrontHazcam = ResetMod.Create(__initial.checkFrontHazcam)
        let _checkMastcam = ResetMod.Create(__initial.checkMastcam)
        let _checkAPXS = ResetMod.Create(__initial.checkAPXS)
        let _checkFrontHazcamR = ResetMod.Create(__initial.checkFrontHazcamR)
        let _checkFrontHazcamL = ResetMod.Create(__initial.checkFrontHazcamL)
        let _checkMastcamR = ResetMod.Create(__initial.checkMastcamR)
        let _checkMastcamL = ResetMod.Create(__initial.checkMastcamL)
        let _checkChemLib = ResetMod.Create(__initial.checkChemLib)
        let _checkChemRmi = ResetMod.Create(__initial.checkChemRmi)
        let _pointSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.pointSize)
        let _sgFeatures = MSgFeatures.Create(__initial.sgFeatures)
        
        member x.minSol = _minSol
        member x.maxSol = _maxSol
        member x.instrument = _instrument :> IMod<_>
        member x.distance = _distance
        member x.distFilteredFeatures = _distFilteredFeatures :> alist<_>
        member x.solLables = _solLables :> amap<_,_>
        member x.checkMAHLI = _checkMAHLI :> IMod<_>
        member x.checkFrontHazcam = _checkFrontHazcam :> IMod<_>
        member x.checkMastcam = _checkMastcam :> IMod<_>
        member x.checkAPXS = _checkAPXS :> IMod<_>
        member x.checkFrontHazcamR = _checkFrontHazcamR :> IMod<_>
        member x.checkFrontHazcamL = _checkFrontHazcamL :> IMod<_>
        member x.checkMastcamR = _checkMastcamR :> IMod<_>
        member x.checkMastcamL = _checkMastcamL :> IMod<_>
        member x.checkChemLib = _checkChemLib :> IMod<_>
        member x.checkChemRmi = _checkChemRmi :> IMod<_>
        member x.pointSize = _pointSize
        member x.sgFeatures = _sgFeatures
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.QueryModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MNumericInput.Update(_minSol, v.minSol)
                Aardvark.UI.Mutable.MNumericInput.Update(_maxSol, v.maxSol)
                ResetMod.Update(_instrument,v.instrument)
                Aardvark.UI.Mutable.MNumericInput.Update(_distance, v.distance)
                MList.Update(_distFilteredFeatures, v.distFilteredFeatures)
                MMap.Update(_solLables, v.solLables)
                ResetMod.Update(_checkMAHLI,v.checkMAHLI)
                ResetMod.Update(_checkFrontHazcam,v.checkFrontHazcam)
                ResetMod.Update(_checkMastcam,v.checkMastcam)
                ResetMod.Update(_checkAPXS,v.checkAPXS)
                ResetMod.Update(_checkFrontHazcamR,v.checkFrontHazcamR)
                ResetMod.Update(_checkFrontHazcamL,v.checkFrontHazcamL)
                ResetMod.Update(_checkMastcamR,v.checkMastcamR)
                ResetMod.Update(_checkMastcamL,v.checkMastcamL)
                ResetMod.Update(_checkChemLib,v.checkChemLib)
                ResetMod.Update(_checkChemRmi,v.checkChemRmi)
                Aardvark.UI.Mutable.MNumericInput.Update(_pointSize, v.pointSize)
                MSgFeatures.Update(_sgFeatures, v.sgFeatures)
                
        
        static member Create(__initial : PRo3D.Minerva.QueryModel) : MQueryModel = MQueryModel(__initial)
        static member Update(m : MQueryModel, v : PRo3D.Minerva.QueryModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.QueryModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module QueryModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let minSol =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.minSol
                    override x.Set(r,v) = { r with minSol = v }
                    override x.Update(r,f) = { r with minSol = f r.minSol }
                }
            let maxSol =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.maxSol
                    override x.Set(r,v) = { r with maxSol = v }
                    override x.Update(r,f) = { r with maxSol = f r.maxSol }
                }
            let instrument =
                { new Lens<PRo3D.Minerva.QueryModel, PRo3D.Minerva.Instrument>() with
                    override x.Get(r) = r.instrument
                    override x.Set(r,v) = { r with instrument = v }
                    override x.Update(r,f) = { r with instrument = f r.instrument }
                }
            let distance =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.distance
                    override x.Set(r,v) = { r with distance = v }
                    override x.Update(r,f) = { r with distance = f r.distance }
                }
            let distFilteredFeatures =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.Base.plist<PRo3D.Minerva.Feature>>() with
                    override x.Get(r) = r.distFilteredFeatures
                    override x.Set(r,v) = { r with distFilteredFeatures = v }
                    override x.Update(r,f) = { r with distFilteredFeatures = f r.distFilteredFeatures }
                }
            let solLables =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.Base.hmap<System.String,Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.solLables
                    override x.Set(r,v) = { r with solLables = v }
                    override x.Update(r,f) = { r with solLables = f r.solLables }
                }
            let checkMAHLI =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkMAHLI
                    override x.Set(r,v) = { r with checkMAHLI = v }
                    override x.Update(r,f) = { r with checkMAHLI = f r.checkMAHLI }
                }
            let checkFrontHazcam =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkFrontHazcam
                    override x.Set(r,v) = { r with checkFrontHazcam = v }
                    override x.Update(r,f) = { r with checkFrontHazcam = f r.checkFrontHazcam }
                }
            let checkMastcam =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkMastcam
                    override x.Set(r,v) = { r with checkMastcam = v }
                    override x.Update(r,f) = { r with checkMastcam = f r.checkMastcam }
                }
            let checkAPXS =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkAPXS
                    override x.Set(r,v) = { r with checkAPXS = v }
                    override x.Update(r,f) = { r with checkAPXS = f r.checkAPXS }
                }
            let checkFrontHazcamR =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkFrontHazcamR
                    override x.Set(r,v) = { r with checkFrontHazcamR = v }
                    override x.Update(r,f) = { r with checkFrontHazcamR = f r.checkFrontHazcamR }
                }
            let checkFrontHazcamL =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkFrontHazcamL
                    override x.Set(r,v) = { r with checkFrontHazcamL = v }
                    override x.Update(r,f) = { r with checkFrontHazcamL = f r.checkFrontHazcamL }
                }
            let checkMastcamR =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkMastcamR
                    override x.Set(r,v) = { r with checkMastcamR = v }
                    override x.Update(r,f) = { r with checkMastcamR = f r.checkMastcamR }
                }
            let checkMastcamL =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkMastcamL
                    override x.Set(r,v) = { r with checkMastcamL = v }
                    override x.Update(r,f) = { r with checkMastcamL = f r.checkMastcamL }
                }
            let checkChemLib =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkChemLib
                    override x.Set(r,v) = { r with checkChemLib = v }
                    override x.Update(r,f) = { r with checkChemLib = f r.checkChemLib }
                }
            let checkChemRmi =
                { new Lens<PRo3D.Minerva.QueryModel, System.Boolean>() with
                    override x.Get(r) = r.checkChemRmi
                    override x.Set(r,v) = { r with checkChemRmi = v }
                    override x.Update(r,f) = { r with checkChemRmi = f r.checkChemRmi }
                }
            let pointSize =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.pointSize
                    override x.Set(r,v) = { r with pointSize = v }
                    override x.Update(r,f) = { r with pointSize = f r.pointSize }
                }
            let sgFeatures =
                { new Lens<PRo3D.Minerva.QueryModel, PRo3D.Minerva.SgFeatures>() with
                    override x.Get(r) = r.sgFeatures
                    override x.Set(r,v) = { r with sgFeatures = v }
                    override x.Update(r,f) = { r with sgFeatures = f r.sgFeatures }
                }
    
    
    type MModel(__initial : PRo3D.Minerva.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.Model> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.Model>
        let _data = MFeatureCollection.Create(__initial.data)
        let _queries = ResetMod.Create(__initial.queries)
        let _vplMessages = ResetMod.Create(__initial.vplMessages)
        let _minervaMessagingMailbox = ResetMod.Create(__initial.minervaMessagingMailbox)
        let _minervaMailboxState = ResetMod.Create(__initial.minervaMailboxState)
        let _queryFilter = MQueryModel.Create(__initial.queryFilter)
        
        member x.data = _data
        member x.queries = _queries :> IMod<_>
        member x.comm = __current.Value.comm
        member x.vplMessages = _vplMessages :> IMod<_>
        member x.minervaMessagingMailbox = _minervaMessagingMailbox :> IMod<_>
        member x.minervaMailboxState = _minervaMailboxState :> IMod<_>
        member x.queryFilter = _queryFilter
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MFeatureCollection.Update(_data, v.data)
                ResetMod.Update(_queries,v.queries)
                ResetMod.Update(_vplMessages,v.vplMessages)
                ResetMod.Update(_minervaMessagingMailbox,v.minervaMessagingMailbox)
                ResetMod.Update(_minervaMailboxState,v.minervaMailboxState)
                MQueryModel.Update(_queryFilter, v.queryFilter)
                
        
        static member Create(__initial : PRo3D.Minerva.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : PRo3D.Minerva.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let data =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.FeatureCollection>() with
                    override x.Get(r) = r.data
                    override x.Set(r,v) = { r with data = v }
                    override x.Update(r,f) = { r with data = f r.data }
                }
            let queries =
                { new Lens<PRo3D.Minerva.Model, Microsoft.FSharp.Collections.List<System.String>>() with
                    override x.Get(r) = r.queries
                    override x.Set(r,v) = { r with queries = v }
                    override x.Update(r,f) = { r with queries = f r.queries }
                }
            let comm =
                { new Lens<PRo3D.Minerva.Model, Microsoft.FSharp.Core.Option<PRo3D.Minerva.Communication.Communicator.Communicator>>() with
                    override x.Get(r) = r.comm
                    override x.Set(r,v) = { r with comm = v }
                    override x.Update(r,f) = { r with comm = f r.comm }
                }
            let vplMessages =
                { new Lens<PRo3D.Minerva.Model, Aardvark.Base.Incremental.ThreadPool<PRo3D.Minerva.MinervaAction>>() with
                    override x.Get(r) = r.vplMessages
                    override x.Set(r,v) = { r with vplMessages = v }
                    override x.Update(r,f) = { r with vplMessages = f r.vplMessages }
                }
            let minervaMessagingMailbox =
                { new Lens<PRo3D.Minerva.Model, Microsoft.FSharp.Control.MailboxProcessor<PRo3D.Minerva.MailboxAction>>() with
                    override x.Get(r) = r.minervaMessagingMailbox
                    override x.Set(r,v) = { r with minervaMessagingMailbox = v }
                    override x.Update(r,f) = { r with minervaMessagingMailbox = f r.minervaMessagingMailbox }
                }
            let minervaMailboxState =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.MailboxState>() with
                    override x.Get(r) = r.minervaMailboxState
                    override x.Set(r,v) = { r with minervaMailboxState = v }
                    override x.Update(r,f) = { r with minervaMailboxState = f r.minervaMailboxState }
                }
            let queryFilter =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.QueryModel>() with
                    override x.Get(r) = r.queryFilter
                    override x.Set(r,v) = { r with queryFilter = v }
                    override x.Update(r,f) = { r with queryFilter = f r.queryFilter }
                }
