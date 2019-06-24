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
        let _names = ResetMod.Create(__initial.names)
        let _positions = ResetMod.Create(__initial.positions)
        let _colors = ResetMod.Create(__initial.colors)
        let _trafo = ResetMod.Create(__initial.trafo)
        
        member x.names = _names :> IMod<_>
        member x.positions = _positions :> IMod<_>
        member x.colors = _colors :> IMod<_>
        member x.trafo = _trafo :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.SgFeatures) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_names,v.names)
                ResetMod.Update(_positions,v.positions)
                ResetMod.Update(_colors,v.colors)
                ResetMod.Update(_trafo,v.trafo)
                
        
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
            let names =
                { new Lens<PRo3D.Minerva.SgFeatures, System.String[]>() with
                    override x.Get(r) = r.names
                    override x.Set(r,v) = { r with names = v }
                    override x.Update(r,f) = { r with names = f r.names }
                }
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
            let trafo =
                { new Lens<PRo3D.Minerva.SgFeatures, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.trafo
                    override x.Set(r,v) = { r with trafo = v }
                    override x.Update(r,f) = { r with trafo = f r.trafo }
                }
    
    
    type MInstrumentColor(__initial : PRo3D.Minerva.InstrumentColor) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.InstrumentColor> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.InstrumentColor>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.InstrumentColor>
        let _mahli = ResetMod.Create(__initial.mahli)
        let _frontHazcam = ResetMod.Create(__initial.frontHazcam)
        let _mastcam = ResetMod.Create(__initial.mastcam)
        let _apxs = ResetMod.Create(__initial.apxs)
        let _frontHazcamR = ResetMod.Create(__initial.frontHazcamR)
        let _frontHazcamL = ResetMod.Create(__initial.frontHazcamL)
        let _mastcamR = ResetMod.Create(__initial.mastcamR)
        let _mastcamL = ResetMod.Create(__initial.mastcamL)
        let _chemLib = ResetMod.Create(__initial.chemLib)
        let _chemRmi = ResetMod.Create(__initial.chemRmi)
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        
        member x.mahli = _mahli :> IMod<_>
        member x.frontHazcam = _frontHazcam :> IMod<_>
        member x.mastcam = _mastcam :> IMod<_>
        member x.apxs = _apxs :> IMod<_>
        member x.frontHazcamR = _frontHazcamR :> IMod<_>
        member x.frontHazcamL = _frontHazcamL :> IMod<_>
        member x.mastcamR = _mastcamR :> IMod<_>
        member x.mastcamL = _mastcamL :> IMod<_>
        member x.chemLib = _chemLib :> IMod<_>
        member x.chemRmi = _chemRmi :> IMod<_>
        member x.color = _color
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.InstrumentColor) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_mahli,v.mahli)
                ResetMod.Update(_frontHazcam,v.frontHazcam)
                ResetMod.Update(_mastcam,v.mastcam)
                ResetMod.Update(_apxs,v.apxs)
                ResetMod.Update(_frontHazcamR,v.frontHazcamR)
                ResetMod.Update(_frontHazcamL,v.frontHazcamL)
                ResetMod.Update(_mastcamR,v.mastcamR)
                ResetMod.Update(_mastcamL,v.mastcamL)
                ResetMod.Update(_chemLib,v.chemLib)
                ResetMod.Update(_chemRmi,v.chemRmi)
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                
        
        static member Create(__initial : PRo3D.Minerva.InstrumentColor) : MInstrumentColor = MInstrumentColor(__initial)
        static member Update(m : MInstrumentColor, v : PRo3D.Minerva.InstrumentColor) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.InstrumentColor> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module InstrumentColor =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let mahli =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.mahli
                    override x.Set(r,v) = { r with mahli = v }
                    override x.Update(r,f) = { r with mahli = f r.mahli }
                }
            let frontHazcam =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.frontHazcam
                    override x.Set(r,v) = { r with frontHazcam = v }
                    override x.Update(r,f) = { r with frontHazcam = f r.frontHazcam }
                }
            let mastcam =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.mastcam
                    override x.Set(r,v) = { r with mastcam = v }
                    override x.Update(r,f) = { r with mastcam = f r.mastcam }
                }
            let apxs =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.apxs
                    override x.Set(r,v) = { r with apxs = v }
                    override x.Update(r,f) = { r with apxs = f r.apxs }
                }
            let frontHazcamR =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.frontHazcamR
                    override x.Set(r,v) = { r with frontHazcamR = v }
                    override x.Update(r,f) = { r with frontHazcamR = f r.frontHazcamR }
                }
            let frontHazcamL =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.frontHazcamL
                    override x.Set(r,v) = { r with frontHazcamL = v }
                    override x.Update(r,f) = { r with frontHazcamL = f r.frontHazcamL }
                }
            let mastcamR =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.mastcamR
                    override x.Set(r,v) = { r with mastcamR = v }
                    override x.Update(r,f) = { r with mastcamR = f r.mastcamR }
                }
            let mastcamL =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.mastcamL
                    override x.Set(r,v) = { r with mastcamL = v }
                    override x.Update(r,f) = { r with mastcamL = f r.mastcamL }
                }
            let chemLib =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.chemLib
                    override x.Set(r,v) = { r with chemLib = v }
                    override x.Update(r,f) = { r with chemLib = f r.chemLib }
                }
            let chemRmi =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.chemRmi
                    override x.Set(r,v) = { r with chemRmi = v }
                    override x.Update(r,f) = { r with chemRmi = f r.chemRmi }
                }
            let color =
                { new Lens<PRo3D.Minerva.InstrumentColor, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
    
    
    type MFeatureProperties(__initial : PRo3D.Minerva.FeatureProperties) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.FeatureProperties> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.FeatureProperties>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.FeatureProperties>
        let _pointSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.pointSize)
        let _textSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.textSize)
        
        member x.pointSize = _pointSize
        member x.textSize = _textSize
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.FeatureProperties) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MNumericInput.Update(_pointSize, v.pointSize)
                Aardvark.UI.Mutable.MNumericInput.Update(_textSize, v.textSize)
                
        
        static member Create(__initial : PRo3D.Minerva.FeatureProperties) : MFeatureProperties = MFeatureProperties(__initial)
        static member Update(m : MFeatureProperties, v : PRo3D.Minerva.FeatureProperties) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.FeatureProperties> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureProperties =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let pointSize =
                { new Lens<PRo3D.Minerva.FeatureProperties, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.pointSize
                    override x.Set(r,v) = { r with pointSize = v }
                    override x.Update(r,f) = { r with pointSize = f r.pointSize }
                }
            let textSize =
                { new Lens<PRo3D.Minerva.FeatureProperties, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.textSize
                    override x.Set(r,v) = { r with textSize = v }
                    override x.Update(r,f) = { r with textSize = f r.textSize }
                }
    
    
    type MQueryModel(__initial : PRo3D.Minerva.QueryModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.QueryModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.QueryModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.QueryModel>
        let _minSol = Aardvark.UI.Mutable.MNumericInput.Create(__initial.minSol)
        let _maxSol = Aardvark.UI.Mutable.MNumericInput.Create(__initial.maxSol)
        let _distance = Aardvark.UI.Mutable.MNumericInput.Create(__initial.distance)
        let _filterLocation = ResetMod.Create(__initial.filterLocation)
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
        
        member x.minSol = _minSol
        member x.maxSol = _maxSol
        member x.distance = _distance
        member x.filterLocation = _filterLocation :> IMod<_>
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
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.QueryModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MNumericInput.Update(_minSol, v.minSol)
                Aardvark.UI.Mutable.MNumericInput.Update(_maxSol, v.maxSol)
                Aardvark.UI.Mutable.MNumericInput.Update(_distance, v.distance)
                ResetMod.Update(_filterLocation,v.filterLocation)
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
            let distance =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.distance
                    override x.Set(r,v) = { r with distance = v }
                    override x.Update(r,f) = { r with distance = f r.distance }
                }
            let filterLocation =
                { new Lens<PRo3D.Minerva.QueryModel, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.filterLocation
                    override x.Set(r,v) = { r with filterLocation = v }
                    override x.Update(r,f) = { r with filterLocation = f r.filterLocation }
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
    
    
    type MModel(__initial : PRo3D.Minerva.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.Model> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.Model>
        let _data = MFeatureCollection.Create(__initial.data)
        let _queries = ResetMod.Create(__initial.queries)
        let _queryFilter = MQueryModel.Create(__initial.queryFilter)
        let _filteredFeatures = MList.Create(__initial.filteredFeatures)
        let _vplMessages = ResetMod.Create(__initial.vplMessages)
        let _minervaMessagingMailbox = ResetMod.Create(__initial.minervaMessagingMailbox)
        let _minervaMailboxState = ResetMod.Create(__initial.minervaMailboxState)
        let _featureProperties = MFeatureProperties.Create(__initial.featureProperties)
        let _selection = ResetMod.Create(__initial.selection)
        let _solLabels = MMap.Create(__initial.solLabels)
        let _sgFeatures = MSgFeatures.Create(__initial.sgFeatures)
        let _selectedSgFeatures = MSgFeatures.Create(__initial.selectedSgFeatures)
        let _picking = ResetMod.Create(__initial.picking)
        
        member x.data = _data
        member x.queries = _queries :> IMod<_>
        member x.queryFilter = _queryFilter
        member x.filteredFeatures = _filteredFeatures :> alist<_>
        member x.comm = __current.Value.comm
        member x.vplMessages = _vplMessages :> IMod<_>
        member x.minervaMessagingMailbox = _minervaMessagingMailbox :> IMod<_>
        member x.minervaMailboxState = _minervaMailboxState :> IMod<_>
        member x.featureProperties = _featureProperties
        member x.selection = _selection :> IMod<_>
        member x.solLabels = _solLabels :> amap<_,_>
        member x.sgFeatures = _sgFeatures
        member x.selectedSgFeatures = _selectedSgFeatures
        member x.picking = _picking :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MFeatureCollection.Update(_data, v.data)
                ResetMod.Update(_queries,v.queries)
                MQueryModel.Update(_queryFilter, v.queryFilter)
                MList.Update(_filteredFeatures, v.filteredFeatures)
                ResetMod.Update(_vplMessages,v.vplMessages)
                ResetMod.Update(_minervaMessagingMailbox,v.minervaMessagingMailbox)
                ResetMod.Update(_minervaMailboxState,v.minervaMailboxState)
                MFeatureProperties.Update(_featureProperties, v.featureProperties)
                ResetMod.Update(_selection,v.selection)
                MMap.Update(_solLabels, v.solLabels)
                MSgFeatures.Update(_sgFeatures, v.sgFeatures)
                MSgFeatures.Update(_selectedSgFeatures, v.selectedSgFeatures)
                ResetMod.Update(_picking,v.picking)
                
        
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
            let queryFilter =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.QueryModel>() with
                    override x.Get(r) = r.queryFilter
                    override x.Set(r,v) = { r with queryFilter = v }
                    override x.Update(r,f) = { r with queryFilter = f r.queryFilter }
                }
            let filteredFeatures =
                { new Lens<PRo3D.Minerva.Model, Aardvark.Base.plist<PRo3D.Minerva.Feature>>() with
                    override x.Get(r) = r.filteredFeatures
                    override x.Set(r,v) = { r with filteredFeatures = v }
                    override x.Update(r,f) = { r with filteredFeatures = f r.filteredFeatures }
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
            let featureProperties =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.FeatureProperties>() with
                    override x.Get(r) = r.featureProperties
                    override x.Set(r,v) = { r with featureProperties = v }
                    override x.Update(r,f) = { r with featureProperties = f r.featureProperties }
                }
            let selection =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.SelectionModel>() with
                    override x.Get(r) = r.selection
                    override x.Set(r,v) = { r with selection = v }
                    override x.Update(r,f) = { r with selection = f r.selection }
                }
            let solLabels =
                { new Lens<PRo3D.Minerva.Model, Aardvark.Base.hmap<System.String,Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.solLabels
                    override x.Set(r,v) = { r with solLabels = v }
                    override x.Update(r,f) = { r with solLabels = f r.solLabels }
                }
            let sgFeatures =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.SgFeatures>() with
                    override x.Get(r) = r.sgFeatures
                    override x.Set(r,v) = { r with sgFeatures = v }
                    override x.Update(r,f) = { r with sgFeatures = f r.sgFeatures }
                }
            let selectedSgFeatures =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.SgFeatures>() with
                    override x.Get(r) = r.selectedSgFeatures
                    override x.Set(r,v) = { r with selectedSgFeatures = v }
                    override x.Update(r,f) = { r with selectedSgFeatures = f r.selectedSgFeatures }
                }
            let picking =
                { new Lens<PRo3D.Minerva.Model, System.Boolean>() with
                    override x.Get(r) = r.picking
                    override x.Set(r,v) = { r with picking = v }
                    override x.Update(r,f) = { r with picking = f r.picking }
                }
