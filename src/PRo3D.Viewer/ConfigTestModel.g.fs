namespace ConfigTest

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open ConfigTest

[<AutoOpen>]
module Mutable =

    
    
    type MBigConfig(__initial : ConfigTest.BigConfig) =
        inherit obj()
        let mutable __current = __initial
        let _arrowSize = ResetMod.Create(__initial.arrowSize)
        
        member x.arrowSize = _arrowSize :> IMod<_>
        
        member x.Update(v : ConfigTest.BigConfig) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_arrowSize,v.arrowSize)
                
        
        static member Create(__initial : ConfigTest.BigConfig) : MBigConfig = MBigConfig(__initial)
        static member Update(m : MBigConfig, v : ConfigTest.BigConfig) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<ConfigTest.BigConfig> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BigConfig =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let arrowSize =
                { new Lens<ConfigTest.BigConfig, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.arrowSize
                    override x.Set(r,v) = { r with arrowSize = v }
                    override x.Update(r,f) = { r with arrowSize = f r.arrowSize }
                }
    
    
    type MBigModel(__initial : ConfigTest.BigModel) =
        inherit obj()
        let mutable __current = __initial
        let _bigConfig = ResetMod.Create(__initial.bigConfig)
        
        member x.bigConfig = _bigConfig :> IMod<_>
        
        member x.Update(v : ConfigTest.BigModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                _bigConfig.Update(v.bigConfig)
                
        
        static member Create(__initial : ConfigTest.BigModel) : MBigModel = MBigModel(__initial)
        static member Update(m : MBigModel, v : ConfigTest.BigModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<ConfigTest.BigModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BigModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let bigConfig =
                { new Lens<ConfigTest.BigModel, ConfigTest.BigConfig>() with
                    override x.Get(r) = r.bigConfig
                    override x.Set(r,v) = { r with bigConfig = v }
                    override x.Update(r,f) = { r with bigConfig = f r.bigConfig }
                }
