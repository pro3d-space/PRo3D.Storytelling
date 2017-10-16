namespace ConfigLensTest

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open ConfigLensTest

[<AutoOpen>]
module Mutable =

    
    
    type MBigConfig(__initial : ConfigLensTest.BigConfig) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.ModRef<ConfigLensTest.BigConfig> = Aardvark.Base.Incremental.Mod.init(__initial)
        let _arrowSize = ResetMod.Create(__initial.arrowSize)
        
        member x.arrowSize = _arrowSize :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : ConfigLensTest.BigConfig) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_arrowSize,v.arrowSize)
                
        
        static member Create(__initial : ConfigLensTest.BigConfig) : MBigConfig = MBigConfig(__initial)
        static member Update(m : MBigConfig, v : ConfigLensTest.BigConfig) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<ConfigLensTest.BigConfig> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BigConfig =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let arrowSize =
                { new Lens<ConfigLensTest.BigConfig, Microsoft.FSharp.Core.double>() with
                    override x.Get(r) = r.arrowSize
                    override x.Set(r,v) = { r with arrowSize = v }
                    override x.Update(r,f) = { r with arrowSize = f r.arrowSize }
                }
