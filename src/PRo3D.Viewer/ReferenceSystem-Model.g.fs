namespace PRo3D.ReferenceSystem

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.ReferenceSystem

[<AutoOpen>]
module Mutable =

    
    
    type MReferenceSystem(__initial : PRo3D.ReferenceSystem.ReferenceSystem) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.ModRef<PRo3D.ReferenceSystem.ReferenceSystem> = Aardvark.Base.Incremental.Mod.init(__initial)
        let _origin = ResetMod.Create(__initial.origin)
        let _north = ResetMod.Create(__initial.north)
        let _up = Aardvark.UI.Mutable.MV3dInput.Create(__initial.up)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        
        member x.origin = _origin :> IMod<_>
        member x.north = _north :> IMod<_>
        member x.up = _up
        member x.isVisible = _isVisible :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.ReferenceSystem.ReferenceSystem) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_origin,v.origin)
                ResetMod.Update(_north,v.north)
                Aardvark.UI.Mutable.MV3dInput.Update(_up, v.up)
                ResetMod.Update(_isVisible,v.isVisible)
                
        
        static member Create(__initial : PRo3D.ReferenceSystem.ReferenceSystem) : MReferenceSystem = MReferenceSystem(__initial)
        static member Update(m : MReferenceSystem, v : PRo3D.ReferenceSystem.ReferenceSystem) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.ReferenceSystem.ReferenceSystem> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ReferenceSystem =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let origin =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.origin
                    override x.Set(r,v) = { r with origin = v }
                    override x.Update(r,f) = { r with origin = f r.origin }
                }
            let north =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.north
                    override x.Set(r,v) = { r with north = v }
                    override x.Update(r,f) = { r with north = f r.north }
                }
            let up =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Aardvark.UI.V3dInput>() with
                    override x.Get(r) = r.up
                    override x.Set(r,v) = { r with up = v }
                    override x.Update(r,f) = { r with up = f r.up }
                }
            let isVisible =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
