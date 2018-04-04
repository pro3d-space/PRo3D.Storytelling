namespace PRo3D.ReferenceSystem

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.ReferenceSystem

[<AutoOpen>]
module Mutable =

    
    
    type MReferenceSystem(__initial : PRo3D.ReferenceSystem.ReferenceSystem) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.ReferenceSystem.ReferenceSystem> = Aardvark.Base.Incremental.EqModRef<PRo3D.ReferenceSystem.ReferenceSystem>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.ReferenceSystem.ReferenceSystem>
        let _origin = ResetMod.Create(__initial.origin)
        let _north = Aardvark.UI.Mutable.MV3dInput.Create(__initial.north)
        let _up = Aardvark.UI.Mutable.MV3dInput.Create(__initial.up)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _size = Aardvark.UI.Mutable.MNumericInput.Create(__initial.size)
        let _scaleChart = MList.Create(__initial.scaleChart)
        let _selectedScale = ResetMod.Create(__initial.selectedScale)
        let _planet = ResetMod.Create(__initial.planet)
        
        member x.origin = _origin :> IMod<_>
        member x.north = _north
        member x.up = _up
        member x.isVisible = _isVisible :> IMod<_>
        member x.size = _size
        member x.scaleChart = _scaleChart :> alist<_>
        member x.selectedScale = _selectedScale :> IMod<_>
        member x.planet = _planet :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.ReferenceSystem.ReferenceSystem) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_origin,v.origin)
                Aardvark.UI.Mutable.MV3dInput.Update(_north, v.north)
                Aardvark.UI.Mutable.MV3dInput.Update(_up, v.up)
                ResetMod.Update(_isVisible,v.isVisible)
                Aardvark.UI.Mutable.MNumericInput.Update(_size, v.size)
                MList.Update(_scaleChart, v.scaleChart)
                ResetMod.Update(_selectedScale,v.selectedScale)
                ResetMod.Update(_planet,v.planet)
                
        
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
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Aardvark.UI.V3dInput>() with
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
            let size =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
            let scaleChart =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Aardvark.Base.plist<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.scaleChart
                    override x.Set(r,v) = { r with scaleChart = v }
                    override x.Update(r,f) = { r with scaleChart = f r.scaleChart }
                }
            let selectedScale =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.selectedScale
                    override x.Set(r,v) = { r with selectedScale = v }
                    override x.Update(r,f) = { r with selectedScale = f r.selectedScale }
                }
            let planet =
                { new Lens<PRo3D.ReferenceSystem.ReferenceSystem, PRo3D.Planet>() with
                    override x.Get(r) = r.planet
                    override x.Set(r,v) = { r with planet = v }
                    override x.Update(r,f) = { r with planet = f r.planet }
                }
