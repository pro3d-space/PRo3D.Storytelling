namespace RemoteControlModel

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open RemoteControlModel

[<AutoOpen>]
module Mutable =

    
    
    type MModel(__initial : RemoteControlModel.Model) =
        inherit obj()
        let mutable __current = __initial
        let _viewPoints = ResetMod.Create(__initial.viewPoints)
        
        member x.viewPoints = _viewPoints :> IMod<_>
        
        member x.Update(v : RemoteControlModel.Model) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_viewPoints,v.viewPoints)
                
        
        static member Create(__initial : RemoteControlModel.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : RemoteControlModel.Model) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<RemoteControlModel.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let viewPoints =
                { new Lens<RemoteControlModel.Model, Microsoft.FSharp.Collections.list<Aardvark.Base.CameraView>>() with
                    override x.Get(r) = r.viewPoints
                    override x.Set(r,v) = { r with viewPoints = v }
                    override x.Update(r,f) = { r with viewPoints = f r.viewPoints }
                }
