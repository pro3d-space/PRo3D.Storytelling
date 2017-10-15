namespace PRo3d

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3d

[<AutoOpen>]
module Mutable =

    
    
    type MRoverModel(__initial : PRo3d.RoverModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.RoverModel> = Aardvark.Base.Incremental.EqModRef<PRo3d.RoverModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.RoverModel>
        let _Rovers = MMap.Create(__initial.Rovers)
        
        member x.Rovers = _Rovers :> amap<_,_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.RoverModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_Rovers, v.Rovers)
                
        
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
            let Rovers =
                { new Lens<PRo3d.RoverModel, Aardvark.Base.hmap<Microsoft.FSharp.Core.string,PRo3d.Rover>>() with
                    override x.Get(r) = r.Rovers
                    override x.Set(r,v) = { r with Rovers = v }
                    override x.Update(r,f) = { r with Rovers = f r.Rovers }
                }
