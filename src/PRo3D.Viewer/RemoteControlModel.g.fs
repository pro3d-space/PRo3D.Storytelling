namespace RemoteControlModel

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open RemoteControlModel

[<AutoOpen>]
module Mutable =

    
    
    type MRemoteModel(__initial : RemoteControlModel.RemoteModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<RemoteControlModel.RemoteModel> = Aardvark.Base.Incremental.EqModRef<RemoteControlModel.RemoteModel>(__initial) :> Aardvark.Base.Incremental.IModRef<RemoteControlModel.RemoteModel>
        let _selectedShot = MOption.Create(__initial.selectedShot)
        let _shots = MList.Create(__initial.shots)
        let _platformShots = MList.Create(__initial.platformShots)
        let _Rover = PRo3d.Mutable.MRoverModel.Create(__initial.Rover)
        
        member x.selectedShot = _selectedShot :> IMod<_>
        member x.shots = _shots :> alist<_>
        member x.platformShots = _platformShots :> alist<_>
        member x.Rover = _Rover
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : RemoteControlModel.RemoteModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MOption.Update(_selectedShot, v.selectedShot)
                MList.Update(_shots, v.shots)
                MList.Update(_platformShots, v.platformShots)
                PRo3d.Mutable.MRoverModel.Update(_Rover, v.Rover)
                
        
        static member Create(__initial : RemoteControlModel.RemoteModel) : MRemoteModel = MRemoteModel(__initial)
        static member Update(m : MRemoteModel, v : RemoteControlModel.RemoteModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<RemoteControlModel.RemoteModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RemoteModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let selectedShot =
                { new Lens<RemoteControlModel.RemoteModel, Microsoft.FSharp.Core.Option<RemoteControlModel.Shot>>() with
                    override x.Get(r) = r.selectedShot
                    override x.Set(r,v) = { r with selectedShot = v }
                    override x.Update(r,f) = { r with selectedShot = f r.selectedShot }
                }
            let shots =
                { new Lens<RemoteControlModel.RemoteModel, Aardvark.Base.plist<RemoteControlModel.Shot>>() with
                    override x.Get(r) = r.shots
                    override x.Set(r,v) = { r with shots = v }
                    override x.Update(r,f) = { r with shots = f r.shots }
                }
            let platformShots =
                { new Lens<RemoteControlModel.RemoteModel, Aardvark.Base.plist<RemoteControlModel.PlatformShot>>() with
                    override x.Get(r) = r.platformShots
                    override x.Set(r,v) = { r with platformShots = v }
                    override x.Update(r,f) = { r with platformShots = f r.platformShots }
                }
            let Rover =
                { new Lens<RemoteControlModel.RemoteModel, PRo3d.RoverModel>() with
                    override x.Get(r) = r.Rover
                    override x.Set(r,v) = { r with Rover = v }
                    override x.Update(r,f) = { r with Rover = f r.Rover }
                }
