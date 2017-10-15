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
        let _shots = MList.Create(__initial.shots)
        let _rovers = MList.Create(__initial.rovers)
        let _selectedRover = MOption.Create(__initial.selectedRover)
        
        member x.shots = _shots :> alist<_>
        member x.rovers = _rovers :> alist<_>
        member x.selectedRover = _selectedRover :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : RemoteControlModel.RemoteModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_shots, v.shots)
                MList.Update(_rovers, v.rovers)
                MOption.Update(_selectedRover, v.selectedRover)
                
        
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
            let shots =
                { new Lens<RemoteControlModel.RemoteModel, Aardvark.Base.plist<RemoteControlModel.Shot>>() with
                    override x.Get(r) = r.shots
                    override x.Set(r,v) = { r with shots = v }
                    override x.Update(r,f) = { r with shots = f r.shots }
                }
            let rovers =
                { new Lens<RemoteControlModel.RemoteModel, Aardvark.Base.plist<PRo3d.Rover>>() with
                    override x.Get(r) = r.rovers
                    override x.Set(r,v) = { r with rovers = v }
                    override x.Update(r,f) = { r with rovers = f r.rovers }
                }
            let selectedRover =
                { new Lens<RemoteControlModel.RemoteModel, Microsoft.FSharp.Core.option<PRo3d.Rover>>() with
                    override x.Get(r) = r.selectedRover
                    override x.Set(r,v) = { r with selectedRover = v }
                    override x.Update(r,f) = { r with selectedRover = f r.selectedRover }
                }
