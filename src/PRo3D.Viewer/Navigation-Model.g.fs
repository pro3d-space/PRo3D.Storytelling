namespace PRo3D.Navigation2

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Navigation2

[<AutoOpen>]
module Mutable =

    
    
    type MNavigationModel(__initial : PRo3D.Navigation2.NavigationModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Navigation2.NavigationModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Navigation2.NavigationModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Navigation2.NavigationModel>
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _navigationMode = ResetMod.Create(__initial.navigationMode)
        let _exploreCenter = ResetMod.Create(__initial.exploreCenter)
        
        member x.camera = _camera
        member x.navigationMode = _navigationMode :> IMod<_>
        member x.exploreCenter = _exploreCenter :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Navigation2.NavigationModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                ResetMod.Update(_navigationMode,v.navigationMode)
                ResetMod.Update(_exploreCenter,v.exploreCenter)
                
        
        static member Create(__initial : PRo3D.Navigation2.NavigationModel) : MNavigationModel = MNavigationModel(__initial)
        static member Update(m : MNavigationModel, v : PRo3D.Navigation2.NavigationModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Navigation2.NavigationModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NavigationModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let camera =
                { new Lens<PRo3D.Navigation2.NavigationModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
            let navigationMode =
                { new Lens<PRo3D.Navigation2.NavigationModel, PRo3D.NavigationMode>() with
                    override x.Get(r) = r.navigationMode
                    override x.Set(r,v) = { r with navigationMode = v }
                    override x.Update(r,f) = { r with navigationMode = f r.navigationMode }
                }
            let exploreCenter =
                { new Lens<PRo3D.Navigation2.NavigationModel, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.exploreCenter
                    override x.Set(r,v) = { r with exploreCenter = v }
                    override x.Update(r,f) = { r with exploreCenter = f r.exploreCenter }
                }
