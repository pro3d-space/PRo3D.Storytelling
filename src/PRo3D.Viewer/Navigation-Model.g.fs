namespace PRo3D.Navigation2

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Navigation2

[<AutoOpen>]
module Mutable =

    
    
    type MNavigationModel(__initial : PRo3D.Navigation2.NavigationModel) =
        inherit obj()
        let mutable __current = __initial
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _navigationMode = ResetMod.Create(__initial.navigationMode)
        let _coordinates = PRo3D.ReferenceSystem.Mutable.MReferenceSystem.Create(__initial.coordinates)
        let _exploreCenter = ResetMod.Create(__initial.exploreCenter)
        let _config = PRo3D.Mutable.MViewConfigModel.Create(__initial.config)
        
        member x.camera = _camera
        member x.navigationMode = _navigationMode :> IMod<_>
        member x.coordinates = _coordinates
        member x.exploreCenter = _exploreCenter :> IMod<_>
        member x.config = _config
        
        member x.Update(v : PRo3D.Navigation2.NavigationModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                ResetMod.Update(_navigationMode,v.navigationMode)
                PRo3D.ReferenceSystem.Mutable.MReferenceSystem.Update(_coordinates, v.coordinates)
                ResetMod.Update(_exploreCenter,v.exploreCenter)
                PRo3D.Mutable.MViewConfigModel.Update(_config, v.config)
                
        
        static member Create(__initial : PRo3D.Navigation2.NavigationModel) : MNavigationModel = MNavigationModel(__initial)
        static member Update(m : MNavigationModel, v : PRo3D.Navigation2.NavigationModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
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
            let coordinates =
                { new Lens<PRo3D.Navigation2.NavigationModel, PRo3D.ReferenceSystem.ReferenceSystem>() with
                    override x.Get(r) = r.coordinates
                    override x.Set(r,v) = { r with coordinates = v }
                    override x.Update(r,f) = { r with coordinates = f r.coordinates }
                }
            let exploreCenter =
                { new Lens<PRo3D.Navigation2.NavigationModel, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.exploreCenter
                    override x.Set(r,v) = { r with exploreCenter = v }
                    override x.Update(r,f) = { r with exploreCenter = f r.exploreCenter }
                }
            let config =
                { new Lens<PRo3D.Navigation2.NavigationModel, PRo3D.ViewConfigModel>() with
                    override x.Get(r) = r.config
                    override x.Set(r,v) = { r with config = v }
                    override x.Update(r,f) = { r with config = f r.config }
                }
