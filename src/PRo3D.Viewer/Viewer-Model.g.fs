namespace PRo3D.Viewer

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Viewer

[<AutoOpen>]
module Mutable =

    
    
    type MScene(__initial : PRo3D.Viewer.Scene) =
        inherit obj()
        let mutable __current = __initial
        let _navigation = PRo3D.Navigation2.Mutable.MNavigationModel.Create(__initial.navigation)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _surfacesModel = PRo3D.Surfaces.Mutable.MSurfaceModel.Create(__initial.surfacesModel)
        let _config = PRo3D.Mutable.MViewConfigModel.Create(__initial.config)
        let _scenePath = MOption.Create(__initial.scenePath)
        let _referenceSystem = PRo3D.ReferenceSystem.Mutable.MReferenceSystem.Create(__initial.referenceSystem)
        
        member x.navigation = _navigation
        member x.interaction = _interaction :> IMod<_>
        member x.surfacesModel = _surfacesModel
        member x.config = _config
        member x.scenePath = _scenePath :> IMod<_>
        member x.referenceSystem = _referenceSystem
        
        member x.Update(v : PRo3D.Viewer.Scene) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                PRo3D.Navigation2.Mutable.MNavigationModel.Update(_navigation, v.navigation)
                ResetMod.Update(_interaction,v.interaction)
                PRo3D.Surfaces.Mutable.MSurfaceModel.Update(_surfacesModel, v.surfacesModel)
                PRo3D.Mutable.MViewConfigModel.Update(_config, v.config)
                MOption.Update(_scenePath, v.scenePath)
                PRo3D.ReferenceSystem.Mutable.MReferenceSystem.Update(_referenceSystem, v.referenceSystem)
                
        
        static member Create(__initial : PRo3D.Viewer.Scene) : MScene = MScene(__initial)
        static member Update(m : MScene, v : PRo3D.Viewer.Scene) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Viewer.Scene> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Scene =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let navigation =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.Navigation2.NavigationModel>() with
                    override x.Get(r) = r.navigation
                    override x.Set(r,v) = { r with navigation = v }
                    override x.Update(r,f) = { r with navigation = f r.navigation }
                }
            let interaction =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.InteractionMode>() with
                    override x.Get(r) = r.interaction
                    override x.Set(r,v) = { r with interaction = v }
                    override x.Update(r,f) = { r with interaction = f r.interaction }
                }
            let surfacesModel =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.Surfaces.SurfaceModel>() with
                    override x.Get(r) = r.surfacesModel
                    override x.Set(r,v) = { r with surfacesModel = v }
                    override x.Update(r,f) = { r with surfacesModel = f r.surfacesModel }
                }
            let config =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.ViewConfigModel>() with
                    override x.Get(r) = r.config
                    override x.Set(r,v) = { r with config = v }
                    override x.Update(r,f) = { r with config = f r.config }
                }
            let scenePath =
                { new Lens<PRo3D.Viewer.Scene, Microsoft.FSharp.Core.option<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.scenePath
                    override x.Set(r,v) = { r with scenePath = v }
                    override x.Update(r,f) = { r with scenePath = f r.scenePath }
                }
            let referenceSystem =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.ReferenceSystem.ReferenceSystem>() with
                    override x.Get(r) = r.referenceSystem
                    override x.Set(r,v) = { r with referenceSystem = v }
                    override x.Update(r,f) = { r with referenceSystem = f r.referenceSystem }
                }
    
    
    type MModel(__initial : PRo3D.Viewer.Model) =
        inherit obj()
        let mutable __current = __initial
        let _scene = MScene.Create(__initial.scene)
        let _drawing = PRo3D.Drawing.Mutable.MDrawingModel.Create(__initial.drawing)
        let _properties = ResetMod.Create(__initial.properties)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _picking = ResetMod.Create(__initial.picking)
        let _recent = ResetMod.Create(__initial.recent)
        let _past = ResetMod.Create(__initial.past)
        let _future = ResetMod.Create(__initial.future)
        
        member x.scene = _scene
        member x.drawing = _drawing
        member x.properties = _properties :> IMod<_>
        member x.interaction = _interaction :> IMod<_>
        member x.picking = _picking :> IMod<_>
        member x.recent = _recent :> IMod<_>
        member x.past = _past :> IMod<_>
        member x.future = _future :> IMod<_>
        
        member x.Update(v : PRo3D.Viewer.Model) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                MScene.Update(_scene, v.scene)
                PRo3D.Drawing.Mutable.MDrawingModel.Update(_drawing, v.drawing)
                ResetMod.Update(_properties,v.properties)
                ResetMod.Update(_interaction,v.interaction)
                ResetMod.Update(_picking,v.picking)
                ResetMod.Update(_recent,v.recent)
                _past.Update(v.past)
                _future.Update(v.future)
                
        
        static member Create(__initial : PRo3D.Viewer.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : PRo3D.Viewer.Model) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
        interface IUpdatable<PRo3D.Viewer.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let scene =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Scene>() with
                    override x.Get(r) = r.scene
                    override x.Set(r,v) = { r with scene = v }
                    override x.Update(r,f) = { r with scene = f r.scene }
                }
            let drawing =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Drawing.DrawingModel>() with
                    override x.Get(r) = r.drawing
                    override x.Set(r,v) = { r with drawing = v }
                    override x.Update(r,f) = { r with drawing = f r.drawing }
                }
            let properties =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Properties>() with
                    override x.Get(r) = r.properties
                    override x.Set(r,v) = { r with properties = v }
                    override x.Update(r,f) = { r with properties = f r.properties }
                }
            let interaction =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Interactions>() with
                    override x.Get(r) = r.interaction
                    override x.Set(r,v) = { r with interaction = v }
                    override x.Update(r,f) = { r with interaction = f r.interaction }
                }
            let picking =
                { new Lens<PRo3D.Viewer.Model, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.picking
                    override x.Set(r,v) = { r with picking = v }
                    override x.Update(r,f) = { r with picking = f r.picking }
                }
            let recent =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Recent>() with
                    override x.Get(r) = r.recent
                    override x.Set(r,v) = { r with recent = v }
                    override x.Update(r,f) = { r with recent = f r.recent }
                }
            let past =
                { new Lens<PRo3D.Viewer.Model, Microsoft.FSharp.Core.Option<PRo3D.Drawing.DrawingModel>>() with
                    override x.Get(r) = r.past
                    override x.Set(r,v) = { r with past = v }
                    override x.Update(r,f) = { r with past = f r.past }
                }
            let future =
                { new Lens<PRo3D.Viewer.Model, Microsoft.FSharp.Core.Option<PRo3D.Drawing.DrawingModel>>() with
                    override x.Get(r) = r.future
                    override x.Set(r,v) = { r with future = v }
                    override x.Update(r,f) = { r with future = f r.future }
                }
