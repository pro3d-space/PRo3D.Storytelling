namespace PRo3D.Viewer

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Viewer

[<AutoOpen>]
module Mutable =

    
    
    type MScene(__initial : PRo3D.Viewer.Scene) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.Scene> = Aardvark.Base.Incremental.EqModRef<PRo3D.Viewer.Scene>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.Scene>
        let _navigation = PRo3D.Navigation2.Mutable.MNavigationModel.Create(__initial.navigation)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _surfacesModel = PRo3D.Groups.Mutable.MSurfaceModel.Create(__initial.surfacesModel)
        let _config = PRo3D.Mutable.MViewConfigModel.Create(__initial.config)
        let _scenePath = MOption.Create(__initial.scenePath)
        let _referenceSystem = PRo3D.ReferenceSystem.Mutable.MReferenceSystem.Create(__initial.referenceSystem)
        let _bookmarksModel = PRo3D.Groups.Mutable.MBookmarksModel.Create(__initial.bookmarksModel)
        let _viewPlans = PRo3d.Mutable.MViewPlanModel.Create(__initial.viewPlans)
        let _dockConfig = ResetMod.Create(__initial.dockConfig)
        let _closedPages = ResetMod.Create(__initial.closedPages)
        let _firstImport = ResetMod.Create(__initial.firstImport)
        let _userFeedback = ResetMod.Create(__initial.userFeedback)
        let _feedbackThreads = ResetMod.Create(__initial.feedbackThreads)
        
        member x.navigation = _navigation
        member x.interaction = _interaction :> IMod<_>
        member x.surfacesModel = _surfacesModel
        member x.config = _config
        member x.scenePath = _scenePath :> IMod<_>
        member x.referenceSystem = _referenceSystem
        member x.bookmarksModel = _bookmarksModel
        member x.viewPlans = _viewPlans
        member x.dockConfig = _dockConfig :> IMod<_>
        member x.closedPages = _closedPages :> IMod<_>
        member x.firstImport = _firstImport :> IMod<_>
        member x.userFeedback = _userFeedback :> IMod<_>
        member x.feedbackThreads = _feedbackThreads :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Viewer.Scene) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                PRo3D.Navigation2.Mutable.MNavigationModel.Update(_navigation, v.navigation)
                ResetMod.Update(_interaction,v.interaction)
                PRo3D.Groups.Mutable.MSurfaceModel.Update(_surfacesModel, v.surfacesModel)
                PRo3D.Mutable.MViewConfigModel.Update(_config, v.config)
                MOption.Update(_scenePath, v.scenePath)
                PRo3D.ReferenceSystem.Mutable.MReferenceSystem.Update(_referenceSystem, v.referenceSystem)
                PRo3D.Groups.Mutable.MBookmarksModel.Update(_bookmarksModel, v.bookmarksModel)
                PRo3d.Mutable.MViewPlanModel.Update(_viewPlans, v.viewPlans)
                ResetMod.Update(_dockConfig,v.dockConfig)
                ResetMod.Update(_closedPages,v.closedPages)
                ResetMod.Update(_firstImport,v.firstImport)
                ResetMod.Update(_userFeedback,v.userFeedback)
                ResetMod.Update(_feedbackThreads,v.feedbackThreads)
                
        
        static member Create(__initial : PRo3D.Viewer.Scene) : MScene = MScene(__initial)
        static member Update(m : MScene, v : PRo3D.Viewer.Scene) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
                { new Lens<PRo3D.Viewer.Scene, PRo3D.Groups.SurfaceModel>() with
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
                { new Lens<PRo3D.Viewer.Scene, Microsoft.FSharp.Core.Option<System.String>>() with
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
            let bookmarksModel =
                { new Lens<PRo3D.Viewer.Scene, PRo3D.Groups.BookmarksModel>() with
                    override x.Get(r) = r.bookmarksModel
                    override x.Set(r,v) = { r with bookmarksModel = v }
                    override x.Update(r,f) = { r with bookmarksModel = f r.bookmarksModel }
                }
            let viewPlans =
                { new Lens<PRo3D.Viewer.Scene, PRo3d.ViewPlanModel>() with
                    override x.Get(r) = r.viewPlans
                    override x.Set(r,v) = { r with viewPlans = v }
                    override x.Update(r,f) = { r with viewPlans = f r.viewPlans }
                }
            let dockConfig =
                { new Lens<PRo3D.Viewer.Scene, Aardvark.UI.Primitives.DockConfig>() with
                    override x.Get(r) = r.dockConfig
                    override x.Set(r,v) = { r with dockConfig = v }
                    override x.Update(r,f) = { r with dockConfig = f r.dockConfig }
                }
            let closedPages =
                { new Lens<PRo3D.Viewer.Scene, Microsoft.FSharp.Collections.List<Aardvark.UI.Primitives.DockElement>>() with
                    override x.Get(r) = r.closedPages
                    override x.Set(r,v) = { r with closedPages = v }
                    override x.Update(r,f) = { r with closedPages = f r.closedPages }
                }
            let firstImport =
                { new Lens<PRo3D.Viewer.Scene, System.Boolean>() with
                    override x.Get(r) = r.firstImport
                    override x.Set(r,v) = { r with firstImport = v }
                    override x.Update(r,f) = { r with firstImport = f r.firstImport }
                }
            let userFeedback =
                { new Lens<PRo3D.Viewer.Scene, System.String>() with
                    override x.Get(r) = r.userFeedback
                    override x.Set(r,v) = { r with userFeedback = v }
                    override x.Update(r,f) = { r with userFeedback = f r.userFeedback }
                }
            let feedbackThreads =
                { new Lens<PRo3D.Viewer.Scene, Aardvark.Base.Incremental.ThreadPool<PRo3D.Viewer.ViewerAction>>() with
                    override x.Get(r) = r.feedbackThreads
                    override x.Set(r,v) = { r with feedbackThreads = v }
                    override x.Update(r,f) = { r with feedbackThreads = f r.feedbackThreads }
                }
    
    
    type MSceneHandle(__initial : PRo3D.Viewer.SceneHandle) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.SceneHandle> = Aardvark.Base.Incremental.EqModRef<PRo3D.Viewer.SceneHandle>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.SceneHandle>
        let _path = ResetMod.Create(__initial.path)
        let _name = ResetMod.Create(__initial.name)
        let _writeDate = ResetMod.Create(__initial.writeDate)
        
        member x.path = _path :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.writeDate = _writeDate :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Viewer.SceneHandle) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_path,v.path)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_writeDate,v.writeDate)
                
        
        static member Create(__initial : PRo3D.Viewer.SceneHandle) : MSceneHandle = MSceneHandle(__initial)
        static member Update(m : MSceneHandle, v : PRo3D.Viewer.SceneHandle) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Viewer.SceneHandle> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SceneHandle =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let path =
                { new Lens<PRo3D.Viewer.SceneHandle, System.String>() with
                    override x.Get(r) = r.path
                    override x.Set(r,v) = { r with path = v }
                    override x.Update(r,f) = { r with path = f r.path }
                }
            let name =
                { new Lens<PRo3D.Viewer.SceneHandle, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let writeDate =
                { new Lens<PRo3D.Viewer.SceneHandle, System.DateTime>() with
                    override x.Get(r) = r.writeDate
                    override x.Set(r,v) = { r with writeDate = v }
                    override x.Update(r,f) = { r with writeDate = f r.writeDate }
                }
    
    
    type MRecent(__initial : PRo3D.Viewer.Recent) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.Recent> = Aardvark.Base.Incremental.EqModRef<PRo3D.Viewer.Recent>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.Recent>
        let _recentScenes = ResetMod.Create(__initial.recentScenes)
        
        member x.recentScenes = _recentScenes :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Viewer.Recent) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_recentScenes,v.recentScenes)
                
        
        static member Create(__initial : PRo3D.Viewer.Recent) : MRecent = MRecent(__initial)
        static member Update(m : MRecent, v : PRo3D.Viewer.Recent) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Viewer.Recent> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Recent =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let recentScenes =
                { new Lens<PRo3D.Viewer.Recent, Microsoft.FSharp.Collections.List<PRo3D.Viewer.SceneHandle>>() with
                    override x.Get(r) = r.recentScenes
                    override x.Set(r,v) = { r with recentScenes = v }
                    override x.Update(r,f) = { r with recentScenes = f r.recentScenes }
                }
    
    
    type MModel(__initial : PRo3D.Viewer.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.Model> = Aardvark.Base.Incremental.EqModRef<PRo3D.Viewer.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Viewer.Model>
        let _scene = MScene.Create(__initial.scene)
        let _drawing = PRo3D.Drawing.Mutable.MDrawingModel.Create(__initial.drawing)
        let _properties = ResetMod.Create(__initial.properties)
        let _interaction = ResetMod.Create(__initial.interaction)
        let _picking = ResetMod.Create(__initial.picking)
        let _ctrlFlag = ResetMod.Create(__initial.ctrlFlag)
        let _recent = MRecent.Create(__initial.recent)
        let _waypoints = MList.Create(__initial.waypoints)
        let _frustum = ResetMod.Create(__initial.frustum)
        let _overlayFrustum = ResetMod.Create(__initial.overlayFrustum)
        let _aspect = ResetMod.Create(__initial.aspect)
        let _trafoKind = ResetMod.Create(__initial.trafoKind)
        let _trafoMode = ResetMod.Create(__initial.trafoMode)
        let _alignment = PRo3d.Align.Mutable.MAlignmentModel.Create(__initial.alignment)
        let _tabMenu = ResetMod.Create(__initial.tabMenu)
        let _viewerMode = ResetMod.Create(__initial.viewerMode)
        let _animations = Aardvark.UI.Animation.Mutable.MAnimationModel.Create(__initial.animations)
        let _messagingMailbox = ResetMod.Create(__initial.messagingMailbox)
        let _mailboxState = ResetMod.Create(__initial.mailboxState)
        let _minervaModel = PRo3D.Minerva.Mutable.MModel.Create(__initial.minervaModel)
        let _past = ResetMod.Create(__initial.past)
        let _future = ResetMod.Create(__initial.future)
        let _footPrint = PRo3d.Mutable.MFootPrint.Create(__initial.footPrint)
        
        member x.scene = _scene
        member x.drawing = _drawing
        member x.properties = _properties :> IMod<_>
        member x.interaction = _interaction :> IMod<_>
        member x.picking = _picking :> IMod<_>
        member x.ctrlFlag = _ctrlFlag :> IMod<_>
        member x.recent = _recent
        member x.waypoints = _waypoints :> alist<_>
        member x.frustum = _frustum :> IMod<_>
        member x.overlayFrustum = _overlayFrustum :> IMod<_>
        member x.aspect = _aspect :> IMod<_>
        member x.trafoKind = _trafoKind :> IMod<_>
        member x.trafoMode = _trafoMode :> IMod<_>
        member x.alignment = _alignment
        member x.tabMenu = _tabMenu :> IMod<_>
        member x.viewerMode = _viewerMode :> IMod<_>
        member x.animations = _animations
        member x.messagingMailbox = _messagingMailbox :> IMod<_>
        member x.mailboxState = _mailboxState :> IMod<_>
        member x.minervaModel = _minervaModel
        member x.past = _past :> IMod<_>
        member x.future = _future :> IMod<_>
        member x.footPrint = _footPrint
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Viewer.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MScene.Update(_scene, v.scene)
                PRo3D.Drawing.Mutable.MDrawingModel.Update(_drawing, v.drawing)
                ResetMod.Update(_properties,v.properties)
                ResetMod.Update(_interaction,v.interaction)
                ResetMod.Update(_picking,v.picking)
                ResetMod.Update(_ctrlFlag,v.ctrlFlag)
                MRecent.Update(_recent, v.recent)
                MList.Update(_waypoints, v.waypoints)
                ResetMod.Update(_frustum,v.frustum)
                ResetMod.Update(_overlayFrustum,v.overlayFrustum)
                ResetMod.Update(_aspect,v.aspect)
                ResetMod.Update(_trafoKind,v.trafoKind)
                ResetMod.Update(_trafoMode,v.trafoMode)
                PRo3d.Align.Mutable.MAlignmentModel.Update(_alignment, v.alignment)
                ResetMod.Update(_tabMenu,v.tabMenu)
                ResetMod.Update(_viewerMode,v.viewerMode)
                Aardvark.UI.Animation.Mutable.MAnimationModel.Update(_animations, v.animations)
                ResetMod.Update(_messagingMailbox,v.messagingMailbox)
                ResetMod.Update(_mailboxState,v.mailboxState)
                PRo3D.Minerva.Mutable.MModel.Update(_minervaModel, v.minervaModel)
                _past.Update(v.past)
                _future.Update(v.future)
                PRo3d.Mutable.MFootPrint.Update(_footPrint, v.footPrint)
                
        
        static member Create(__initial : PRo3D.Viewer.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : PRo3D.Viewer.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
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
                { new Lens<PRo3D.Viewer.Model, System.Boolean>() with
                    override x.Get(r) = r.picking
                    override x.Set(r,v) = { r with picking = v }
                    override x.Update(r,f) = { r with picking = f r.picking }
                }
            let ctrlFlag =
                { new Lens<PRo3D.Viewer.Model, System.Boolean>() with
                    override x.Get(r) = r.ctrlFlag
                    override x.Set(r,v) = { r with ctrlFlag = v }
                    override x.Update(r,f) = { r with ctrlFlag = f r.ctrlFlag }
                }
            let recent =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.Recent>() with
                    override x.Get(r) = r.recent
                    override x.Set(r,v) = { r with recent = v }
                    override x.Update(r,f) = { r with recent = f r.recent }
                }
            let waypoints =
                { new Lens<PRo3D.Viewer.Model, Aardvark.Base.plist<PRo3D.Viewer.WayPoint>>() with
                    override x.Get(r) = r.waypoints
                    override x.Set(r,v) = { r with waypoints = v }
                    override x.Update(r,f) = { r with waypoints = f r.waypoints }
                }
            let frustum =
                { new Lens<PRo3D.Viewer.Model, Aardvark.Base.Frustum>() with
                    override x.Get(r) = r.frustum
                    override x.Set(r,v) = { r with frustum = v }
                    override x.Update(r,f) = { r with frustum = f r.frustum }
                }
            let overlayFrustum =
                { new Lens<PRo3D.Viewer.Model, Aardvark.Base.Frustum>() with
                    override x.Get(r) = r.overlayFrustum
                    override x.Set(r,v) = { r with overlayFrustum = v }
                    override x.Update(r,f) = { r with overlayFrustum = f r.overlayFrustum }
                }
            let aspect =
                { new Lens<PRo3D.Viewer.Model, System.Double>() with
                    override x.Get(r) = r.aspect
                    override x.Set(r,v) = { r with aspect = v }
                    override x.Update(r,f) = { r with aspect = f r.aspect }
                }
            let trafoKind =
                { new Lens<PRo3D.Viewer.Model, Aardvark.UI.Trafos.TrafoKind>() with
                    override x.Get(r) = r.trafoKind
                    override x.Set(r,v) = { r with trafoKind = v }
                    override x.Update(r,f) = { r with trafoKind = f r.trafoKind }
                }
            let trafoMode =
                { new Lens<PRo3D.Viewer.Model, Aardvark.UI.Trafos.TrafoMode>() with
                    override x.Get(r) = r.trafoMode
                    override x.Set(r,v) = { r with trafoMode = v }
                    override x.Update(r,f) = { r with trafoMode = f r.trafoMode }
                }
            let alignment =
                { new Lens<PRo3D.Viewer.Model, PRo3d.Align.AlignmentModel>() with
                    override x.Get(r) = r.alignment
                    override x.Set(r,v) = { r with alignment = v }
                    override x.Update(r,f) = { r with alignment = f r.alignment }
                }
            let tabMenu =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.TabMenu>() with
                    override x.Get(r) = r.tabMenu
                    override x.Set(r,v) = { r with tabMenu = v }
                    override x.Update(r,f) = { r with tabMenu = f r.tabMenu }
                }
            let viewerMode =
                { new Lens<PRo3D.Viewer.Model, PRo3D.ViewerMode>() with
                    override x.Get(r) = r.viewerMode
                    override x.Set(r,v) = { r with viewerMode = v }
                    override x.Update(r,f) = { r with viewerMode = f r.viewerMode }
                }
            let animations =
                { new Lens<PRo3D.Viewer.Model, Aardvark.UI.Animation.AnimationModel>() with
                    override x.Get(r) = r.animations
                    override x.Set(r,v) = { r with animations = v }
                    override x.Update(r,f) = { r with animations = f r.animations }
                }
            let messagingMailbox =
                { new Lens<PRo3D.Viewer.Model, Microsoft.FSharp.Control.MailboxProcessor<PRo3D.Viewer.MailboxAction>>() with
                    override x.Get(r) = r.messagingMailbox
                    override x.Set(r,v) = { r with messagingMailbox = v }
                    override x.Update(r,f) = { r with messagingMailbox = f r.messagingMailbox }
                }
            let mailboxState =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Viewer.MailboxState>() with
                    override x.Get(r) = r.mailboxState
                    override x.Set(r,v) = { r with mailboxState = v }
                    override x.Update(r,f) = { r with mailboxState = f r.mailboxState }
                }
            let minervaModel =
                { new Lens<PRo3D.Viewer.Model, PRo3D.Minerva.Model>() with
                    override x.Get(r) = r.minervaModel
                    override x.Set(r,v) = { r with minervaModel = v }
                    override x.Update(r,f) = { r with minervaModel = f r.minervaModel }
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
            let footPrint =
                { new Lens<PRo3D.Viewer.Model, PRo3d.FootPrint>() with
                    override x.Get(r) = r.footPrint
                    override x.Set(r,v) = { r with footPrint = v }
                    override x.Update(r,f) = { r with footPrint = f r.footPrint }
                }
