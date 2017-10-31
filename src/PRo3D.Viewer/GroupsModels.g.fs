namespace PRo3D.Groups

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Groups

[<AutoOpen>]
module Mutable =

    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MChild() =
        abstract member TryUpdate : PRo3D.Groups.Child -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : PRo3D.Groups.Child) = 
            match __model with
                | Annotations(value) -> MAnnotations(__model, value) :> MChild
                | Surfaces(value) -> MSurfaces(__model, value) :> MChild
        
        static member Create(v : PRo3D.Groups.Child) =
            ResetMod.Create(MChild.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MChild>, v : PRo3D.Groups.Child) =
            let m = unbox<ResetMod<MChild>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MChild.CreateValue v)
    
    and private MAnnotations(__initial : PRo3D.Groups.Child, value : PRo3D.Annotation) =
        inherit MChild()
        
        let mutable __current = __initial
        let _value = PRo3D.Mutable.MAnnotation.Create(value)
        member x.value = _value
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : PRo3D.Groups.Child) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Annotations(value) -> 
                        __current <- __model
                        PRo3D.Mutable.MAnnotation.Update(_value, value)
                        true
                    | _ -> false
    
    and private MSurfaces(__initial : PRo3D.Groups.Child, value : PRo3D.Surfaces.Surface) =
        inherit MChild()
        
        let mutable __current = __initial
        let _value = PRo3D.Surfaces.Mutable.MSurface.Create(value)
        member x.value = _value
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : PRo3D.Groups.Child) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Surfaces(value) -> 
                        __current <- __model
                        PRo3D.Surfaces.Mutable.MSurface.Update(_value, value)
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MChildPatterns =
        let (|MAnnotations|MSurfaces|) (m : MChild) =
            match m with
            | :? MAnnotations as v -> MAnnotations(v.value)
            | :? MSurfaces as v -> MSurfaces(v.value)
            | _ -> failwith "impossible"
    
    
    
    
    
    
    type MAnnotationGroup(__initial : PRo3D.Groups.AnnotationGroup) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroup> = Aardvark.Base.Incremental.EqModRef<PRo3D.Groups.AnnotationGroup>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroup>
        let _key = ResetMod.Create(__initial.key)
        let _name = ResetMod.Create(__initial.name)
        let _childList = MList.Create(__initial.childList, (fun v -> MChild.Create(v)), (fun (m,v) -> MChild.Update(m, v)), (fun v -> v))
        let _subGroups = MList.Create(__initial.subGroups, (fun v -> MAnnotationGroup.Create(v)), (fun (m,v) -> MAnnotationGroup.Update(m, v)), (fun v -> v))
        let _visible = ResetMod.Create(__initial.visible)
        let _expanded = ResetMod.Create(__initial.expanded)
        
        member x.key = _key :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.childList = _childList :> alist<_>
        member x.subGroups = _subGroups :> alist<_>
        member x.visible = _visible :> IMod<_>
        member x.expanded = _expanded :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Groups.AnnotationGroup) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_key,v.key)
                ResetMod.Update(_name,v.name)
                MList.Update(_childList, v.childList)
                MList.Update(_subGroups, v.subGroups)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_expanded,v.expanded)
                
        
        static member Create(__initial : PRo3D.Groups.AnnotationGroup) : MAnnotationGroup = MAnnotationGroup(__initial)
        static member Update(m : MAnnotationGroup, v : PRo3D.Groups.AnnotationGroup) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Groups.AnnotationGroup> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationGroup =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let key =
                { new Lens<PRo3D.Groups.AnnotationGroup, System.Guid>() with
                    override x.Get(r) = r.key
                    override x.Set(r,v) = { r with key = v }
                    override x.Update(r,f) = { r with key = f r.key }
                }
            let name =
                { new Lens<PRo3D.Groups.AnnotationGroup, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let childList =
                { new Lens<PRo3D.Groups.AnnotationGroup, Aardvark.Base.plist<PRo3D.Groups.Child>>() with
                    override x.Get(r) = r.childList
                    override x.Set(r,v) = { r with childList = v }
                    override x.Update(r,f) = { r with childList = f r.childList }
                }
            let subGroups =
                { new Lens<PRo3D.Groups.AnnotationGroup, Aardvark.Base.plist<PRo3D.Groups.AnnotationGroup>>() with
                    override x.Get(r) = r.subGroups
                    override x.Set(r,v) = { r with subGroups = v }
                    override x.Update(r,f) = { r with subGroups = f r.subGroups }
                }
            let visible =
                { new Lens<PRo3D.Groups.AnnotationGroup, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let expanded =
                { new Lens<PRo3D.Groups.AnnotationGroup, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.expanded
                    override x.Set(r,v) = { r with expanded = v }
                    override x.Update(r,f) = { r with expanded = f r.expanded }
                }
    
    
    type MAnnotationGroupsModel(__initial : PRo3D.Groups.AnnotationGroupsModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroupsModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Groups.AnnotationGroupsModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroupsModel>
        let _rootGroup = MAnnotationGroup.Create(__initial.rootGroup)
        let _activeGroupKey = ResetMod.Create(__initial.activeGroupKey)
        let _activeGroupPath = ResetMod.Create(__initial.activeGroupPath)
        let _activeGroupName = ResetMod.Create(__initial.activeGroupName)
        let _activeChildKey = ResetMod.Create(__initial.activeChildKey)
        let _activeChildPath = ResetMod.Create(__initial.activeChildPath)
        let _annotationsFlat = MSet.Create(unbox, __initial.annotationsFlat, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _lastSelectedItem = ResetMod.Create(__initial.lastSelectedItem)
        
        member x.rootGroup = _rootGroup
        member x.activeGroupKey = _activeGroupKey :> IMod<_>
        member x.activeGroupPath = _activeGroupPath :> IMod<_>
        member x.activeGroupName = _activeGroupName :> IMod<_>
        member x.activeChildKey = _activeChildKey :> IMod<_>
        member x.activeChildPath = _activeChildPath :> IMod<_>
        member x.annotationsFlat = _annotationsFlat :> aset<_>
        member x.lastSelectedItem = _lastSelectedItem :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Groups.AnnotationGroupsModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MAnnotationGroup.Update(_rootGroup, v.rootGroup)
                ResetMod.Update(_activeGroupKey,v.activeGroupKey)
                ResetMod.Update(_activeGroupPath,v.activeGroupPath)
                ResetMod.Update(_activeGroupName,v.activeGroupName)
                ResetMod.Update(_activeChildKey,v.activeChildKey)
                ResetMod.Update(_activeChildPath,v.activeChildPath)
                MSet.Update(_annotationsFlat, v.annotationsFlat)
                ResetMod.Update(_lastSelectedItem,v.lastSelectedItem)
                
        
        static member Create(__initial : PRo3D.Groups.AnnotationGroupsModel) : MAnnotationGroupsModel = MAnnotationGroupsModel(__initial)
        static member Update(m : MAnnotationGroupsModel, v : PRo3D.Groups.AnnotationGroupsModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Groups.AnnotationGroupsModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationGroupsModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rootGroup =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, PRo3D.Groups.AnnotationGroup>() with
                    override x.Get(r) = r.rootGroup
                    override x.Set(r,v) = { r with rootGroup = v }
                    override x.Update(r,f) = { r with rootGroup = f r.rootGroup }
                }
            let activeGroupKey =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, System.Guid>() with
                    override x.Get(r) = r.activeGroupKey
                    override x.Set(r,v) = { r with activeGroupKey = v }
                    override x.Update(r,f) = { r with activeGroupKey = f r.activeGroupKey }
                }
            let activeGroupPath =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, Microsoft.FSharp.Collections.list<Aardvark.Base.Index>>() with
                    override x.Get(r) = r.activeGroupPath
                    override x.Set(r,v) = { r with activeGroupPath = v }
                    override x.Update(r,f) = { r with activeGroupPath = f r.activeGroupPath }
                }
            let activeGroupName =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.activeGroupName
                    override x.Set(r,v) = { r with activeGroupName = v }
                    override x.Update(r,f) = { r with activeGroupName = f r.activeGroupName }
                }
            let activeChildKey =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, System.Guid>() with
                    override x.Get(r) = r.activeChildKey
                    override x.Set(r,v) = { r with activeChildKey = v }
                    override x.Update(r,f) = { r with activeChildKey = f r.activeChildKey }
                }
            let activeChildPath =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, Microsoft.FSharp.Collections.list<Aardvark.Base.Index>>() with
                    override x.Get(r) = r.activeChildPath
                    override x.Set(r,v) = { r with activeChildPath = v }
                    override x.Update(r,f) = { r with activeChildPath = f r.activeChildPath }
                }
            let annotationsFlat =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, Aardvark.Base.hset<PRo3D.Annotation>>() with
                    override x.Get(r) = r.annotationsFlat
                    override x.Set(r,v) = { r with annotationsFlat = v }
                    override x.Update(r,f) = { r with annotationsFlat = f r.annotationsFlat }
                }
            let lastSelectedItem =
                { new Lens<PRo3D.Groups.AnnotationGroupsModel, PRo3D.Groups.SelectedItem>() with
                    override x.Get(r) = r.lastSelectedItem
                    override x.Set(r,v) = { r with lastSelectedItem = v }
                    override x.Update(r,f) = { r with lastSelectedItem = f r.lastSelectedItem }
                }
    
    
    type MAnnotationGroupsImporterModel(__initial : PRo3D.Groups.AnnotationGroupsImporterModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroupsImporterModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Groups.AnnotationGroupsImporterModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroupsImporterModel>
        let _rootGroupI = MList.Create(__initial.rootGroupI, (fun v -> MAnnotationGroup.Create(v)), (fun (m,v) -> MAnnotationGroup.Update(m, v)), (fun v -> v))
        
        member x.rootGroupI = _rootGroupI :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Groups.AnnotationGroupsImporterModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_rootGroupI, v.rootGroupI)
                
        
        static member Create(__initial : PRo3D.Groups.AnnotationGroupsImporterModel) : MAnnotationGroupsImporterModel = MAnnotationGroupsImporterModel(__initial)
        static member Update(m : MAnnotationGroupsImporterModel, v : PRo3D.Groups.AnnotationGroupsImporterModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Groups.AnnotationGroupsImporterModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationGroupsImporterModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rootGroupI =
                { new Lens<PRo3D.Groups.AnnotationGroupsImporterModel, Aardvark.Base.plist<PRo3D.Groups.AnnotationGroup>>() with
                    override x.Get(r) = r.rootGroupI
                    override x.Set(r,v) = { r with rootGroupI = v }
                    override x.Update(r,f) = { r with rootGroupI = f r.rootGroupI }
                }
