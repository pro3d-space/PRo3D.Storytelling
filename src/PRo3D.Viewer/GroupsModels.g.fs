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
    
    
    
    
    
    
    type MGroup(__initial : PRo3D.Groups.Group) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Groups.Group> = Aardvark.Base.Incremental.EqModRef<PRo3D.Groups.Group>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Groups.Group>
        let _key = ResetMod.Create(__initial.key)
        let _name = ResetMod.Create(__initial.name)
        let _childList = MList.Create(__initial.childList, (fun v -> MChild.Create(v)), (fun (m,v) -> MChild.Update(m, v)), (fun v -> v))
        let _subGroups = MList.Create(__initial.subGroups, (fun v -> MGroup.Create(v)), (fun (m,v) -> MGroup.Update(m, v)), (fun v -> v))
        let _visible = ResetMod.Create(__initial.visible)
        let _expanded = ResetMod.Create(__initial.expanded)
        
        member x.key = _key :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.childList = _childList :> alist<_>
        member x.subGroups = _subGroups :> alist<_>
        member x.visible = _visible :> IMod<_>
        member x.expanded = _expanded :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Groups.Group) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_key,v.key)
                ResetMod.Update(_name,v.name)
                MList.Update(_childList, v.childList)
                MList.Update(_subGroups, v.subGroups)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_expanded,v.expanded)
                
        
        static member Create(__initial : PRo3D.Groups.Group) : MGroup = MGroup(__initial)
        static member Update(m : MGroup, v : PRo3D.Groups.Group) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Groups.Group> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Group =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let key =
                { new Lens<PRo3D.Groups.Group, System.Guid>() with
                    override x.Get(r) = r.key
                    override x.Set(r,v) = { r with key = v }
                    override x.Update(r,f) = { r with key = f r.key }
                }
            let name =
                { new Lens<PRo3D.Groups.Group, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let childList =
                { new Lens<PRo3D.Groups.Group, Aardvark.Base.plist<PRo3D.Groups.Child>>() with
                    override x.Get(r) = r.childList
                    override x.Set(r,v) = { r with childList = v }
                    override x.Update(r,f) = { r with childList = f r.childList }
                }
            let subGroups =
                { new Lens<PRo3D.Groups.Group, Aardvark.Base.plist<PRo3D.Groups.Group>>() with
                    override x.Get(r) = r.subGroups
                    override x.Set(r,v) = { r with subGroups = v }
                    override x.Update(r,f) = { r with subGroups = f r.subGroups }
                }
            let visible =
                { new Lens<PRo3D.Groups.Group, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let expanded =
                { new Lens<PRo3D.Groups.Group, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.expanded
                    override x.Set(r,v) = { r with expanded = v }
                    override x.Update(r,f) = { r with expanded = f r.expanded }
                }
    
    
    type MGroupsModel(__initial : PRo3D.Groups.GroupsModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Groups.GroupsModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Groups.GroupsModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Groups.GroupsModel>
        let _rootGroup = MGroup.Create(__initial.rootGroup)
        let _activeGroup = ResetMod.Create(__initial.activeGroup)
        let _activeChild = ResetMod.Create(__initial.activeChild)
        let _annotationsFlat = MSet.Create((fun (v : PRo3D.Annotation) -> v.key :> obj), __initial.annotationsFlat, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _lastSelectedItem = ResetMod.Create(__initial.lastSelectedItem)
        let _selectedChilds = MSet.Create(__initial.selectedChilds)
        let _lastSelectedChild = MOption.Create(__initial.lastSelectedChild, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.rootGroup = _rootGroup
        member x.activeGroup = _activeGroup :> IMod<_>
        member x.activeChild = _activeChild :> IMod<_>
        member x.annotationsFlat = _annotationsFlat :> aset<_>
        member x.lastSelectedItem = _lastSelectedItem :> IMod<_>
        member x.selectedChilds = _selectedChilds :> aset<_>
        member x.lastSelectedChild = _lastSelectedChild :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Groups.GroupsModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MGroup.Update(_rootGroup, v.rootGroup)
                ResetMod.Update(_activeGroup,v.activeGroup)
                ResetMod.Update(_activeChild,v.activeChild)
                MSet.Update(_annotationsFlat, v.annotationsFlat)
                ResetMod.Update(_lastSelectedItem,v.lastSelectedItem)
                MSet.Update(_selectedChilds, v.selectedChilds)
                MOption.Update(_lastSelectedChild, v.lastSelectedChild)
                
        
        static member Create(__initial : PRo3D.Groups.GroupsModel) : MGroupsModel = MGroupsModel(__initial)
        static member Update(m : MGroupsModel, v : PRo3D.Groups.GroupsModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Groups.GroupsModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GroupsModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rootGroup =
                { new Lens<PRo3D.Groups.GroupsModel, PRo3D.Groups.Group>() with
                    override x.Get(r) = r.rootGroup
                    override x.Set(r,v) = { r with rootGroup = v }
                    override x.Update(r,f) = { r with rootGroup = f r.rootGroup }
                }
            let activeGroup =
                { new Lens<PRo3D.Groups.GroupsModel, PRo3D.Groups.TreeSelection>() with
                    override x.Get(r) = r.activeGroup
                    override x.Set(r,v) = { r with activeGroup = v }
                    override x.Update(r,f) = { r with activeGroup = f r.activeGroup }
                }
            let activeChild =
                { new Lens<PRo3D.Groups.GroupsModel, PRo3D.Groups.TreeSelection>() with
                    override x.Get(r) = r.activeChild
                    override x.Set(r,v) = { r with activeChild = v }
                    override x.Update(r,f) = { r with activeChild = f r.activeChild }
                }
            let annotationsFlat =
                { new Lens<PRo3D.Groups.GroupsModel, Aardvark.Base.hset<PRo3D.Annotation>>() with
                    override x.Get(r) = r.annotationsFlat
                    override x.Set(r,v) = { r with annotationsFlat = v }
                    override x.Update(r,f) = { r with annotationsFlat = f r.annotationsFlat }
                }
            let lastSelectedItem =
                { new Lens<PRo3D.Groups.GroupsModel, PRo3D.Groups.SelectedItem>() with
                    override x.Get(r) = r.lastSelectedItem
                    override x.Set(r,v) = { r with lastSelectedItem = v }
                    override x.Update(r,f) = { r with lastSelectedItem = f r.lastSelectedItem }
                }
            let selectedChilds =
                { new Lens<PRo3D.Groups.GroupsModel, Aardvark.Base.hset<PRo3D.Groups.TreeSelection>>() with
                    override x.Get(r) = r.selectedChilds
                    override x.Set(r,v) = { r with selectedChilds = v }
                    override x.Update(r,f) = { r with selectedChilds = f r.selectedChilds }
                }
            let lastSelectedChild =
                { new Lens<PRo3D.Groups.GroupsModel, Microsoft.FSharp.Core.option<PRo3D.Annotation>>() with
                    override x.Get(r) = r.lastSelectedChild
                    override x.Set(r,v) = { r with lastSelectedChild = v }
                    override x.Update(r,f) = { r with lastSelectedChild = f r.lastSelectedChild }
                }
    
    
    type MAnnotationGroupsImporterModel(__initial : PRo3D.Groups.AnnotationGroupsImporterModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroupsImporterModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Groups.AnnotationGroupsImporterModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Groups.AnnotationGroupsImporterModel>
        let _rootGroupI = MList.Create(__initial.rootGroupI, (fun v -> MGroup.Create(v)), (fun (m,v) -> MGroup.Update(m, v)), (fun v -> v))
        
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
                { new Lens<PRo3D.Groups.AnnotationGroupsImporterModel, Aardvark.Base.plist<PRo3D.Groups.Group>>() with
                    override x.Get(r) = r.rootGroupI
                    override x.Set(r,v) = { r with rootGroupI = v }
                    override x.Update(r,f) = { r with rootGroupI = f r.rootGroupI }
                }
