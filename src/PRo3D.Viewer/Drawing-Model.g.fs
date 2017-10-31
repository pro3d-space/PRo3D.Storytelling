namespace PRo3D.Drawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Drawing

[<AutoOpen>]
module Mutable =

    
    
    type MDrawingModel(__initial : PRo3D.Drawing.DrawingModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Drawing.DrawingModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Drawing.DrawingModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Drawing.DrawingModel>
        let _draw = ResetMod.Create(__initial.draw)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _semantic = ResetMod.Create(__initial.semantic)
        let _annotations = PRo3D.Groups.Mutable.MAnnotationGroupsModel.Create(__initial.annotations)
        let _exportPath = ResetMod.Create(__initial.exportPath)
        let _selected = MSet.Create(__initial.selected)
        let _selectedAnnotation = MOption.Create(__initial.selectedAnnotation, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _pendingIntersections = ResetMod.Create(__initial.pendingIntersections)
        let _past = ResetMod.Create(__initial.past)
        let _future = ResetMod.Create(__initial.future)
        
        member x.draw = _draw :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.semantic = _semantic :> IMod<_>
        member x.annotations = _annotations
        member x.exportPath = _exportPath :> IMod<_>
        member x.selected = _selected :> aset<_>
        member x.selectedAnnotation = _selectedAnnotation :> IMod<_>
        member x.pendingIntersections = _pendingIntersections :> IMod<_>
        member x.past = _past :> IMod<_>
        member x.future = _future :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Drawing.DrawingModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_draw,v.draw)
                MOption.Update(_hoverPosition, v.hoverPosition)
                MOption.Update(_working, v.working)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_semantic,v.semantic)
                PRo3D.Groups.Mutable.MAnnotationGroupsModel.Update(_annotations, v.annotations)
                ResetMod.Update(_exportPath,v.exportPath)
                MSet.Update(_selected, v.selected)
                MOption.Update(_selectedAnnotation, v.selectedAnnotation)
                ResetMod.Update(_pendingIntersections,v.pendingIntersections)
                _past.Update(v.past)
                _future.Update(v.future)
                
        
        static member Create(__initial : PRo3D.Drawing.DrawingModel) : MDrawingModel = MDrawingModel(__initial)
        static member Update(m : MDrawingModel, v : PRo3D.Drawing.DrawingModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Drawing.DrawingModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DrawingModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let draw =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
            let hoverPosition =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
            let working =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.Option<PRo3D.Annotation>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let projection =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let geometry =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.Geometry>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let semantic =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.Semantic>() with
                    override x.Get(r) = r.semantic
                    override x.Set(r,v) = { r with semantic = v }
                    override x.Update(r,f) = { r with semantic = f r.semantic }
                }
            let annotations =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.Groups.AnnotationGroupsModel>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let exportPath =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.exportPath
                    override x.Set(r,v) = { r with exportPath = v }
                    override x.Update(r,f) = { r with exportPath = f r.exportPath }
                }
            let selected =
                { new Lens<PRo3D.Drawing.DrawingModel, Aardvark.Base.hset<System.Guid>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let selectedAnnotation =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.option<PRo3D.Annotation>>() with
                    override x.Get(r) = r.selectedAnnotation
                    override x.Set(r,v) = { r with selectedAnnotation = v }
                    override x.Update(r,f) = { r with selectedAnnotation = f r.selectedAnnotation }
                }
            let pendingIntersections =
                { new Lens<PRo3D.Drawing.DrawingModel, Aardvark.Base.ThreadPool<PRo3D.Drawing.Action>>() with
                    override x.Get(r) = r.pendingIntersections
                    override x.Set(r,v) = { r with pendingIntersections = v }
                    override x.Update(r,f) = { r with pendingIntersections = f r.pendingIntersections }
                }
            let past =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.Option<PRo3D.Drawing.DrawingModel>>() with
                    override x.Get(r) = r.past
                    override x.Set(r,v) = { r with past = v }
                    override x.Update(r,f) = { r with past = f r.past }
                }
            let future =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.Option<PRo3D.Drawing.DrawingModel>>() with
                    override x.Get(r) = r.future
                    override x.Set(r,v) = { r with future = v }
                    override x.Update(r,f) = { r with future = f r.future }
                }
