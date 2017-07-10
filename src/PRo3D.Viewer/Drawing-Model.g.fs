namespace PRo3D.Drawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Drawing

[<AutoOpen>]
module Mutable =

    
    
    type MDrawingModel(__initial : PRo3D.Drawing.DrawingModel) =
        inherit obj()
        let mutable __current = __initial
        let _draw = ResetMod.Create(__initial.draw)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _semantic = ResetMod.Create(__initial.semantic)
        let _annotations = MList.Create(__initial.annotations, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _exportPath = ResetMod.Create(__initial.exportPath)
        let _selected = MSet.Create(__initial.selected)
        let _selectedAnnotation = MOption.Create(__initial.selectedAnnotation, (fun v -> PRo3D.Mutable.MAnnotation.Create(v)), (fun (m,v) -> PRo3D.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _pendingIntersections = ResetMod.Create(__initial.pendingIntersections)
        let _config = PRo3D.Mutable.MViewConfigModel.Create(__initial.config)
        let _coordinates = PRo3D.Mutable.MReferenceSystem.Create(__initial.coordinates)
        
        member x.draw = _draw :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.semantic = _semantic :> IMod<_>
        member x.annotations = _annotations :> alist<_>
        member x.exportPath = _exportPath :> IMod<_>
        member x.selected = _selected :> aset<_>
        member x.selectedAnnotation = _selectedAnnotation :> IMod<_>
        member x.pendingIntersections = _pendingIntersections :> IMod<_>
        member x.config = _config
        member x.coordinates = _coordinates
        
        member x.Update(v : PRo3D.Drawing.DrawingModel) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_draw,v.draw)
                MOption.Update(_hoverPosition, v.hoverPosition)
                MOption.Update(_working, v.working)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_semantic,v.semantic)
                MList.Update(_annotations, v.annotations)
                ResetMod.Update(_exportPath,v.exportPath)
                MSet.Update(_selected, v.selected)
                MOption.Update(_selectedAnnotation, v.selectedAnnotation)
                ResetMod.Update(_pendingIntersections,v.pendingIntersections)
                PRo3D.Mutable.MViewConfigModel.Update(_config, v.config)
                PRo3D.Mutable.MReferenceSystem.Update(_coordinates, v.coordinates)
                
        
        static member Create(__initial : PRo3D.Drawing.DrawingModel) : MDrawingModel = MDrawingModel(__initial)
        static member Update(m : MDrawingModel, v : PRo3D.Drawing.DrawingModel) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
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
                { new Lens<PRo3D.Drawing.DrawingModel, Aardvark.Base.plist<PRo3D.Annotation>>() with
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
            let config =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.ViewConfigModel>() with
                    override x.Get(r) = r.config
                    override x.Set(r,v) = { r with config = v }
                    override x.Update(r,f) = { r with config = f r.config }
                }
            let coordinates =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.ReferenceSystem>() with
                    override x.Get(r) = r.coordinates
                    override x.Set(r,v) = { r with coordinates = v }
                    override x.Update(r,f) = { r with coordinates = f r.coordinates }
                }
