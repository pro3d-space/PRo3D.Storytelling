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
        let _pick = ResetMod.Create(__initial.pick)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> PRo3D.Versioned.Mutable.MAnnotation'.Create(v)), (fun (m,v) -> PRo3D.Versioned.Mutable.MAnnotation'.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _geometry = ResetMod.Create(__initial.geometry)
        let _semantic = ResetMod.Create(__initial.semantic)
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        let _annotations = PRo3D.Groups.Mutable.MGroupsModel.Create(__initial.annotations)
        let _exportPath = MOption.Create(__initial.exportPath)
        let _pendingIntersections = ResetMod.Create(__initial.pendingIntersections)
        let _past = ResetMod.Create(__initial.past)
        let _future = ResetMod.Create(__initial.future)
        let _colorLegend = PRo3D.Mutable.MFalseColorsModel.Create(__initial.colorLegend)
        
        member x.draw = _draw :> IMod<_>
        member x.pick = _pick :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.geometry = _geometry :> IMod<_>
        member x.semantic = _semantic :> IMod<_>
        member x.color = _color
        member x.thickness = _thickness
        member x.annotations = _annotations
        member x.exportPath = _exportPath :> IMod<_>
        member x.pendingIntersections = _pendingIntersections :> IMod<_>
        member x.processedMessages = __current.Value.processedMessages
        member x.past = _past :> IMod<_>
        member x.future = _future :> IMod<_>
        member x.colorLegend = _colorLegend
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Drawing.DrawingModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_draw,v.draw)
                ResetMod.Update(_pick,v.pick)
                MOption.Update(_hoverPosition, v.hoverPosition)
                MOption.Update(_working, v.working)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_semantic,v.semantic)
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                Aardvark.UI.Mutable.MNumericInput.Update(_thickness, v.thickness)
                PRo3D.Groups.Mutable.MGroupsModel.Update(_annotations, v.annotations)
                MOption.Update(_exportPath, v.exportPath)
                ResetMod.Update(_pendingIntersections,v.pendingIntersections)
                _past.Update(v.past)
                _future.Update(v.future)
                PRo3D.Mutable.MFalseColorsModel.Update(_colorLegend, v.colorLegend)
                
        
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
                { new Lens<PRo3D.Drawing.DrawingModel, System.Boolean>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
            let pick =
                { new Lens<PRo3D.Drawing.DrawingModel, System.Boolean>() with
                    override x.Get(r) = r.pick
                    override x.Set(r,v) = { r with pick = v }
                    override x.Update(r,f) = { r with pick = f r.pick }
                }
            let hoverPosition =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.Option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
            let working =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.Option<PRo3D.Versioned.Annotation'>>() with
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
            let color =
                { new Lens<PRo3D.Drawing.DrawingModel, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let thickness =
                { new Lens<PRo3D.Drawing.DrawingModel, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.thickness
                    override x.Set(r,v) = { r with thickness = v }
                    override x.Update(r,f) = { r with thickness = f r.thickness }
                }
            let annotations =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.Groups.GroupsModel>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let exportPath =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Core.Option<System.String>>() with
                    override x.Get(r) = r.exportPath
                    override x.Set(r,v) = { r with exportPath = v }
                    override x.Update(r,f) = { r with exportPath = f r.exportPath }
                }
            let pendingIntersections =
                { new Lens<PRo3D.Drawing.DrawingModel, Aardvark.Base.Incremental.ThreadPool<PRo3D.Drawing.Action>>() with
                    override x.Get(r) = r.pendingIntersections
                    override x.Set(r,v) = { r with pendingIntersections = v }
                    override x.Update(r,f) = { r with pendingIntersections = f r.pendingIntersections }
                }
            let processedMessages =
                { new Lens<PRo3D.Drawing.DrawingModel, Microsoft.FSharp.Collections.List<PRo3D.Drawing.Action>>() with
                    override x.Get(r) = r.processedMessages
                    override x.Set(r,v) = { r with processedMessages = v }
                    override x.Update(r,f) = { r with processedMessages = f r.processedMessages }
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
            let colorLegend =
                { new Lens<PRo3D.Drawing.DrawingModel, PRo3D.FalseColorsModel>() with
                    override x.Get(r) = r.colorLegend
                    override x.Set(r,v) = { r with colorLegend = v }
                    override x.Update(r,f) = { r with colorLegend = f r.colorLegend }
                }
