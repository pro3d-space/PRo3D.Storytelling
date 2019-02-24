namespace PRo3D.Correlations

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Correlations

[<AutoOpen>]
module Mutable =

    
    
    type MCorrelationPanelsAppModel(__initial : PRo3D.Correlations.CorrelationPanelsAppModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Correlations.CorrelationPanelsAppModel> = Aardvark.Base.Incremental.EqModRef<PRo3D.Correlations.CorrelationPanelsAppModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Correlations.CorrelationPanelsAppModel>
        let _selectedPoints = MList.Create(__initial.selectedPoints)
        let _annotations = MMap.Create(__initial.annotations, (fun v -> CorrelationDrawing.Mutable.MAnnotation.Create(v)), (fun (m,v) -> CorrelationDrawing.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _cpModel = CorrelationDrawing.Mutable.MCorrelationPlotModel.Create(__initial.cpModel)
        
        member x.selectedPoints = _selectedPoints :> alist<_>
        member x.annotations = _annotations :> amap<_,_>
        member x.cpModel = _cpModel
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Correlations.CorrelationPanelsAppModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_selectedPoints, v.selectedPoints)
                MMap.Update(_annotations, v.annotations)
                CorrelationDrawing.Mutable.MCorrelationPlotModel.Update(_cpModel, v.cpModel)
                
        
        static member Create(__initial : PRo3D.Correlations.CorrelationPanelsAppModel) : MCorrelationPanelsAppModel = MCorrelationPanelsAppModel(__initial)
        static member Update(m : MCorrelationPanelsAppModel, v : PRo3D.Correlations.CorrelationPanelsAppModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Correlations.CorrelationPanelsAppModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationPanelsAppModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let selectedPoints =
                { new Lens<PRo3D.Correlations.CorrelationPanelsAppModel, Aardvark.Base.plist<PRo3D.Correlations.LogPoint>>() with
                    override x.Get(r) = r.selectedPoints
                    override x.Set(r,v) = { r with selectedPoints = v }
                    override x.Update(r,f) = { r with selectedPoints = f r.selectedPoints }
                }
            let annotations =
                { new Lens<PRo3D.Correlations.CorrelationPanelsAppModel, Aardvark.Base.hmap<CorrelationDrawing.AnnotationId,CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let cpModel =
                { new Lens<PRo3D.Correlations.CorrelationPanelsAppModel, CorrelationDrawing.CorrelationPlotModel>() with
                    override x.Get(r) = r.cpModel
                    override x.Set(r,v) = { r with cpModel = v }
                    override x.Update(r,f) = { r with cpModel = f r.cpModel }
                }
