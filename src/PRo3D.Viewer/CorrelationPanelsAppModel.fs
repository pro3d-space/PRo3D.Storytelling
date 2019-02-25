namespace PRo3D.Correlations

open System
open Aardvark.Base
open CorrelationDrawing
open Aardvark.Base.Incremental

type LogPoint = {
  annoId   : Guid //PRo3D Annotation Id
  position : V3d
}

[<DomainType>]
type CorrelationPanelsAppModel = {
  selectedPoints : hmap<Guid, V3d>
  annotations    : hmap<AnnotationId, Annotation>
  cpModel        : CorrelationPlotModel
}

module CorrelationPanelsAppModel =
  let initial = {
    selectedPoints = HMap.empty
    annotations    = HMap.empty
    cpModel        = CorrelationPlotApp.initial
  }


//module CorrelationPanelsAppModel

