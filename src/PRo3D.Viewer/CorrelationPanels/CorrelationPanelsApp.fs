namespace PRo3D.Correlations

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI
open Aardvark.UI.Primitives

open CorrelationDrawing
open UIPlus
open PRo3D.Viewer

module CorrelationPanelsApp =
  let _colourMapApp   = CorrelationPlotModel.Lens.correlationPlot |. CorrelationPlot.Lens.colourMapApp
  let _semantics      = CorrelationPlotModel.Lens.correlationPlot |. CorrelationPlot.Lens.semanticApp
  let _selectedPoints = CorrelationPlotModel.Lens.correlationPlot |. CorrelationPlot.Lens.selectedPoints
  
  let convertSelectedPoints (points : plist<LogPoint>) : hmap<AnnotationId, V3d> =
    HMap.empty

  let update (m : CorrelationPanelsAppModel) (msg : CorrelationPanelsMessage) : CorrelationPanelsAppModel =
    match msg with
    | CorrPlotMessage a -> 
      //TODO get selected poitns ...annotationId * v3d 
      //TODO get annotation list ...annotationId * annotation
      //m.correlationPlot.selectedPoints
      
      let cp = 
        CorrelationPlotApp.update' HMap.empty m.cpModel a 
          |> Lenses.set _selectedPoints (m.selectedPoints |> convertSelectedPoints)

      { m with cpModel = cp }
      
    | SemanticAppMessage a ->
      let updSemApp = SemanticApp.update m.cpModel.semanticApp a
      let cp = { m.cpModel with semanticApp = updSemApp } |> Lenses.set _semantics updSemApp //TODO TO refactor double setting
      { m with cpModel = cp }
    | ColourMapMessage a ->
      let colors' = ColourMap.update m.cpModel.correlationPlot.colourMapApp a      
      let cp = m.cpModel |> Lenses.set _colourMapApp colors'
      { m with cpModel = cp }
    | AddSelectedPoint (id,p) ->
      { m with selectedPoints = m.selectedPoints |> PList.prepend { annoId = id; position = p} }
   
  let view (m : MCorrelationPlotModel) : DomNode<CorrelationPanelsMessage> = div [] []

  let viewMappings (m : MCorrelationPlotModel) : DomNode<CorrelationPanelsMessage> =
    let domNode = 
      ColourMap.view m.correlationPlot.colourMapApp
        |> UI.map CorrelationPanelsMessage.ColourMapMessage  

    require (GUI.CSS.myCss) (
      body [style "overflow: auto"] [
        div [] [
          // menu |> ui.map correlationplotmessage
          div [clazz "ui inverted segment"] [ domNode ]
        ]
      ]
    )

  let viewSemantics (m : MCorrelationPlotModel) =
    SemanticApp.View.expertGUI m.semanticApp |> UI.map CorrelationPanelsMessage.SemanticAppMessage

  let viewWorkingLog (m : MCorrelationPlotModel) =
    Sg.empty
                
