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
  let _colourMapApp = CorrelationPlotModel.Lens.correlationPlot |. CorrelationPlot.Lens.colourMapApp

  let update (m : CorrelationPlotModel) (msg : CorrelationPanelsMessage) : CorrelationPlotModel =
    match msg with
    | CorrPlotMessage a -> 
      m
    | SemanticAppMessage a ->
      m
    | ColourMapMessage a ->
      let colors' = ColourMap.update m.correlationPlot.colourMapApp a
      m |> Lenses.set _colourMapApp colors'
   
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
                
