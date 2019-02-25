namespace PRo3D.Correlations

open System

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open Aardvark.UI.Primitives

open UIPlus
open PRo3D
open PRo3D.Viewer
open CorrelationDrawing

module CorrelationPanelsApp =
  open PRo3D
  open PRo3D.Base

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
      
      //let cp = 
      //  CorrelationPlotApp.update' HMap.empty m.cpModel a 
      //    |> Lenses.set _selectedPoints (m.selectedPoints |> convertSelectedPoints)

      //{ m with cpModel = cp }
      
      m
    | SemanticAppMessage a ->
      let updSemApp = SemanticApp.update m.cpModel.semanticApp a
      let cp = { m.cpModel with semanticApp = updSemApp } |> Lenses.set _semantics updSemApp //TODO TO refactor double setting
      { m with cpModel = cp }
    | ColourMapMessage a ->
      let colors' = ColourMap.update m.cpModel.correlationPlot.colourMapApp a      
      let cp = m.cpModel |> Lenses.set _colourMapApp colors'
      { m with cpModel = cp }
    | AddSelectedPoint (id,p) ->
      Log.warn "log drawing: %A" p
      { m with selectedPoints = HMap.add id p m.selectedPoints }
   
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

  let viewSemantics (m : MCorrelationPanelsAppModel) =
    SemanticApp.View.expertGUI m.cpModel.semanticApp |> UI.map CorrelationPanelsMessage.SemanticAppMessage

  let viewWorkingLog (m : MCorrelationPanelsAppModel) =
    let points = 
      m.selectedPoints 
        |> AMap.toASet |> ASet.map snd |> ASet.toAList
        
    let points' = 
      points 
        |> AList.toMod 
        |> Mod.map (fun x -> x |> PList.sortBy (fun y -> CooTransformation.getAltitude y V3d.Zero Planet.Mars))
        |> AList.ofMod

    let linesSg = Sg.lines' points' (Mod.constant C4b.DarkMagenta) (Mod.constant 5.0)

    //CooTransformation.getAltitude x V3d.Zero Planet.Mars)
    
    points 
      |> AList.map(fun x -> Sg.mkDotISg (Mod.constant C4b.DarkMagenta) (Mod.constant 8.0) (Mod.constant x)) 
      |> AList.toASet 
      |> Sg.set
      |> Sg.noEvents
      |> Sg.effect [
            toEffect <| Shader.screenSpaceScale
            toEffect <| Shader.stableTrafo
            toEffect <| DefaultSurfaces.vertexColor          
        ]
      |> Sg.andAlso linesSg              


