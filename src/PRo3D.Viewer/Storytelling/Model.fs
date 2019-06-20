namespace Model

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI.Primitives

open Provenance
open Provenance.Abstraction.Camera
open Story
open View
open Preview
open Session
open Animation
open Bookmarks

type Action =
    | AppAction             of AppAction
    | ProvenanceAction      of ProvenanceAction
    | StoryAction           of StoryAction
    | UpdateConfig          of DockConfig
    | NodeClick             of NodeId
    | ViewAction            of ViewAction
    | PreviewAction         of PreviewAction
    | BookmarkAction        of BookmarkAction
    | SessionAction         of SessionAction
    | KeyDown               of Keys
    | KeyUp                 of Keys
    | RenderControlResized  of V2i

[<DomainType>]
type InnerModel = {
    current : AppModel          // The current state of the inner application
    preview : Preview option    // A preview or temporary state
    output : AppModel           // The state that is displayed (may be a preview)
}

[<DomainType>]
type Model = {
    inner : InnerModel
    view : View
    dockConfig : DockConfig
    provenance : Provenance
    story : Story
    bookmarks : Bookmarks
    renderControlSize : V2i
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AppModel =

    let setCamera (camera : CameraView) (model : AppModel) =
        let c = { model.scene.navigation.camera with view = CameraView.restore camera }
        { model with scene = { model.scene with navigation = { model.scene.navigation with camera = c } }}

    let getCamera (model : AppModel) =
        model.scene.navigation.camera.view |> CameraView.create

    let setRendering (rendering  : RenderingParams) (model : AppModel) =
        { model with scene = { model.scene with config = rendering } }

    let getRendering (model : AppModel) =
        model.scene.config

    let setPresentation (presentation : PresentationParams) (model : AppModel) =
        model |> setRendering presentation.rendering

    let getPresentation (model : AppModel) =
        { rendering = getRendering model }

    let setViewParams (view : ViewParams) (model : AppModel) =
        model |> setCamera view.camera
              |> setPresentation view.presentation

    let getViewParams (model : AppModel) =
        { camera = model |> getCamera
          presentation = model |> getPresentation }

    let setTargeting (enabled : bool) (model : AppModel) =
        let t = { model.scene.targeting with enabled = enabled }
        { model with scene = { model.scene with targeting = t } }

    let getTargeting (model : AppModel) =
        model.scene.targeting

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InnerModel =

    // Gets the active encapsulated model according to the given mode.
    // E.g. when a preview of a camera position is ongoing, the preview model is returned
    // when calling this function for mode = View
    let getActiveModel (mode : Mode) (model : InnerModel) =
        model.preview |> Option.filter (Preview.is mode)
                      |> Option.map (fun p -> p.model)
                      |> Option.defaultValue model.current

    let getCurrentModel (model : InnerModel) =
        model.current

    let setCamera (camera : CameraView) (model : InnerModel) =
        { model with current = model.current |> AppModel.setCamera camera }

    let getCamera (model : InnerModel) =
        model.current |> AppModel.getCamera

    let setRendering (rendering  : RenderingParams) (model : InnerModel) =
        { model with current = model.current |> AppModel.setRendering rendering }

    let getRendering (model : InnerModel) =
        model.current |> AppModel.getRendering

    let setPresentation (presentation : PresentationParams) (model : InnerModel) =
        { model with current = model.current |> AppModel.setPresentation presentation }

    let getPresentation (model : InnerModel) =
        model.current |> AppModel.getPresentation

    let setViewParams (view : ViewParams) (model : InnerModel) =
        { model with current = model.current |> AppModel.setViewParams view }

    let setActiveViewParams (view : ViewParams) (model : InnerModel) =
        match model.preview with
            | Some p when p |> Preview.is View ->
                { model with preview = Some { p with model = p.model |> AppModel.setViewParams view } }
            | _ ->
                { model with current = model.current |> AppModel.setViewParams view }

    let getViewParams (model : InnerModel) =
        model.current |> AppModel.getViewParams

    let getActiveViewParams (model : InnerModel) =
        model.preview |> Option.filter (Preview.is View)
                      |> Option.map (fun p -> p.model)
                      |> Option.defaultValue model.current
                      |> AppModel.getViewParams

    let getActivePresentation (model : InnerModel) =
        model |> getActiveViewParams
              |> fun v -> v.presentation

    let setTargeting (enabled : bool) (model : InnerModel) =
        { model with current = model.current |> AppModel.setTargeting enabled }

    let getTargeting (model : InnerModel) =
        model.current |> AppModel.getTargeting

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let getActiveModel (mode : Mode) (model : Model) =
        model.inner |> InnerModel.getActiveModel mode

    let getCurrentModel (model : Model) =
        model.inner |> InnerModel.getCurrentModel

    let setCamera (camera : CameraView) (model : Model) =
        { model with inner = model.inner |> InnerModel.setCamera camera }

    let getCamera (model : Model) =
        model.inner |> InnerModel.getCamera

    let setRendering (rendering  : RenderingParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setRendering rendering }

    let getRendering (model : Model) =
        model.inner |> InnerModel.getRendering

    let setPresentation (presentation : PresentationParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setPresentation presentation }

    let getPresentation (model : Model) =
        model.inner |> InnerModel.getPresentation

    let getActivePresentation (model : Model) =
        model.inner |> InnerModel.getActivePresentation

    let setViewParams (view : ViewParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setViewParams view }

    let setActiveViewParams (view : ViewParams) (model : Model) =
        { model with inner = model.inner |> InnerModel.setActiveViewParams view }

    let getViewParams (model : Model) =
        model.inner |> InnerModel.getViewParams

    let getActiveViewParams (model : Model) =
        model.inner |> InnerModel.getActiveViewParams

    let setTargeting (enabled : bool) (model : Model) =
        { model with inner = model.inner |> InnerModel.setTargeting enabled }

    let getTargeting (model : Model) =
        model.inner |> InnerModel.getTargeting

    let isAnimating (model : Model) =
        model.view.animation |> Animation.isAnimating

    let isPreview (model : Model) =
        model.inner.preview |> Option.isSome

    let isPreviewMode (mode : Mode) (model : Model) =
        model.inner.preview |> Option.exists (Preview.is mode)