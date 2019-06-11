module BookmarkApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Model
open Provenance
open Provenance.Reduced
open Bookmarks
open Story
open View

[<AutoOpen>]
module private Helpers =

    // Updates the model based on the id of a bookmark and the given function; if the bookmark
    // does not exist, the identity function is applied instead
    let updateById (f : Index -> Bookmark -> Model -> Model) (id : BookmarkId) (model : Model) =
        match model.bookmarks.list |> PList.tryFindIndex (fun b -> b.id = id) with
            | None ->
                Log.error "Bookmark with id '%A' does not exist!" id
                model
            | Some i ->
                f i (PList.get i model.bookmarks.list) model

    // Restores the state and view from a bookmark
    let restore (animate : bool) (b : Bookmark) (model : Model) =

        let p = model.provenance |> ProvenanceApp.update model.story (Provenance.Goto b.node.id)
        let m = State.restore model.inner.current <| Provenance.state p

        model |> Lens.set Model.Lens.provenance p
              |> Lens.set (Model.Lens.inner |. InnerModel.Lens.current) m
              |> Model.setViewParams b.view
              |> ViewApp.update (if animate then Move else Set)

let init : Bookmarks = {
    list = PList.empty
}

let update (msg : BookmarkAction) (model : Model) =
    let lensList = Model.Lens.bookmarks |. Bookmarks.Lens.list

    match msg with
        | Add ->
            let n = PList.count model.bookmarks.list
            let t = sprintf "Bookmark #%d" (n + 1)
            let b = Bookmark.create t (Provenance.current model.provenance) model.view.state
            model |> Lens.update lensList (PList.append b)

        | Remove id ->
            model |> updateById (fun i _ m ->
                m |> Lens.update lensList (PList.remove i)
            ) id

        | Update id ->
            model |> updateById (fun i b m ->
                let b = { b with view = model.view.state }
                m |> Lens.update lensList (PList.set i b)
            ) id

        | SetTitle (id, s) ->
            model |> updateById (fun i b m ->
                let b = { b with title = s }
                m |> Lens.update lensList (PList.set i b)
            ) id

        | Goto id ->
            model |> updateById (fun _ b m ->
                m |> restore true b
            ) id

        | MouseEnter id ->
            model |> updateById (fun _ b m ->
                m |> Lens.update Model.Lens.provenance (ProvenanceApp.update m.story <| SetHighlight b.node.id)
            ) id

        | MouseLeave ->
            model |> Lens.update Model.Lens.provenance (ProvenanceApp.update model.story RemoveHighlight)

        | CreateStory ->
            let slides =
                model.bookmarks.list |> PList.map (fun b ->
                    let a = AnnotationApp.init |> AnnotationApp.update (Annotations.Add b.title)

                    { id = SlideId.generate()
                      content = FrameContent (b.node, b.view, a)
                      thumbnail = ThumbnailApp.empty }
                )

            let story = { StoryApp.init with slides = slides 
                                             showAnnotations = true }

            model |> Lens.set (Model.Lens.story) story

let view (model : MModel) =

    let disableClickPropagation =
        onBoot "$('#__ID__').on('click', function(e) { e.stopPropagation(); } ); "

    let bookmark (b : MBookmark) =

        let selected = adaptive {
            let! id = b.node.id
            let! t = model.provenance.tree
            return id = t.Value.id
        }

        let highlighted = adaptive {
            let! hovered = model.provenance.hovered
            let! id = b.node.id

            return hovered |> Option.map (fun t -> id = t.Value.id)
                           |> Option.defaultValue false
        }

        let viewChanged = Mod.map2 (<>) b.view.Current model.view.state.Current

        Incremental.div (AttributeMap.ofAMap <| amap {
            let! id = b.id
            let! selected = selected
            let! highlighted = highlighted

            yield clazz <| "bookmark item"  + if selected then " selected" else ""
                                            + if highlighted then " highlighted" else ""
            yield onMouseDoubleClick (fun _ -> Goto id)
            yield onMouseEnter (fun _ -> MouseEnter id)
            yield onMouseLeave (fun _ -> MouseLeave)

        }) <| AList.ofList [
            i [clazz "large bookmark aligned-vertical inverted icon"] []

            div [clazz "content"] [
                a [clazz "ui fluid inverted transparent input"] [
                    disableClickPropagation (
                        Incremental.input (AttributeMap.ofAMap <| amap {
                            let! id = b.id
                            let! value = b.title

                            yield attribute "type" "text"
                            yield attribute "value" value
                            yield attribute "placeholder" "Add name or description..."
                            yield onChange (fun s -> SetTitle (id, s))
                        })
                    )
                ]

                div [clazz "description"] [
                    b.timestamp |> Mod.map (fun d -> d.ToString "g") |> Incremental.text
                ]

                Incremental.div (AttributeMap.ofList [clazz "actions"]) <| alist {
                    let! id = b.id
                    let! selected = selected
                    let! viewChanged = viewChanged

                    if selected && viewChanged then
                        yield disableClickPropagation (
                            i [
                                clazz "large clickable sync alternate icon"
                                onClick (fun _ -> Update id)
                            ] []
                        )

                    yield disableClickPropagation (
                        i [
                            clazz "large clickable remove icon"
                            onClick (fun _ -> Remove id)
                        ] []
                    )
                }
            ]
        ]

    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "bookmarksStyle"; url = "Bookmarks.css" }
    ]

    require (dependencies) (
        div [clazz "bookmarks"] [
            Incremental.div (AttributeMap.ofList [clazz "ui inverted divided list"]) <| alist {
                for b in model.bookmarks.list do
                    yield bookmark b

                yield div [
                    clazz "bookmark item add"
                    onClick (fun _ -> Add)
                ] [
                    i [clazz "large aligned-horizontal inverted add icon"] []
                ]
            }

            div [clazz "controls"] [
                button [
                    clazz "ui basic inverted button"
                    onClick (fun _ -> CreateStory)
                ] [
                    i [clazz "film icon"][]
                    text "Create story"
                ]
            ]
        ]
    )