module SessionApp

open System

open Aardvark.Base
open Aardvark.UI
open Aardvark.Service

open PRo3D.Groups

open Model
open Session
open Provenance
open Abstraction.Presentation
open Abstraction.Surfaces
open Abstraction.State
open Bookmarks
open Story
open View

[<AutoOpen>]
module private Helpers =
    
    open MBrace.FsPickler
    open Aardvark.UI.Primitives

    // We have to save the name of the surfaces since the GUID is not
    // persistent. Thus, after loading a session, the GUIDs have to be adjusted based
    // on the names. Obviously, a better solution would be to use hashes as identifiers instead
    // of GUIDs
    type SurfaceNames = hmap<Guid, string>

    type SessionModel = {
        view : View
        dockConfig : DockConfig
        provenance : Node ztree
        story : Story
        bookmarks : Bookmarks
        surfaceNames : SurfaceNames
        renderControlSize : V2i
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surfaces =

        let getNames (model : Model) : SurfaceNames =
            let inner = Model.getCurrentModel model
            let surfaces = inner.scene.surfacesModel.surfaces.flat |> Leaf.toSurfaces

            surfaces |> HMap.map (fun _ v -> v.name)

        let adjustGuids (model : Model) (session : SessionModel) (s : hmap<Guid, 'a>) =

            let getName (k : Guid) =
                session.surfaceNames |> HMap.tryFind k

            let findGuid (name : string) =
                model |> getNames
                      |> HMap.filter (fun _ x -> x = name)
                      |> HMap.keys |> HSet.toList
                      |> List.tryHead

            // Find surfaces with outdated GUIDs
            let outdated = s |> HMap.map (fun k v -> k |> getName |> Option.bind findGuid, v)
                             |> HMap.choose (fun _ (g, v)-> g |> Option.map (fun x -> x, v))

            // Remove outdated elements and add again with new GUIDs
            let removed =
                outdated |> HMap.keys |> HSet.fold (fun x k -> x |> HMap.remove k) s

            outdated |> HMap.values |> Seq.fold (fun x (k, v) -> x |> HMap.add k v) removed

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Node =

        let adjustSurfaceGuids (model : Model) (session : SessionModel) (node : Node) : Node =
            let l = Node.Lens.state |. State.Lens.surfaces |. Surfaces.Lens.flat
            node |> Lens.update l (Surfaces.adjustGuids model session)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module View =

        let adjustSurfaceGuids (model : Model) (session : SessionModel) (view : View) : View =
            let l = View.Lens.state |. ViewParams.Lens.presentation |. PresentationParams.Lens.surfaces
            view |> Lens.update l (Surfaces.adjustGuids model session)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Story =

        let adjustSurfaceGuids (model : Model) (session : SessionModel) (story : Story) : Story =

            let updateViewParams (v : ViewParams) =
                let l = ViewParams.Lens.presentation |. PresentationParams.Lens.surfaces
                v |> Lens.update l (Surfaces.adjustGuids model session)

            let updateContent = function
                | FrameContent (n, v, a) -> FrameContent (n |> Node.adjustSurfaceGuids model session, updateViewParams v, a)
                | x -> x

            let updateSlide (s : Slide) =
                s |> Lens.update Slide.Lens.content updateContent

            let updateSelected (s : Story) =
                match s.selected with
                    | None -> s
                    | Some x -> s |> Story.selectById (Some x.current.id)

            story |> Lens.update Story.Lens.slides (PList.map updateSlide)
                  |> updateSelected

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SessionModel =

        let create (model : Model) = {
            view = model.view
            dockConfig = model.dockConfig
            provenance = model.provenance.tree
            story = model.story
            bookmarks = model.bookmarks
            surfaceNames = Surfaces.getNames model
            renderControlSize = model.renderControlSize
        }

        let restore (current : Model) (model : SessionModel) =
            let m = {
                current with
                    dockConfig = model.dockConfig
                    provenance = {current.provenance with tree = model.provenance |> ZTree.map (Node.adjustSurfaceGuids current model) }
                    story = model.story                                           |> Story.adjustSurfaceGuids current model
                    bookmarks = model.bookmarks
                    renderControlSize = model.renderControlSize
                    view = model.view                                             |> View.adjustSurfaceGuids current model
            }

            m |> Lens.set (Model.Lens.inner |. InnerModel.Lens.current) (State.restore m.inner.current <| Provenance.state m.provenance)
              |> Model.setViewParams m.view.state

    // XML serializer
    let private xmlSerializer =
        FsPickler.CreateXmlSerializer ()

    let pickle (model : Model) =
        model |> SessionModel.create
              |> xmlSerializer.PickleToString

    let unpickle (current : Model) (data : string) : Model =
        data |> xmlSerializer.UnPickleOfString |> SessionModel.restore current

[<AutoOpen>]
module private Events =
    let onChooseFile (chosen : string option -> 'msg) =
        let cb xs =
            match xs with
                | x::[] when x <> null ->
                    x |> Pickler.unpickleOfJson |> List.map PathUtils.ofUnixStyle |> List.tryHead |> chosen
                | _ ->
                    chosen None
        onEvent "onchoose" [] cb

    let onSaveFile (chosen : string option -> 'msg) =
        let cb xs =
            match xs with
                | x::[] when x <> null ->
                    x |> Pickler.unpickleOfJson |> PathUtils.ofUnixStyle |> Some |> chosen
                | _ ->
                    chosen None
        onEvent "onsave" [] cb

let update (msg : SessionAction) (model : Model) =
    match msg with
        | Save (Some file) ->
            Log.startTimed "Saving session to '%s'... " file
            try
                File.writeAllText file <| pickle model
                Log.stop ()
            with
                | e -> Log.error "%A" e
            
            model

        | Load (Some file) ->
            Log.startTimed "Loading session from '%s'... " file
            try
                let model = file |> File.readAllText |> unpickle model
                Log.stop ()
                model
            with
                | e -> Log.error "%A" e
                       model

        | _ ->
            model

let view =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "menuStyle"; url = "Menu.css" }
    ]

    let initPopup =
        "$('.session.item').popup({ hoverable : true, variation : 'basic' });"

    let closePopup =
        "$('.session.item').popup('hide');"

    let openDialog =
        "aardvark.processEvent('__ID__', 'onchoose', aardvark.dialog.showOpenDialog({" +
            "properties: ['openFile']," + 
            "filters: [{ name: 'Session files', extensions: ['xml'] }]" +
            "}));"

    let saveDialog =
        "aardvark.processEvent('__ID__', 'onsave', aardvark.dialog.showSaveDialog({" +
            "filters: [{ name: 'Session files', extensions: ['xml'] }]" +
            "}));"

    require dependencies (
        onBoot initPopup (
            div [clazz "ui main menu"] [
                a [clazz "session item"] [
                    text "Session"
                    i [clazz "dropdown icon"] []
                ]

                div [clazz "ui popup"] [
                    div [clazz "ui vertical menu"] [
                        a [
                            clazz "item"
                            onChooseFile Load
                            clientEvent "onclick" closePopup
                            clientEvent "onclick" openDialog
                        ] [
                            text "Load"
                            i [clazz "open folder icon"] []
                        ]

                        a [
                            clazz "item"
                            onSaveFile Save
                            clientEvent "onclick" closePopup
                            clientEvent "onclick" saveDialog
                        ] [
                            text "Save"
                            i [clazz "save icon"] []
                        ]
                    ]
                ]
            ]
        )
    )