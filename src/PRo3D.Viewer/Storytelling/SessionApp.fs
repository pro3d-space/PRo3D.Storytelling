module SessionApp

open Aardvark.Base
open Aardvark.UI
open Aardvark.Service

open Model
open Session
open Provenance
open Provenance.Reduced
open Bookmarks
open Story
open View

[<AutoOpen>]
module private Helpers =
    open MBrace.FsPickler
    open Aardvark.UI.Primitives

    type SessionModel = {
        view : View
        dockConfig : DockConfig
        provenance : Node ztree
        story : Story
        bookmarks : Bookmarks
        renderControlSize : V2i
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SessionModel =

        let create (model : Model) = {
            view = model.view
            dockConfig = model.dockConfig
            provenance = model.provenance.tree
            story = model.story
            bookmarks = model.bookmarks
            renderControlSize = model.renderControlSize
        }

        let restore (current : Model) (model : SessionModel) =
            let m = {
                current with
                    dockConfig = model.dockConfig
                    provenance = {current.provenance with tree = model.provenance }
                    story = model.story
                    bookmarks = model.bookmarks
                    renderControlSize = model.renderControlSize
                    view = model.view
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