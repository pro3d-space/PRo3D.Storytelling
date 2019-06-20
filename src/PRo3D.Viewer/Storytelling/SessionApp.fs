module SessionApp

open System

open Aardvark.Base
open Aardvark.UI
open Aardvark.Service

open PRo3D.Groups

open Model
open Session
open Provenance
open Provenance.Abstraction.Annotations
open Provenance.Abstraction.Surfaces
open Provenance.Abstraction.State
open Provenance.Abstraction.Message
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
    type Surfaces' = { 
        flat : hmap<Guid, Surface * string> 
    }

    type State' = {
        annotations : Annotations
        surfaces : Surfaces'
    }

    type Node' = {
        id : NodeId
        state : State'
        message : Option<Message>
    }

    type SessionModel = {
        view : View
        dockConfig : DockConfig
        provenance : Node' ztree
        story : Story
        bookmarks : Bookmarks
        renderControlSize : V2i
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Node =

        let create (model : Model) (node : Node) =
            let getSurfaceName (k : Guid) =
                let inner = Model.getCurrentModel model
                let surfaces = inner.scene.surfacesModel.surfaces

                let getLeafName = function
                    | Surfaces s -> Some s.name
                    | _ -> None

                surfaces.flat |> HMap.tryFind k
                              |> Option.bind getLeafName
                              |> Option.defaultValue ""

            let surf (s : Surfaces) =
                { flat = s.flat |> HMap.map (fun k v -> v, getSurfaceName k) }

            let state (s : State) =
                { annotations = s.annotations
                  surfaces = surf s.surfaces }

            { id = node.id
              state = state node.state
              message = node.message }

        let restore (model : Model) (node : Node') : Node =
            let findGuid (name : string) =
                let inner = Model.getCurrentModel model
                let surfaces = inner.scene.surfacesModel.surfaces

                let checkName = function
                    | Surfaces s -> s.name = name
                    | _ -> false

                surfaces.flat |> HMap.filter (fun _ s -> checkName s)
                              |> HMap.keys |> HSet.toList
                              |> List.tryHead

            let surf (s : Surfaces') : Surfaces =

                // Find surfaces with outdated GUIDs
                let outdated = s.flat |> HMap.map (fun _ (v, n) -> findGuid n, v)
                                      |> HMap.choose (fun _ (g, v)-> g |> Option.map (fun x -> x, v))

                // Remove the names
                let f = s.flat |> HMap.map (fun _ (v, _) -> v)

                // Remove outdated elements and add again with new GUIDs
                let removed =
                    outdated |> HMap.keys |> HSet.fold (fun x k -> x |> HMap.remove k) f

                let updated =
                    outdated |> HMap.values |> Seq.fold (fun x (k, v) -> x |> HMap.add k v) removed

                { flat = updated }

            let state (s : State') : State =
                { annotations = s.annotations
                  surfaces = surf s.surfaces }

            { id = node.id
              state = state node.state 
              message = node.message }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SessionModel =

        let create (model : Model) = {
            view = model.view
            dockConfig = model.dockConfig
            provenance = model.provenance.tree |> ZTree.map (Node.create model)
            story = model.story
            bookmarks = model.bookmarks
            renderControlSize = model.renderControlSize
        }

        let restore (current : Model) (model : SessionModel) =
            let m = {
                current with
                    dockConfig = model.dockConfig
                    provenance = {current.provenance with tree = model.provenance |> ZTree.map (Node.restore current) }
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