open System
open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open System.Windows.Forms

module TestApp =

    type Model = int

    type Action = Inc | Dec

    let update (m : Model) (a : Action) =
        match a with
            | Inc -> m + 1
            | Dec -> m - 1

    let view (m : Model) : DomNode<Action> =
        div [] [
            text (sprintf "current content: %d" m)
            br []
            button [onMouseClick (fun dontCare -> Inc)] [text "increment"]
            button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
        ]

    let app =
        {
            initial = 0
            update = update 
            view = view
        }

[<EntryPoint;STAThread>]
let main argv =
    ChromiumUtilities.unpackCef()
    Chromium.init argv

    let app = TestApp.app

    let browser = Chromium.runControl "8083" app
    use w = new Form()
    w.Controls.Add browser
    w.Width <- 800
    w.Height <- 600
    Application.Run(w) 

    0