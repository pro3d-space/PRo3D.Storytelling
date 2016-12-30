open System
open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Application
open Aardvark.Application.WinForms


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
            onRendered = Scripts.ignore
        }

[<EntryPoint;STAThread>]
let main argv =
    ChromiumUtilities.unpackCef()
    Chromium.init argv

    Aardvark.Base.Ag.initialize()
    Aardvark.Init()

    use glApp = new OpenGlApplication()
    use ctrl = new Aardvark.Application.WinForms.OpenGlRenderControl(glApp.Runtime)

   // let app = TestApp.app
    let app = PRo3DGui.FullLayoutApp.app

    use w = new Form()
    let browser = Chromium.runControl "8083" app

    use panel = new Panel()
    panel.Controls.Add browser
    panel.Size <- Drawing.Size(200,200)

    ctrl.Location <- Drawing.Point(150,150)
    ctrl.Width <- 100
    ctrl.Height <- 100
    ctrl.TabIndex <- 2

    w.Controls.Add ctrl
    w.Controls.Add panel
    w.Width <- 800
    w.Height <- 600
    ctrl.BringToFront()

    Application.Run(w) 

    0