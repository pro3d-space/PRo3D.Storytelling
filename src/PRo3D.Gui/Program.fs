open System
open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph
open Aardvark.Base.Incremental


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
            onRendered = OnRendered.ignore
        }

[<EntryPoint;STAThread>]
let main argv =
    ChromiumUtilities.unpackCef()
    Chromium.init argv

    Aardvark.Base.Ag.initialize()
    Aardvark.Init()

    use glApp  = new OpenGlApplication()
    use ctrl   = new Aardvark.Application.WinForms.RenderControl()
    let glCtrl = new Aardvark.Application.WinForms.OpenGlRenderControl(glApp.Runtime)
    ctrl.Implementation <- glCtrl

    let sg = Aardvark.Opc.Viewer.createSceneGraph ctrl (Aardvark.Opc.Configurations.Network.perjen())

    ctrl.RenderTask <- 
        RenderTask.ofList [
            glApp.Runtime.CompileClear(ctrl.FramebufferSignature, Mod.constant C4f.Gray)
            glApp.Runtime.CompileRender(ctrl.FramebufferSignature,sg)
        ]

    let onRenderControlRectChanged (c : Fablish.JavascriptInterop.ClientRect) =
        let change () = 
            ctrl.set_Location(Drawing.Point(int c.left, int c.top))
            ctrl.Size <- Drawing.Size(int c.width,int c.height)
        ctrl.Invoke(Action(change)) |> ignore

   // let app = TestApp.app
    let app = PRo3DGui.FullLayoutApp.app

    use w = new Form()
    let browser = Chromium.runControl "8083" (app onRenderControlRectChanged)

    use panel = new Panel()
    panel.Controls.Add browser
    panel.Size <- Drawing.Size(200,200)
    panel.Dock <- DockStyle.Fill

    w.Controls.Add ctrl
    w.Controls.Add panel
    w.Width <- 1280
    w.Height <- 960
    ctrl.BringToFront()

    Application.Run(w) 

    0