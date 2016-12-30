
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

open PRo3DGui

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
    
    use splashScreen = SplashScreen.spawn()

    Aardvark.Base.Ag.initialize()
    Aardvark.Init()

    use glApp  = new OpenGlApplication()
    use ctrl   = new Aardvark.Application.WinForms.RenderControl()
    let glCtrl = new Aardvark.Application.WinForms.OpenGlRenderControl(glApp.Runtime)
    ctrl.Implementation <- glCtrl


    let loader = 
        async {
            return Aardvark.Opc.Viewer.createSceneGraph ctrl (Aardvark.Opc.Configurations.Network.mola())
        } |> Mod.async

    let sg = 
        adaptive {
            let! loadedScene = loader
            match loadedScene with
                | Some l -> 
                    return l
                | None ->   
                    return Sg.group' []
        } |> Sg.dynamic

    ctrl.RenderTask <- 
        RenderTask.ofList [
            glApp.Runtime.CompileClear(ctrl.FramebufferSignature, Mod.constant C4f.Gray)
            glApp.Runtime.CompileRender(ctrl.FramebufferSignature,sg)
        ]


    use w = new Form()
    let desiredSize = V2i(1280,960)
    let mutable initialized = false
    w.Width <- desiredSize.X
    w.Height <- desiredSize.Y

    let onRenderControlRectChanged (c : Fablish.JavascriptInterop.ClientRect) =
        let change () = 
            ctrl.set_Location(Drawing.Point(int c.left, int c.top))
            ctrl.Size <- Drawing.Size(int c.width,int c.height)
            if not initialized then
                w.FormBorderStyle <- FormBorderStyle.Sizable
                w.Size <- Drawing.Size(desiredSize.X,desiredSize.Y)
                splashScreen.Close()
                initialized <- true

        ctrl.Invoke(Action(change)) |> ignore

    let app = PRo3DGui.FullLayoutApp.app
    let browser = Chromium.runControl "8083" (app onRenderControlRectChanged)


    let style = w.FormBorderStyle
    w.Load.Add(fun _ -> w.Size <- Drawing.Size(0,0))
    w.FormBorderStyle <- FormBorderStyle.None

    use panel = new Panel()
    panel.Controls.Add browser
    panel.Size <- Drawing.Size(200,200)
    panel.Dock <- DockStyle.Fill

    w.Controls.Add ctrl
    w.Controls.Add panel
    w.Width <- desiredSize.X
    w.Height <- desiredSize.Y
    ctrl.BringToFront()

    Application.Run(w) 

    0