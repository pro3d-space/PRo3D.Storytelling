open System

open System.Windows.Forms

open Aardvark.Base
open Aardvark.Application.WinForms
open Aardvark.UI

open Suave
open Suave.WebPart
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Operators
open Suave.Filters

open System.Collections.Concurrent

[<EntryPoint;STAThread>]
let main argv = 
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Aardvark.UI.Chromium.init argv

    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()
    let runtime = app.Runtime    

    use form = new Form(Width = 1280, Height = 800)
    let signature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

    use bc = new BlockingCollection<string>()
        
    let ws (webSocket : WebSocket) (context: HttpContext) =
        socket {
            let mutable loop = true

            while loop do
                let str = bc.Take()
                Log.warn "taking item from bc"
                let s = ByteSegment(System.Text.Encoding.UTF8.GetBytes str)
                
                do! webSocket.send Text s true
        }

    let a = PRo3D.ViewerApp.start form runtime signature bc   


    WebPart.startServer 4321 [ 
        MutableApp.toWebPart runtime a
        path "/websocket" >=> handShake ws
        Suave.Files.browseHome
    ] 

    use ctrl = new AardvarkCefBrowser()
    ctrl.Dock <- DockStyle.Fill
    form.Controls.Add ctrl
    ctrl.StartUrl <- "http://localhost:4321/"

    ctrl.ShowDevTools()

    Application.Run form
    System.Environment.Exit 0
    0