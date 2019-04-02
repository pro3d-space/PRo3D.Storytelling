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
open Suave.Successful

open System.Collections.Concurrent
open System.Diagnostics
open System.Xml
open PRo3D.Surfaces.Mutable
open PRo3D.Surfaces
open RemoteControlModel
open PRo3D
open ViewPlanner

open Suave.Json
open System.Runtime.Serialization
open Aardvark.VRVis
open PRo3D.Viewer
open PRo3D.Groups
open System.Text
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis.Opc
open Aardvark.GeoSpatial.Opc

open Aardium
open System.Threading
open PRo3D.Base


[<DataContract>]
type Calc =
   { 
      [<field: DataMember(Name = "a")>]
      a : int;
      [<field: DataMember(Name = "b")>]
      b : int;
   }
 

 [<DataContract>]
type Result =
   { 
      [<field: DataMember(Name = "result")>]
      result : string;
   }

type EmbeddedRessource = EmbeddedRessource

type License = {
  key         : string
  userName    : string
  institution : string
}

let viewerVersion       = "2.9.5-prerelease1"
let mutable licenseText = ""
let catchDomainErrors   = false
let licenseCheck        = false
let doBuildLicense      = false
let licPath             = @".\license\pro3d.lic"

let buildLicense(time) = 
  
  let userName    = "Isa Barbarisi"  
  let institution = "ESAC"

  let mutable createAKey = new SKGL.Generate()
  createAKey.secretPhase <- (userName + institution + viewerVersion).GetHashCode().ToString()

  let lic = {
     key         = createAKey.doKey(time)
     userName    = userName
     institution = institution
  }
  
  lic |> Serialization.save licPath |> ignore
  Log.warn "Built license %A for time %d" lic time
  Environment.Exit(0)

let checkLicense () =
  let contact = "Please contact ortner@vrvis.at, VRVis Zentrum für Virtual Reality und Visualisierung Forschungs-GmbH, Vienna, Austria, www.vrvis.at"  

  if (not (System.IO.File.Exists(licPath))) then            
    MessageBox.Show("No licencefile found at " + licPath + ". \n" + contact) |> ignore
    Environment.Exit(0);
  else
    Report.Line("Found License File");

  let lic = Serialization.loadAs<License>(licPath)

  let mutable validateAKey = SKGL.Validate()
  validateAKey.secretPhase <- (lic.userName + lic.institution + viewerVersion).GetHashCode().ToString()
  validateAKey.Key         <- lic.key

  match (not validateAKey.IsValid || validateAKey.IsExpired) with
  | true ->
    MessageBox.Show("License file is invalid or expired. \n" + contact) |> ignore
    Environment.Exit(0)
  | false -> 
    licenseText <- " - " + lic.userName + ", " + lic.institution + ", " + validateAKey.DaysLeft.ToString() + " days left"



[<EntryPoint;STAThread>]
let main argv = 
    //Xilium.CefGlue.ChromiumUtilities.unpackCef()
    //Chromium.init argv        

    System.Threading.ThreadPool.SetMinThreads(12, 12) |> ignore

    Ag.initialize()
    Aardvark.Init()
    Aardium.init()        

    //use app = new VulkanApplication()
    use app = new OpenGlApplication()
    let runtime = app.Runtime    

    Aardvark.Rendering.GL.RuntimeConfig.SupressSparseBuffers <- true
    app.ShaderCachePath <- None

    if doBuildLicense then
      buildLicense(365)      
        
    Serialization.registry.RegisterFactory (fun _ -> SurfaceModel.surfaceModelPickler)
    Serialization.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)
    Serialization.registry.RegisterFactory (fun _ -> Surfaces.Init.incorePickler)
        
    if licenseCheck then
      checkLicense()
    
    Log.line "PRo3D Viewer - Version: %s; lic: %A" viewerVersion licenseCheck

   // use form = new Form(Width = 0, Height = 0)
    let signature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth,  { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

    use sendQueue = new BlockingCollection<string>()    
        
    let ws (webSocket : WebSocket) (context: HttpContext) =
      socket {
        let mutable loop = true
        
        while loop do
          let str = sendQueue.Take()
          Log.warn "taking item from bc"
          let s = ByteSegment(System.Text.Encoding.UTF8.GetBytes str)
          
          do! webSocket.send Text s true
      }        

    PatchLod.useAsyncLoading <- (argv |> Array.contains "-sync" |> not)
    let startEmpty = (argv |> Array.contains "-empty")

    // main app
    //use form = new Form(Width = 1280, Height = 800)
    let cts = new CancellationTokenSource()
    let messagingMailbox = MailboxProcessor.Start(Viewer.initMessageLoop cts, cts.Token)    

    let minervaMailbox = MailboxProcessor.Start(PRo3D.Minerva.App.messagingMailbox cts, cts.Token)

    let mainApp = 
        PRo3D.ViewerApp.start runtime signature startEmpty messagingMailbox minervaMailbox sendQueue

    let s = { MailboxState.empty with update = mainApp.update Guid.Empty }
    MailboxAction.InitMailboxState s |> messagingMailbox.Post
    
    //let s2 = { PRo3D.Minerva.MailboxState.empty with update = mainApp.update Guid.Empty }
    //PRo3D.Minerva.MailboxAction.InitMailboxState s2 |> minervaMailbox.Post
    
    CooTransformation.initCooTrafo () // should be a function not a value
    
    let domainError (sender:obj) (args:UnhandledExceptionEventArgs) =
        let e = args.ExceptionObject :?> Exception;
        Log.error "%A" e
        MessageBox.Show(e.Message, "Sorry for the inconvenience", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
        ()
    
    if catchDomainErrors then
      AppDomain.CurrentDomain.UnhandledException.AddHandler(UnhandledExceptionEventHandler(domainError))
    
    WebPart.startServerLocalhost 54321 [
        MutableApp.toWebPart' runtime false mainApp
        path "/websocket" >=> handShake ws
        Reflection.assemblyWebPart typeof<EmbeddedRessource>.Assembly
       // prefix "/instrument" >=> MutableApp.toWebPart runtime instrumentApp
        Suave.Files.browseHome        
    ]

    //WebPart.startServer 4322 [
    //    MutableApp.toWebPart' runtime false instrumentApp        
    //    Suave.Files.browseHome
    //]

    // screenshot app
    let send msg =
        match msg with
          | RemoteAction.SetCameraView cv ->
              mainApp.update Guid.Empty (ViewerAction.SetCamera cv |> Seq.singleton)
          | RemoteAction.SetView v ->                                
              Log.line "Setting View %A" v
              let frustum = Frustum.perspective v.fovH v.near v.far (float v.resolution.X / float v.resolution.Y)
              let frustum = { 
                  frustum with 
                      left   = frustum.left   - v.principalPoint.X
                      right  = frustum.right  - v.principalPoint.X
                      top    = frustum.top    - v.principalPoint.Y
                      bottom = frustum.bottom - v.principalPoint.Y
                  }
              mainApp.update Guid.Empty (ViewerAction.SetCameraAndFrustum2 (v.view,frustum) |> Seq.singleton)

    let remoteApp = 
        App.start (PRo3D.RemoteControlApp.app "http://localhost:54321" send)

    let takeScreenshot (shot:Shot) =   
        let act = CaptureShot shot |> Seq.singleton
        remoteApp.update Guid.Empty act
        { result = shot.folder }

    let takePlatformShot (shot:PlatformShot) =   
        let act = CapturePlatform shot |> Seq.singleton
        remoteApp.update Guid.Empty act
        { result = shot.folder }

    WebPart.startServerLocalhost 12345 [ 
        MutableApp.toWebPart runtime (remoteApp)
        POST >=> path "/shots" >=> mapJson takeScreenshot
        POST >=> path "/platformshots" >=> mapJson takePlatformShot
        Suave.Files.browseHome
    ] 

    let titlestr = "PRo3D Viewer - " + viewerVersion + " - VRVis Zentrum für Virtual Reality und Visualisierung Forschungs-GmbH" + licenseText

    Aardium.run {
        url "http://localhost:54321/"   //"http://localhost:4321/?page=main"
        width 1280
        height 800
        debug true
        title titlestr
    }

       
    //use ctrl = new AardvarkCefBrowser()
    //ctrl.Dock <- DockStyle.Fill
    //form.Controls.Add ctrl    
    //form.Text <- "PRo3D Viewer - " + viewerVersion + " - VRVis Zentrum für Virtual Reality und Visualisierung Forschungs-GmbH" + licenseText
    ////form.WindowState <- FormWindowState.Minimized
    ////form.Controls.Add (new Label()) |> ignore
    //ctrl.StartUrl <- "http://localhost:4321/?page=main" //"http://localhost:4321/?page=instrumentview"       
    ////ctrl.ShowDevTools()

    ////Config.shouldTimeUIUpdate <- true
    ////Config.shouldTimeJsCodeGeneration <- true
    ////Config.shouldTimeUnpersistCalls <- true


    //Application.Run form

    //let p = Process.Start("chrome.exe", "--app=http://localhost:4321/")

    //p.Exited.Add(fun _ -> form.Close())

    //Application.Run form
    ////Console.WriteLine("I bims der Aardvark Fallback Forms")
    ////Console.ReadLine() |> ignore
    //if p.HasExited |> not then p.Kill()
    ////sem.Wait()

    CooTransformation.deInitCooTrafo ()

    0