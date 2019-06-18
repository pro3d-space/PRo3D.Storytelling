open System
open Suave

open Aardvark
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardium
open Aardvark.Application.WinForms
open Aardvark.UI
open FSharp.Data
open FSharp.Data.JsonExtensions
open Aardvark.Application
open PRo3D.Minerva
open PRo3D.Minerva.Communication
open System.Threading

[<EntryPoint;STAThread>]
let main argv = 
  Ag.initialize()
  Aardvark.Init()
  Aardium.init() 

  use app = new OpenGlApplication()
  let runtime = app.Runtime    

  let cts = new CancellationTokenSource()

  let messagingMailbox = MailboxProcessor.Start(App.messagingMailbox cts, cts.Token) 

  let mainApp = PRo3D.Minerva.App.start messagingMailbox

  let s = { MailboxState.empty with update = mainApp.update Guid.Empty }
  MailboxAction.InitMailboxState s |> messagingMailbox.Post

  WebPart.startServerLocalhost 1776 [
        MutableApp.toWebPart' runtime false mainApp
      //  Reflection.assemblyWebPart typeof<EmbeddedResources>.Assembly
        Suave.Files.browseHome        
  ] |> ignore

  Aardium.run {
        url "http://localhost:1776/"   //"http://localhost:4321/?page=main"
        width  400
        height 800
        debug true
        title "Minerva 0.0.1"
    }

  0



