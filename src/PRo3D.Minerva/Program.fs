open System
open Suave

open Aardvark
open Aardvark.Base
open Aardvark.UI
open Aardium
open Aardvark.Application.WinForms

[<EntryPoint;STAThread>]
let main argv = 
  Ag.initialize()
  Aardvark.Init()
  Aardium.init() 

  use app = new OpenGlApplication()
  let runtime = app.Runtime    

  let mainApp = PRo3D.Minerva.App.start

  WebPart.startServerLocalhost 1776 [
        MutableApp.toWebPart' runtime false mainApp
      //  Reflection.assemblyWebPart typeof<EmbeddedResources>.Assembly
        Suave.Files.browseHome        
  ]

  Aardium.run {
        url "http://localhost:1776/"   //"http://localhost:4321/?page=main"
        width  400
        height 800
        debug true
        title "Minerva 0.0.1"
    }

  0



