open System

open Aardvark.Fablish
open System.Windows.Forms

[<EntryPoint;STAThread>]
let main argv =

    let app = PerformanceTest.app
    let app = PRo3dTreeApp.app
    let runWindow = true

    if runWindow then
        let browser = Chromium.runControl "8083" app
        use w = new Form()
        w.Controls.Add browser
        w.Width <- 800
        w.Height <- 600
        Application.Run(w) 
    else
        Fablish.runLocally "8083" app

    0