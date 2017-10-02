namespace PRo3D

open Aardvark.Base
open Aardvark.UI
open RemoteControlModel

type ClientStatistics =
    {
        session         : System.Guid
        name            : string
        frameCount      : int
        invalidateTime  : float
        renderTime      : float
        compressTime    : float
        frameTime       : float
    }

module RemoteControlApp =

    let view (m : MModel) : DomNode<Action> =
        button [onClick (fun _ -> MakeScreenshot)] [text "mk"]

    let doIt baseAddress =
        let wc = new System.Net.WebClient()
        let path = "screenshots"
        if System.IO.Directory.Exists path |> not then
            System.IO.Directory.CreateDirectory path |> ignore
        let clientStatistic = 
            let path = sprintf "%s/rendering/stats.json" baseAddress
            Log.line "[RemoteControl] querying rendering stats at: %s" path
            let result = wc.DownloadString(path)
            let clientBla : list<ClientStatistics> =
                Pickler.unpickleOfJson  result
            match clientBla with
                | [] -> failwith "no client bla"
                | x::[] -> x
                | _ -> failwith "doent know"
        let screenshot =
            let w = 1024
            let h = 1024
            sprintf "%s/rendering/screenshot/%s?w=%d&h=%d" baseAddress clientStatistic.name w h
        Log.line "[RemoteControl] Running screenshot on: %s" screenshot
        wc.DownloadFile(screenshot,Path.combine [path; "a.jpg"])

    let update (baseAddress : string) (send : RemoteAction -> unit) (m : Model) (a : Action) =
        match a with
            | MakeScreenshot ->
                send <| RemoteAction.SetViewpoint (CameraView.lookAt V3d.OOO V3d.OOO V3d.OOI)
                try doIt baseAddress with e -> printfn "error: %A" e
                m

    let threads (m : Model) : ThreadPool<Action> =
        ThreadPool.empty

    let app baseAddress send =
        {
            unpersist = Unpersist.instance
            threads = threads
            view = view
            update = update baseAddress send
            initial = { viewPoints = [] }
        }
