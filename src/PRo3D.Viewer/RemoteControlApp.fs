namespace PRo3D

open Aardvark.Base
open Aardvark.UI
open RemoteControlModel
open System

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
    open Aardvark.Base.Incremental    
    open MBrace
    open MBrace.FsPickler.Json
    open System.Diagnostics
    
        
    let jsonSerializer = FsPickler.Json.JsonSerializer(indent=false)

    

    let fromDate (dt : DateTime) =
//        sprintf "%d%d%d_%d%d%d" dt.Year dt.Month dt.Day dt.Hour dt.Minute dt.Second
        dt.ToString("yyyymmdd_hhmmss")

    let takeScreenshot baseAddress shot =
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
            let w = shot.col
            let h = shot.row
            sprintf "%s/rendering/screenshot/%s?w=%d&h=%d" baseAddress clientStatistic.name w h
        Log.line "[RemoteControl] Running screenshot on: %s" screenshot    

        match System.IO.Directory.Exists shot.folder with
          | true -> ()
          | false -> System.IO.Directory.CreateDirectory shot.folder |> ignore
               
        let filename = System.IO.Path.ChangeExtension (shot.id,".jpg")
        wc.DownloadFile(screenshot,Path.combine [shot.folder; filename])

    let loadWaypoints m = 
        match Serialization.fileExists "./waypoints.wps" with
          | Some path -> 
                let wp = Serialization.loadAs<plist<PRo3D.Viewer.WayPoint>> path

                let shots = wp |> PList.map Shot.fromWp
                let pshots = shots |> PList.toList |> jsonSerializer.PickleToString

                System.IO.File.WriteAllText("./waypoints.json", pshots)

                { m with shots = shots }
          | None -> m

    let update (baseAddress : string) (send : RemoteAction -> unit) (m : Model) (a : RemoteControlModel.Action) =
        match a with
            | MakeScreenshot sh ->
                send <| RemoteAction.SetViewpoint (sh |> Shot.getCamera)
                
                try takeScreenshot baseAddress sh with e -> printfn "error: %A" e
                m
            | UpdateCamera sh ->
                send <| RemoteAction.SetViewpoint (sh |> Shot.getCamera)
                m
            | Play ->
                for sh in m.shots do
                    send <| RemoteAction.SetViewpoint (sh |> Shot.getCamera)                  
                    try takeScreenshot baseAddress sh with e -> printfn "error: %A" e
                m
            | Load ->
                loadWaypoints m
            | OpenFolder path ->
                Process.Start("explorer.exe", path) |> ignore
                m

    let view (m : MModel) : DomNode<RemoteControlModel.Action> =
        
        require Html.semui (
            div[] [
                button [clazz "ui labeled icon button"; onClick (fun _ -> Play)] [
                    i [clazz "play icon"][]
                    text "play"
                ]
                button [clazz "ui labeled icon button"; onClick (fun _ -> Load)] [
                    i [clazz "Eject icon"][]
                    text "load"
                ]
                Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto; width: 300px"]) 
                    (                
                        alist {                                                                     
                            for sh in m.shots do 
                                yield div [clazz "item"] [
                                    div [clazz "content"] [                                        
                                         div [clazz "header"] [                                        
                                            text sh.id
                                            i [clazz "ui inverted icon map pin"; onClick (fun _ -> UpdateCamera sh)] []
                                            i [clazz "ui inverted icon folder"; onClick (fun _ -> OpenFolder sh.folder)] []
                                        ]
                                    ]
                                ]
                        }
                    )
            ]    
        )

    let threads (m : Model) : ThreadPool<RemoteControlModel.Action> =
        ThreadPool.empty

    

    let app baseAddress send =
        {
            unpersist = Unpersist.instance
            threads = threads
            view = view
            update = update baseAddress send
            initial = { shots = PList.empty } |> loadWaypoints
        }
