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
        
    let view (m : MModel) : DomNode<RemoteControlModel.Action> =
        
        require Html.semui (
            div[] [
                button [clazz "ui labeled icon button"; onClick (fun _ -> Play)] [
                    i [clazz "play icon"][]
                    text "play"
                ]
                Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto; width: 300px"]) 
                    (                
                        alist {                                                                     
                            for vp in m.viewPoints do 
                                yield div [clazz "item"] [
                                    div [clazz "content"] [
                                        //div [clazz "header"] [ text (sprintf "%A" vp.Location) ]
                                        //div [clazz "description"] [
                                        //    text (sprintf "%A" vp.Forward)
                                        //    button [clazz "ui inverted"; style "color:black"; onClick (fun _ -> MakeScreenshot vp)] [text "goto"]
                                        //]
                                         div [clazz "header"] [                                        
                                            text vp.name
                                            i [clazz "ui inverted icon map pin"; onClick (fun _ -> MakeScreenshot vp.cv)] []
                                        ]
                                    ]
                                ]
                        }
                    )
            ]    
        )

    let fromDate (dt : DateTime) =
//        sprintf "%d%d%d_%d%d%d" dt.Year dt.Month dt.Day dt.Hour dt.Minute dt.Second
        dt.ToString("yyyymmdd_hhmmss")

    let takeScreenshot baseAddress filename=
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
               
        wc.DownloadFile(screenshot,Path.combine [path; filename])

    let update (baseAddress : string) (send : RemoteAction -> unit) (m : Model) (a : RemoteControlModel.Action) =
        match a with
            | MakeScreenshot cv ->
                send <| RemoteAction.SetViewpoint cv

                let filename = System.IO.Path.ChangeExtension ((DateTime.Now |> fromDate),".jpg")
                try takeScreenshot baseAddress filename with e -> printfn "error: %A" e
                m
            | UpdateCamera cv ->
                send <| RemoteAction.SetViewpoint cv
                m
            | Play ->
                for wp in m.viewPoints do
                    send <| RemoteAction.SetViewpoint wp.cv
                    let fileName = System.IO.Path.ChangeExtension (wp.name,".jpg")
                    try takeScreenshot baseAddress fileName with e -> printfn "error: %A" e
                m

    let threads (m : Model) : ThreadPool<RemoteControlModel.Action> =
        ThreadPool.empty

    let loadWaypoints m = 
        match Serialization.fileExists "./waypoints.wps" with
          | Some path -> 
                let wp = Serialization.loadAs<plist<PRo3D.Viewer.WayPoint>> path

                let shots = wp |> PList.map(fun x ->
                        { Shot.init with 
                            pos     = x.cv.Location |> Xyz.fromV3d
                            up      = x.cv.Up |> Xyz.fromV3d
                            lookAt  = (x.cv.Location + x.cv.Forward) |> Xyz.fromV3d  }
                    )
                { m with viewPoints = wp; shots = shots }
          | None -> m

    let app baseAddress send =
        {
            unpersist = Unpersist.instance
            threads = threads
            view = view
            update = update baseAddress send
            initial = { viewPoints = PList.empty; shots = PList.empty } |> loadWaypoints

        }
