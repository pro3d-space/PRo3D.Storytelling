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
    open ViewPlanner
    open PRo3d
    
        
    let jsonSerializer = FsPickler.Json.JsonSerializer(indent=true)

    

    let fromDate (dt : DateTime) =
//        sprintf "%d%d%d_%d%d%d" dt.Year dt.Month dt.Day dt.Hour dt.Minute dt.Second
        dt.ToString("yyyymmdd_hhmmss")

    let takeScreenshot baseAddress (shot:Shot) =
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

    let mkInstrumetnWps m = 
        let pShots = 
            m.shots |> PList.choose(fun x -> PlatformShot.fromRoverModel m.Rover x)
        { m with platformShots = pShots }

    let loadWaypoints m = 
        match Serialization.fileExists "./waypoints.wps" with
          | Some path -> 
                let wp = Serialization.loadAs<plist<PRo3D.Viewer.WayPoint>> path

                let shots = wp |> PList.map Shot.fromWp
                let wp_shots = shots |> PList.toList |> jsonSerializer.PickleToString                

                System.IO.File.WriteAllText("./shots.json", wp_shots)

                let m = { m with shots = shots }  |> mkInstrumetnWps
                let plat_shots = m.platformShots |> PList.toList |> jsonSerializer.PickleToString    
                System.IO.File.WriteAllText("./platformshots.json", plat_shots)

                m
          | None -> m

    let roverStuff (m : RemoteModel) =
        let directory = @".\InstrumentStuff"
        let errorCode = IPWrapper.Init(directory, directory)
        printfn "%A" errorCode

        let names = RoverProvider.platformNames()    
        printfn "%A" names

        let roverData =
            names 
              |> Array.map RoverProvider.initRover 
              |> List.ofArray 

        let rovs = roverData|> List.map(fun (r,_) -> r.id, r) |> HMap.ofList
        let plats = roverData|> List.map(fun (r,p) -> r.id, p) |> HMap.ofList

        printfn "found rover(s): %d" (rovs |> HMap.count)
        
        { m with Rover = { RoverApp.initial with rovers = rovs; platforms = plats } }
       
    let norm (v:V3d) = 
        v.Normalized

    let update (baseAddress : string) (send : RemoteAction -> unit) (m : RemoteModel) (a : RemoteControlModel.Action) =
        match a with
            | MakeScreenshot2 psh ->

                //get actual instrument params from shot
                //build camera view/ frustum from shot
                //set camera/frustum remote
                //take screenshot (width and height)

                m
            | MakeScreenshot sh ->
                
                send <| RemoteAction.SetViewpoint (sh |> Shot.getCamera)
                
                try takeScreenshot baseAddress sh with e -> printfn "error: %A" e
                m
            | UpdateCamera sh ->                                
                send <| RemoteAction.SetViewpoint (sh |> Shot.getCamera)
                m
            | UpdateCamera2 sh ->
                let platform = RemoteControlModel.PlatformShot.fromRoverModel m.Rover sh
                match platform with 
                   | Some p ->
                        let psh = PlatformShot.withShot sh p

                        //getviewpoint from platformshot
                        //fov
                        //placementtrafo

                        let forward = ((psh.lookAt |> Xyz.toV3d) - (psh.pos |> Xyz.toV3d)) |> norm
                        let up = psh.up |> Xyz.toV3d |> norm
                        let pos = psh.pos |> Xyz.toV3d |> norm

                        let placement = Trafo3d.FromBasis(forward, up.Cross forward, up, pos)

                        let cv = sh |> Shot.getCamera

                        send <| RemoteAction.SetViewpoint (cv)
                        m
                    | None -> m
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
            | RoverMessage a ->
                // react on change and update rover via dll
                { m with Rover = RoverApp.update m.Rover a }                

    let viewShots (m:MRemoteModel) = 
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
                                    i [clazz "ui inverted icon find"; onClick (fun _ -> UpdateCamera2 sh)] []
                                ]
                            ]
                        ]
                }
            )

    

    let view (m : MRemoteModel) : DomNode<RemoteControlModel.Action> =
        
  //      <div class="ui three column grid">
  //<div class="column">
  //  <div class="ui segment">

        require Html.semui (
            div[clazz "ui two column grid"] [
                div[clazz "column"] [
                    div[clazz "ui segment"] [
                        h2 [clazz "ui header"][text "Waypoints"]
                        button [clazz "ui labeled icon button"; onClick (fun _ -> Play)] [
                            i [clazz "play icon"][]
                            text "play"
                        ]
                        button [clazz "ui labeled icon button"; onClick (fun _ -> Load)] [
                            i [clazz "Eject icon"][]
                            text "load"
                        ]
                        viewShots m
                    ]
                    div[clazz "ui segment"] [
                        h2 [clazz "ui header"][text "Rovers"]
                        RoverApp.view m.Rover |> UI.map RoverMessage
                    ]
                ]
                div[clazz "column"] []
                
            ]    
        )

    let threads (m : RemoteModel) : ThreadPool<RemoteControlModel.Action> =
        ThreadPool.empty

    

    let app baseAddress send =
        {
            unpersist = Unpersist.instance
            threads = threads
            view = view
            update = update baseAddress send
            initial = { shots = PList.empty; Rover = RoverApp.initial; platformShots = PList.empty } |> loadWaypoints |> roverStuff
        }
