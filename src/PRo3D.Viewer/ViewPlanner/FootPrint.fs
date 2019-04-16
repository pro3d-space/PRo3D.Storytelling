namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.VRVis
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.SgPrimitives
open Aardvark.Base.Rendering

open System
open System.Diagnostics
open System.IO

open MBrace.FsPickler.Json   
open PRo3D.Versioned
open Chiron

open PRo3d

module FootPrint = 
    
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
    

    let getFootprintsPath (scenePath:string) =
        let path = Path.GetDirectoryName scenePath
        path + "\\FootPrints"
    
    let takeScreenshot baseAddress (width:int) (height:int) name folder =
        let wc = new System.Net.WebClient()
        
        let clientStatistic = 
            let path = sprintf "%s/rendering/stats.json" baseAddress //sprintf "%s/rendering/stats.json" baseAddress
            Log.line "[RemoteControl] querying rendering stats at: %s" path
            let result = wc.DownloadString(path)
            let clientBla : list<ClientStatistics> =
                Pickler.unpickleOfJson  result
            match clientBla.Length with
                | 2 -> clientBla // clientBla.[1] 
                | _ -> failwith "no client bla"

        for cs in clientStatistic do
            let screenshot =            
                sprintf "%s/rendering/screenshot/%s?w=%d&h=%d&samples=4" baseAddress cs.name width height
            Log.line "[RemoteControl] Running screenshot on: %s" screenshot    

            match System.IO.Directory.Exists folder with
              | true -> ()
              | false -> System.IO.Directory.CreateDirectory folder |> ignore
            
            let filename = cs.name + name
            wc.DownloadFile(screenshot,Path.combine [folder; filename])
         
        //let screenshot =            
        //    sprintf "%s/rendering/screenshot/%s?w=%d&h=%d&samples=4" baseAddress clientStatistic.[0].name width height
        //Log.line "[RemoteControl] Running screenshot on: %s" screenshot    

        //match System.IO.Directory.Exists folder with
        //  | true -> ()
        //  | false -> System.IO.Directory.CreateDirectory folder |> ignore
               
        //wc.DownloadFile(screenshot,Path.combine [folder; name])

    let createFootprintData (vp:ViewPlanModel) (scenePath:string) =

        let fpPath = getFootprintsPath scenePath

        match vp.selectedViewPlan with
            | Some v -> 
                let now = DateTime.Now
                let roverName = v.rover.id
                let instrumentName = 
                    match v.selectedInstrument with
                                | Some i -> i.id
                                | None -> ""
                
               
                let pngName = System.String.Format(
                                        "{0:0000}{1:00}{2:00}_{3:00}{4:00}{5:00}_{6}_{7}.png",
                                        now.Year, now.Month, now.Day, now.Hour, now.Minute, now.Second, roverName, instrumentName)

                let svxName = System.String.Format(
                                    "{0:0000}{1:00}{2:00}_{3:00}{4:00}{5:00}_{6}_{7}.svx",
                                    now.Year, now.Month, now.Day, now.Hour, now.Minute, now.Second, roverName, instrumentName)

                let width, height =
                        match v.selectedInstrument with
                                | Some i -> let horRes = i.intrinsics.horizontalResolution/uint32(2)
                                            let vertRes = i.intrinsics.verticalResolution/uint32(2)
                                            int(horRes), int(vertRes)
                                | None -> 512, 512
                // save png file
                try takeScreenshot "http://localhost:54321" width height pngName fpPath with e -> printfn "error: %A" e

               
                let fileInfo = {
                    fileType = "PNGImage"
                    path = fpPath
                    name = pngName
                }
                let calibration = {
                    instrumentPlatformXmlFileName       = v.rover.id + ".xml"
                    instrumentPlatformXmlFileVersion    = 1.0
                }
                let roverInfo = {
                    position = v.position
                    lookAtPosition = v.lookAt
                    placementTrafo = v.roverTrafo
                }
                let panAx = v.rover.axes.TryFind "Pan Axis" |> Option.map(fun x -> x.angle.value )
                let panVal = match panAx with | Some av -> av | None -> 1.0

                let tiltAx = v.rover.axes.TryFind "Tilt Axis" |> Option.map(fun x -> x.angle.value )
                let tiltVal = match tiltAx with | Some av -> av | None -> 1.0
                let angles = {
                    panAxis = panVal
                    tiltAxis = tiltVal
                }
                let focal =
                        match v.selectedInstrument with
                                | Some i -> i.focal.value
                                | None -> 1.0
                let referenceFrameInfo = {
                    name = "Ground"
                    parentFrameName = ""
                }

                let instrumentinfo = {
                    camIdentifier       = instrumentName
                    angles              = angles
                    focalLength         = focal
                    referenceFrameInfo  = referenceFrameInfo
                }
                let acquisition = {
                    roverInfo       = roverInfo
                    instrumentInfo  = instrumentinfo
                }
                let simulatedViewData =
                    {
                        fileInfo    = fileInfo
                        calibration = calibration
                        acquisition = acquisition
                    }
                //Serialization.save (Path.Combine(fpPath, svxName)) simulatedViewData |> ignore
                let json = simulatedViewData |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty |> IO.writeToFile (Path.Combine(fpPath, svxName))
                //Serialization.writeToFile (Path.Combine(fpPath, svxName)) json 
                vp
            | None -> vp 


    //let projTrafo (near : float) (far : float) (focalLength:float) (aspect:float) (principalPoint:V2d) =
    //    let tnf = 2.0 * far * near
    //    let a = (far + near) / (far - near)
    //    let b = tnf / (near - far)

    //    let A = far / (far - near)
    //    let B = near * far / (near - far)

    //    Trafo3d(
    //        M44d(
    //            focalLength,                0.0,                               -principalPoint.X,       0.0,
    //            0.0,                        focalLength * aspect,              -principalPoint.Y,       0.0,
    //            0.0,                        0.0,                               -a,                      b,
    //            0.0,                        0.0,                               -1.0,                    0.0
    //        ),
    //        //M44d(
    //        //    focalLength,                0.0,                               principalPoint.X,  0.0,
    //        //    0.0,                        focalLength * aspect,              principalPoint.Y,  0.0,
    //        //    0.0,                        0.0,                               A,                      B,
    //        //    0.0,                        0.0,                               1.0,                    0.0
    //        //),
    //        M44d(
    //            1.0/focalLength,        0.0,                                    0.0,       -principalPoint.X / focalLength,
    //            0.0,                    1.0 / (aspect * focalLength),           0.0,       -principalPoint.Y / (focalLength * aspect),
    //            0.0,                    0.0,                                    0.0,       -1.0,
    //            0.0,                    0.0,                                    1.0/b,     -a/b

    //        )
    //    )

    let updateFootprint (instrument:Instrument) (roverpos:V3d) (model:ViewPlanModel) =
        
        let id = match model.selectedViewPlan with
                    | Some vp -> Some vp.id
                    | None -> None
        

        let res = V2i((int)instrument.intrinsics.horizontalResolution, (int)instrument.intrinsics.verticalResolution)
        //let image = PixImage<byte>(Col.Format.RGB,res).ToPixImage(Col.Format.RGB)
       
        let pi = PixImage<byte>(Col.Format.RGBA, res)
        pi.GetMatrix<C4b>().SetByCoord(fun (c : V2l) -> C4b.White) |> ignore
        let tex = PixTexture2d(PixImageMipMap [| (pi.ToPixImage(Col.Format.RGBA)) |], true) :> ITexture

        //let focal = instrument.focal.value
        let projectionTrafo = model.instrumentFrustum |> Frustum.projTrafo
        //|> Frustum.aspect
        //let frust    = model.instrumentFrustum
        //let hfov     = instrument.intrinsics.horizontalFieldOfView
        //let resh     = float(instrument.intrinsics.horizontalResolution)
        //let resv     = float(instrument.intrinsics.verticalResolution)
        //let ifrustum = Frustum.perspective (hfov.DegreesFromGons()) frust.near frust.far (resh/resv)
        //let aspect = frust |> Frustum.aspect
        //let pP = V2d(instrument.intrinsics.horizontalPrinciplePoint, instrument.intrinsics.verticalPrinciplePoint)

        //let projectionTrafo = projTrafo model.instrumentFrustum.near model.instrumentFrustum.far focal aspect pP

        let location = model.instrumentCam.view.Location - roverpos //transformenExt.position
        let testview = model.instrumentCam.view.WithLocation location


        let fp = 
                {
                    vpId      = id
                    isVisible = true
                    projectionMatrix = projectionTrafo.Forward
                    instViewMatrix = testview.ViewTrafo.Forward //model.instrumentCam.view.ViewTrafo.Forward
                    projTex = tex
                    globalToLocalPos = roverpos //transformenExt.position
                }
        fp //{ model with footPrint = fp }


