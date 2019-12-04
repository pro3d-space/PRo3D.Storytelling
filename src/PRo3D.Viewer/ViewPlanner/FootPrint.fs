namespace PRo3D

open Aardvark.Base
open Aardvark.UI.Animation
open Aardvark.UI.Primitives
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
open PRo3D.Base

module FootPrint = 
        
    let getFootprintsPath (scenePath:string) =
        let path = Path.GetDirectoryName scenePath
        path + "\\FootPrints"
       
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
                                        "{0:0000}{1:00}{2:00}_{3:00}{4:00}{5:00}_{6}_{7}",
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
                try Utilities.takeScreenshotFromAllViews "http://localhost:54321" width height pngName fpPath with e -> printfn "error: %A" e

               
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
                let json = simulatedViewData |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty |> Serialization.writeToFile (Path.Combine(fpPath, svxName))
                //Serialization.writeToFile (Path.Combine(fpPath, svxName)) json 
                vp
            | None -> vp 
    
    let updateFootprint (instrument:Instrument) (roverpos:V3d) (model:ViewPlanModel) =
        
        let id = match model.selectedViewPlan with
                    | Some vp -> Some vp.id
                    | None -> None
        

        let res = V2i((int)instrument.intrinsics.horizontalResolution, (int)instrument.intrinsics.verticalResolution)
        //let image = PixImage<byte>(Col.Format.RGB,res).ToPixImage(Col.Format.RGB)
       
        let pi = PixImage<byte>(Col.Format.RGBA, res)
        pi.GetMatrix<C4b>().SetByCoord(fun (c : V2l) -> C4b.White) |> ignore
        let tex = PixTexture2d(PixImageMipMap [| (pi.ToPixImage(Col.Format.RGBA)) |], true) :> ITexture
        
        let projectionTrafo = model.instrumentFrustum |> Frustum.projTrafo
        

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

module SnapshotAnimation = 

    let animateAndScreenshot (scenePath:string) (ex:Extrinsics) (duration : RelativeTime) (name : string) (width : int) (height : int) = 
      let fpPath = FootPrint.getFootprintsPath scenePath
      {
        (CameraAnimations.initial name) with 
          sample = fun (localTime, globalTime) (state : CameraView) -> // given the state and t since start of the animation, compute a state and the cameraview
            if localTime < duration then 
                
              let now = DateTime.Now
              let pngName = System.String.Format(
                                        "{0:0000}{1:00}{2:00}_{3:00}{4:00}{5:00}_{6}",
                                        now.Year, now.Month, now.Day, now.Hour, now.Minute, now.Second, now.Millisecond)
               
               
              //try Utilities.takeScreenshot "http://localhost:54321" width height pngName fpPath with e -> printfn "error: %A" e

              let rot      = Rot3d(state.Forward, ex.camLookAt) * localTime / duration
              let forward' = rot.TransformDir(state.Forward)

              let uprot     = Rot3d(state.Up, ex.camUp) * localTime / duration
              let up'       = uprot.TransformDir(state.Up)
              
              let vec       = ex.position - state.Location
              let velocity  = vec.Length / duration                  
              let dir       = vec.Normalized
              let location' = state.Location + dir * velocity * localTime

              let view = 
                state 
                  |> CameraView.withForward forward'
                  |> CameraView.withUp up'
                  |> CameraView.withLocation location'
      
              Some (state,view)
            else None
      }

    let updateSnapshotCam (extr:ArnoldSnapshot)  = 
        CameraView.look extr.location extr.foward.Normalized extr.up.Normalized

    let updateSnapshotFrustum (frust:Frustum) (dataAnimation:ArnoldAnimation)  =

        let hfov  = dataAnimation.fieldOfView
        let resh  = float(dataAnimation.resolution.X)
        let resv  = float(dataAnimation.resolution.Y)

        Frustum.perspective (hfov.DegreesFromGons()) frust.near frust.far (resh/resv)

        
    let loadData (path : string) =
        try 
             let (arnoldanims : ArnoldAnimation) =
                    path 
                        |> Serialization.readFromFile
                        |> Json.parse 
                        |> Json.deserialize
             arnoldanims |> Some    
        with e ->        
            Log.error "[ViewerIO] couldn't load %A" e
            None

    let writeDataToJsonFile =   
        //let path = "C:\Users\laura\VRVis\Data\Donut_medium_georef_compl\donut.snapshots.json"
        //let ex1 =
        //     {
        //        location = V3d(-36924.38, 272515.17, 587.71)
        //        foward   = V3d(0.44, -0.31, -0.84)
        //        up       = V3d(-0.67, 0.51, -0.54)
        //    }
           
        //let ex2 =
        //    {
        //        location = V3d(-36930.36, 272519.67, 569.74)
        //        foward   = V3d(0.46, -0.34, 0.82)
        //        up       = V3d(-0.74, -0.66, 0.15)
        //    }
            
        //let ex3 =
        //    {
        //        location = V3d(-36926.17, 272516.11, 584.91)
        //        foward   = V3d(0.83, -0.51, -0.24)
        //        up       = V3d(-0.39, -0.82, 0.41)
        //    }
            
        //let testdata = 
        //    {
        //        fieldOfView = 26.49
        //        resolution  = V2i(1600, 1200)
        //        snapshots   = [ex1; ex2; ex3]
        //    }

        let path = "C:\Users\laura\VRVis\Data\GardenCitySmall\GardenCitySmall\gardenCity.snapshots.json"
        let ex1 =
             {
                location = V3d(-2486974.23, 2288926.50, -275792.76)
                foward   = V3d(0.42, -0.63, -0.65)
                up       = V3d(-0.54, 0.40, -0.74)
            }
           
        let ex2 =
            {
                location = V3d(-2486973.65, 2288925.99, -275793.60)
                foward   = V3d(0.34, -0.85, 0.41)
                up       = V3d(-0.55, 0.18, -0.82)
            }
            
        let ex3 =
            {
                location = V3d(-2486973.72, 2288924.03, -275796.63)
                foward   = V3d(0.05, 0.51, 0.86)
                up       = V3d(-0.07, -0.86, 0.51)
            }

        let ex4 =
            {
                location = V3d(-2486971.72, 2288922.95, -275794.30)
                foward   = V3d(-0.54, 0.82, 0.17)
                up       = V3d(-0.34, -0.40, 0.85)
            }

        let ex5 =
            {
                location = V3d(-2486972.34, 2288940.76, -275790.74)
                foward   = V3d(-0.08, -0.98, -0.19)
                up       = V3d(0.09, -0.20, 0.98)
            }
            
        let testdata = 
            {
                fieldOfView = 26.49
                resolution  = V2i(1600, 1200)
                snapshots   = [ex1; ex2; ex3; ex4; ex5]
            }
          
        testdata
         |> Json.serialize 
         |> Json.formatWith JsonFormattingOptions.Pretty 
         |> Serialization.writeToFile path
        
    
    let snapshot (fpPath:string) (width : int) (height : int) =
        let now = DateTime.Now
        let pngName = System.String.Format(
                                "{0:0000}{1:00}{2:00}_{3:00}{4:00}{5:00}{6:00}",
                                now.Year, now.Month, now.Day, now.Hour, now.Minute, now.Second, now.Millisecond)
        try Utilities.takeScreenshotFromAllViews "http://localhost:54321" width height pngName fpPath with e -> printfn "error: %A" e
       

    
    let startSnapshots (scenepath : string) (width : int) (height : int) = 
        let fpPath = FootPrint.getFootprintsPath scenepath
        snapshot fpPath width height 