namespace PRo3D

open System
open System.Runtime.InteropServices

open Aardvark.Base
open ViewPlanner

open PRo3d

module RoverProvider =
    
    let toInstrument i (inst : IPWrapper.SInstrument) = 
        let pos    = inst.m_oInstrumentExtrinsics.m_oPosition.ToV3d()
        let up     = inst.m_oInstrumentExtrinsics.m_oUp.ToV3d()
        let lookAt = inst.m_oInstrumentExtrinsics.m_oLookAt.ToV3d()
        let box    = inst.m_oInstrumentExtrinsics.m_oBoundingBox.ToBox3d()

        let focals = 
            IPWrapper.UnMarshalArray<double>(
                    inst.m_pdCalibratedFocalLengths, (int)inst.m_nNrOfCalibratedFocalLengths) 

        let extrinsics =
            {
                position  = pos
                camUp     = up
                camLookAt = lookAt
                box       = box
            }

        let intrinsics =
            {
                horizontalFieldOfView         = inst.m_oInstrumentIntrinsics.m_dFieldOfViewH
                verticalFieldOfView           = inst.m_oInstrumentIntrinsics.m_dFieldOfViewV
                horizontalResolution          = inst.m_oInstrumentIntrinsics.m_nResolutionH
                verticalResolution            = inst.m_oInstrumentIntrinsics.m_nResolutionV
                horizontalPrinciplePoint      = inst.m_oInstrumentIntrinsics.m_dPrinciplePointH
                verticalPrinciplePoint        = inst.m_oInstrumentIntrinsics.m_dPrinciplePointV
                horizontalFocalLengthPerPixel = inst.m_oInstrumentIntrinsics.m_dFocalLengthPerPxH
                verticalFocalLengthPerPixel   = inst.m_oInstrumentIntrinsics.m_dFocalLengthPerPxV
                horizontalDistortionMap       = String.Empty
                verticalDistortionMap         = String.Empty
                vignettingMap                 = String.Empty
            }

        let instrument = {
            index                  = i
            id                     = inst.m_pcInstrumentName.ToStrAnsi()
            calibratedFocalLengths = focals |> Array.toList
            currentFocalLength     = inst.m_dCurrentFocalLengthInMm
            intrinsics             = intrinsics
            extrinsics             = extrinsics
            iType                  = InstrumentType.Undefined
        }        

        instrument

    let toInstruments (instruments : IPWrapper.SInstrument[]) : list<Instrument> = 
        instruments |> Array.mapi toInstrument |> Array.toList
        
    let toAxes (axes : IPWrapper.SAxis[]) : list<Axis> =
        axes |> Array.mapi(fun i x->
            { 
                index        = i
                id           = x.m_pcAxisId.ToStrAnsi()
                description  = x.m_pcAxisDescription.ToStrAnsi()
                startPoint   = x.m_oStartPoint.ToV3d()
                endPoint     = x.m_oEndPoint.ToV3d()
                minAngle     = x.m_fMinAngle.DegreesFromGons()
                maxAngle     = x.m_fMaxAngle.DegreesFromGons()
                currentAngle = x.m_fCurrentAngle.DegreesFromGons()
            }) |> Array.toList

    let toRover (platform : IPWrapper.SPlatform) =
        let wheels = 
            IPWrapper.UnMarshalArray<IPWrapper.SPoint3D>(
                    platform.m_poPointsOnGround, (int)platform.m_nNrOfPlatformPointsOnGround).ToV3ds();

        let axes = 
            IPWrapper.UnMarshalArray<IPWrapper.SAxis>(platform.m_poPlatformAxes, (int)platform.m_nNrOfPlatformAxes) |> toAxes

        let instruments = 
            IPWrapper.UnMarshalArray<IPWrapper.SInstrument>(platform.m_poPlatformInstruments, (int)platform.m_nNrOfPlatformInstruments) |> toInstruments

        wheels, axes, instruments

    let platformNames () =
        let numberOfPlatforms = IPWrapper.GetNrOfAvailablePlatforms();
        Log.line "Number of Plattforms: %d" numberOfPlatforms
    
        let pointerArray : IntPtr array = Array.zeroCreate (int numberOfPlatforms)
        IPWrapper.GetAvailablePlatforms(pointerArray, numberOfPlatforms);
       
        pointerArray |> Array.map(fun x -> Marshal.PtrToStringAnsi(x))


    let initRover platformId =
        //Get various counts to initialize respective arrays
        let numWheelPoints = uint32 6 // IPWrapper.GetNrOfPlatformPointsOnGround(platformId)
        let numInstruments = IPWrapper.GetNrOfPlatformInstruments(platformId)
        let numAxis =        IPWrapper.GetNrOfPlatformAxes(platformId)

        Log.line "Initialising %A" platformId
        Log.line "Wheels: %d, Instruments: %d, Axes: %d" numWheelPoints numInstruments numAxis

        //create and empty platform struct to be filled by the backend
        let mutable platform = IPWrapper.SPlatform.CreateEmpty(numWheelPoints, numInstruments, numAxis)

        //crucial for backend to know which platform / rover to take
        platform.m_pcPlatformId <- platformId.ToPtr();

        //init platform writes values into platform reference
        let errorCode = IPWrapper.InitPlatform(ref platform, numWheelPoints, numInstruments, numAxis)
        if (errorCode <> 0) then failwith "init platform failed" else ()

        //convert initialised platform to rover and add to database
        let rover = platform |> toRover
        //RoverDatabase.Add(platformId, rover);
        //PlatformDatabase.Add(platformId, platform);
        rover
        