namespace PRo3D

open System

open Aardvark.Base
open Aardvark.UI
open ViewPlanner
open PRo3d

module RoverProvider =
    open System.Runtime.InteropServices
    
    
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

        let intrinsics : Intrinsics =
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

       // let focalToNumeric (focals : list<double>) (current : double) : NumericInput =

        let instrument = {
            index                  = i
            id                     = inst.m_pcInstrumentName.ToStrAnsi()
            calibratedFocalLengths = focals |> Array.toList
            //currentFocalLength     = inst.m_dCurrentFocalLengthInMm
            intrinsics             = intrinsics
            extrinsics             = extrinsics
            iType                  = InstrumentType.Undefined

            focal = {
                        value  = inst.m_dCurrentFocalLengthInMm
                        min    = focals |> Array.head 
                        max    = focals |> Array.last
                        step   = 0.1
                        format = "{0:0.0}"
            }
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
                //minAngle     = x.m_fMinAngle.DegreesFromGons()
                //maxAngle     = x.m_fMaxAngle.DegreesFromGons()
                //currentAngle = x.m_fCurrentAngle.DegreesFromGons()

                angle = {
                            value  = x.m_fCurrentAngle.DegreesFromGons()
                            min    = x.m_fMinAngle.DegreesFromGons()
                            max    = x.m_fMaxAngle.DegreesFromGons()
                            step   = 0.1
                            format = "{0:0.0}"
                        }

            }) |> Array.toList

    let toRover (platform : IPWrapper.SPlatform) =
        let wheels = 
            IPWrapper.UnMarshalArray<IPWrapper.SPoint3D>(
                    platform.m_poPointsOnGround, (int)platform.m_nNrOfPlatformPointsOnGround).ToV3ds();

        let axes = 
            IPWrapper.UnMarshalArray<IPWrapper.SAxis>(platform.m_poPlatformAxes, (int)platform.m_nNrOfPlatformAxes) |> toAxes

        let instruments = 
            IPWrapper.UnMarshalArray<IPWrapper.SInstrument>(platform.m_poPlatformInstruments, (int)platform.m_nNrOfPlatformInstruments) |> toInstruments
                
        {
            id              = platform.m_pcPlatformId.ToStrAnsi()
            platform2Ground = platform.m_oPlatform2Ground.m_oHelmertTransfMatrix.ToM44d()
            wheelPositions  = wheels |> List.ofSeq
            instruments     = instruments |> List.map(fun x -> x.id, x) |> HMap.ofList
            axes            = axes |> List.map(fun x -> x.id, x) |> HMap.ofList
            box             = platform.m_oBoundingBox.ToBox3d()
        }
        
    let platformNames () =
        let numberOfPlatforms = IPWrapper.GetNrOfAvailablePlatforms();
        Log.line "Number of Plattforms: %d" numberOfPlatforms
    
        let pointerArray : IntPtr array = Array.zeroCreate (int numberOfPlatforms)
        IPWrapper.GetAvailablePlatforms(pointerArray, numberOfPlatforms);
       
        pointerArray |> Array.map(fun x -> Marshal.PtrToStringAnsi(x))

    let initRover platformId : Rover =
        //Get various counts to initialize respective arrays
        let numWheelPoints = uint32 6 // IPWrapper.GetNrOfPlatformPointsOnGround(platformId)
        let numInstruments = IPWrapper.GetNrOfPlatformInstruments(platformId)
        let numAxis =        IPWrapper.GetNrOfPlatformAxes(platformId)

        Log.line "Initialising %A" platformId
        Log.line "Wheels: %d, Instruments: %d, Axes: %d" numWheelPoints numInstruments numAxis

        //create and empty platform struct to be filled by the backend
        let mutable platform = 
            IPWrapper.SPlatform.CreateEmpty(numWheelPoints, numInstruments, numAxis)

        //crucial for backend to know which platform / rover to take
        platform.m_pcPlatformId <- platformId.ToPtr();

        //init platform writes values into platform reference
        let errorCode = IPWrapper.InitPlatform(ref platform, numWheelPoints, numInstruments, numAxis)
        if (errorCode <> 0) then failwith "init platform failed" else ()

        //convert initialised platform to rover and add to database
        platform |> toRover        

    let updateAngles(update : AxisAngleUpdate) =
        // save c structs counterparts of rover

        // get rover by id

        // get axis by id

        // update Saxis

        // call updatePlatform on platform
            //var error = UpdatePlatform(ref platform);
            //if (error != 0)
            //    Report.Warn("Update error code {0}", error);

        //push updated values into rovers database rm


        ()
        

module RoverApp = 
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI.UI                   

    type Action =
        | ChangeAngle      of string * Numeric.Action
        | ChangeFocal      of string * Numeric.Action
        | SelectRover      of option<Rover>
        | SelectInstrument of option<Instrument>
        | SelectAxis       of option<Axis>

    let updateRovers (r : Rover) (m : RoverModel) = 
        let rovers' = 
            m.rovers 
                |> HMap.update r.id (fun x -> 
                    match x with 
                        | Some _ -> r
                        | None   -> failwith "rover not found")
        
        { m with selectedRover = Some r; rovers = rovers' }

    let update (m : RoverModel) (a:Action) =
        match a with
          | SelectRover r      -> { m with selectedRover = r }
          | SelectInstrument i -> { m with selectedInstrument = i }
          | SelectAxis a       -> { m with selectedAxis = a }
          | ChangeAngle (id,a) ->            
                match m.selectedRover with 
                  | Some r -> 
                     let axes' = r.axes |> HMap.update id (fun x -> 
                        match x with
                          | Some ax -> { ax with angle = Numeric.update ax.angle a }
                          | None    -> failwith "axis not found"                 
                     )                      

                     m |> updateRovers { r with axes= axes' }                                         
                     
                  | None -> m        
          | ChangeFocal (id, f) ->
             match m.selectedRover with 
                  | Some r -> 
                      match m.selectedInstrument with
                        | Some inst ->             
                            
                            let inst' =  { inst with focal = Numeric.update inst.focal f }

                            let instruments' = 
                                r.instruments |> HMap.update id (fun x -> 
                                    match x with
                                      | Some _ -> inst'
                                      | None   -> failwith "axis not found")                       

                            { m with selectedInstrument = Some inst' } |> updateRovers { r with instruments = instruments' }                            
                        | None -> m                                        
                  | None -> m          

    let angle = 
        {
            value = 0.0
            min =  -90.0
            max = 90.0
            step = 0.1
            format = "{0:0.0}"
        }

    let initial =
        {
            rovers = hmap.Empty
            selectedInstrument = None
            selectedRover = None
            selectedAxis = None
            currentAngle = angle
        }

    let mapTolist (input : amap<_,'a>) : alist<'a> = 
        input |> AMap.toASet |> AList.ofASet |> AList.map snd    
    
    let roversList (m:MRoverModel) = 
        (m.rovers |> mapTolist)

    let toText (r : MRover) =
        adaptive {
            let! instr  = r.instruments |> mapTolist |> AList.count
            let! axes   = r.axes |> mapTolist |> AList.count
            let! wheels = r.wheelPositions |> Mod.map(fun x -> x.Length)

            return sprintf "instr: %d | axes: %d  | wheels: %d" instr axes wheels
        }

    let viewRovers (m:MRoverModel) = 
        Incremental.div 
            (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto; width: 300px"]) 
            (                  
                alist {                                                                                         
                    for r in (roversList m) do
                        yield div [clazz "item"] [
                            div [clazz "content"] [                                        
                                div [clazz "header"] [
                                    Incremental.text r.id                                    
                                ]
                                div [clazz "description"] [
                                   Incremental.text (r |> toText)
                                ]
                            ]
                        ]
                }
            )    

    let focalGui (i : MInstrument) (m : MRoverModel) =
        let nodes =
            alist {
                
                let! focals = i.calibratedFocalLengths
                if focals.Length > 1 then                    
                    yield Numeric.view' [NumericInputType.Slider; NumericInputType.InputBox] i.focal |> UI.map (fun x -> ChangeFocal (i.id |> Mod.force,x))
                else
                    yield Incremental.text (i.focal.value |> Mod.map string)
            }

        Incremental.div AttributeMap.Empty nodes
    
    

    let instrumentProperties(i : MInstrument) (m : MRoverModel) =

        let sensor = i.intrinsics |> Mod.map(fun x -> sprintf "%d X %d" x.horizontalResolution x.verticalResolution)

        require Html.semui (
            Html.table [                
                Html.row "Sensor (px):"  [ Incremental.text sensor ]
                Html.row "Focal (mm):"    [ focalGui i m ]
            ]
        )

    let viewInstrumentProperties (m : MRoverModel) = 
        m.selectedInstrument 
            |> Mod.map(fun x ->
                match x with 
                  | Some i -> instrumentProperties i m
                  | None ->   div[][])

    let instrumentsDd (r:MRover) (m : MRoverModel) = 
        UI.dropDown'' (r.instruments |> mapTolist) m.selectedInstrument (fun x -> SelectInstrument (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) )   

    let viewAxesList (r : MRover) (m : MRoverModel) =
        alist {
            for axis in (r.axes |> mapTolist) do
                yield div[][Incremental.text axis.id; text "(deg)"]
                yield Numeric.view' [NumericInputType.Slider; NumericInputType.InputBox] axis.angle |> UI.map (fun x -> ChangeAngle (axis.id |> Mod.force,x))
        }

    let viewRoverProperties' (r : MRover) (m : MRoverModel) =

       // let bla = UI.dropDown'' (roversList m)  m.selectedInstrument SelectRover (fun x -> x.id )

        require Html.semui (
            Html.table [
                 Html.row "Name:"       [ Incremental.text r.id ]
                 Html.row "Instrument:" [ 
                    instrumentsDd r m 
                    Incremental.div AttributeMap.empty (AList.ofModSingle (viewInstrumentProperties m))                    
                 ]
                 Html.row "Axes:" [                    
                    Incremental.div AttributeMap.empty (viewAxesList r m)
                 ]
            ]
        )

    let viewRoverProperties (m : MRoverModel) = 
        m.selectedRover 
            |> Mod.map(fun x ->
                match x with 
                  | Some r -> viewRoverProperties' r m
                  | None ->   div[][])
                     
    let view (m : MRoverModel) : DomNode<Action> =
        Static.div[][
            viewRovers m
            br[]
            UI.dropDown'' (roversList m)  m.selectedRover (fun x -> SelectRover (x |> Option.map(fun y -> y.Current |> Mod.force))) (fun x -> (x.id |> Mod.force) )
            Incremental.div AttributeMap.empty (AList.ofModSingle (viewRoverProperties m))
        ]