namespace PRo3d

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Aardvark.UI
open ViewPlanner
open FShade.Intrinsics
open Aardvark.Base.CameraView


type Intrinsics = {
    horizontalFieldOfView           : double
    verticalFieldOfView             : double
    horizontalResolution            : uint32
    verticalResolution              : uint32
    horizontalPrinciplePoint        : double
    verticalPrinciplePoint          : double
    horizontalFocalLengthPerPixel   : double
    verticalFocalLengthPerPixel     : double
    horizontalDistortionMap         : string
    verticalDistortionMap           : string
    vignettingMap                   : string    
}

[<DomainType>]
type Extrinsics = {
    position    : V3d
    camUp       : V3d
    camLookAt   : V3d
    box         : Box3d
}

module Extrinsics =
    let transformed (t:M44d) (ex:Extrinsics)=
        {
            position = t.TransformPos ex.position
            camUp    = t.TransformDir ex.camUp
            camLookAt= t.TransformDir ex.camLookAt
            box      = ex.box.Transformed t
        }

type InstrumentType = 
    | WACL       // Left Wide Angle Camera (PanCam)        
    | WACR       // Right Wide Angle Camera (PanCam)
    | HRC        // High Resolution Camera
    | WISDOM     // penetrating radar
    | CLUPI      // Close UP Imager microscope
    | ISEM       // Infrared Spectrometer
    | DRILL      // Drill
    | RIM        // Rover Inspection Mirror
    | Undefined

[<DomainType>]
type Instrument = {
    id                      : string
    iType                   : InstrumentType
    calibratedFocalLengths  : list<double>
    //currentFocalLength      : double
    focal                   : NumericInput
    intrinsics              : Intrinsics
    extrinsics              : Extrinsics   
    index                   : int
}

type AxisAngleUpdate = {
    roverId : string
    axisId  : string
    angle   : double
}

type InstrumentFocusUpdate = {
    roverId       : string
    instrumentId  : string
    focal         : double
}

[<DomainType>]
type Axis = {
    id           : string
    description  : string
    startPoint   : V3d
    endPoint     : V3d
    //minAngle     : double
    //maxAngle     : double
    //currentAngle : double

    index        : int
    angle        : NumericInput
}

[<DomainType>]
type Rover = {
    id               : string
    platform2Ground  : M44d
    wheelPositions   : list<V3d>
    instruments      : hmap<string, Instrument>
    axes             : hmap<string, Axis>
    box              : Box3d   
}

[<DomainType>]
type RoverModel = {
    rovers             : hmap<string, Rover>
    platforms          : hmap<string, IPWrapper.SPlatform>
    selectedRover      : option<Rover>
    //selectedInstrument : option<Instrument>
    //selectedAxis       : option<Axis>
    //currentAngle       : NumericInput
}

[<DomainType>]
type ViewPlan = {
    [<PrimaryKey>]
    id                  : Guid
    name                : string
    position            : V3d
    lookAt              : V3d
    viewerState         : CameraControllerState
    vectorsVisible      : bool
    rover               : Rover
    roverTrafo          : Trafo3d
    isVisible           : bool
    selectedInstrument  : option<Instrument>
    selectedAxis        : option<Axis>
    currentAngle        : NumericInput
}

[<DomainType>]
type ViewPlanModel = {
    viewPlans           : hset<ViewPlan>
    selectedViewPlan    : Option<ViewPlan>
    working             : list<V3d> // pos + lookAt
    roverModel          : RoverModel
    instrumentCam       : CameraControllerState
    instrumentFrustum   : Frustum
    }

module ViewPlanModel =
    let currentAngle = 
        {
            value = 0.0
            min =  0.0
            max = 90.0
            step = 0.1
            format = "{0:0.0}"
        }

    let initRoverModel = {
        rovers = hmap.Empty
        platforms = hmap.Empty
        selectedRover = None
        //selectedInstrument = None
        //selectedAxis = None
        //currentAngle = currentAngle
        }

    let initial = {
        viewPlans = hset.Empty
        selectedViewPlan = None
        working = list.Empty
        roverModel = initRoverModel
        instrumentCam = { CameraController.initial with view = CameraView.lookAt V3d.Zero V3d.One V3d.OOI }        
        instrumentFrustum = Frustum.perspective 60.0 0.1 10000.0 1.0
        }

  