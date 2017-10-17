namespace PRo3d

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

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
    selectedRover      : option<Rover>
    selectedInstrument : option<Instrument>
    selectedAxis       : option<Axis>
    currentAngle       : NumericInput
}
