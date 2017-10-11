namespace PRo3d

open Aardvark.Base

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

type Instrument = {
    id                      : string
    iType                   : InstrumentType
    calibratedFocalLengths  : list<double>
    currentFocalLength      : double
    intrinsics              : Intrinsics
    extrinsics              : Extrinsics   
    index                   : int
}

type Axis = {
    id           : string
    description  : string
    startPoint   : V3d
    endPoint     : V3d
    minAngle     : double
    maxAngle     : double
    currentAngle : double

    index        : int
}
