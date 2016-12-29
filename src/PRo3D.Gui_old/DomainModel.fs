namespace PRo3D.Gui

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser


[<AutoOpen>]
module FableCompatToBeRemoved = 
    type V3d = float * float * float
    type Trafo3d = string

//type Group<'a, 'b> =
//    | Group of list<Group<'a, 'b>> * 'b * 'a

[<AutoOpen>]
module Measurements = 
    type ProjectionMode = Linear | Viewpoint | Ortho

    type MeasurementBase = {
        Id        : string
      //  Trafo     : Trafo3d
    }

//    type Measure = 
//        | Point of V3d 
//        | Line of V3d * V3d
//        | Polyline of V3d list
//        | Polygon of V3d list
//        | DipAndStrike of V3d list

    type Measurement = {
       // Measure     : Measure
        Base        : MeasurementBase
        //Viewpoint   : Viewpoint
    }

    type MeasurementGroup = {
        Name         : string
        Measurements : Measurement list
        Groups       : MeasurementGroup list
    }
        //| Group of string * list<MeasurementGroup> * list<Measurement>

module TestModel = 
 
  type Opc = {
    Name : string
  }

  type Surface = {
    Name : string
    IsActive : bool
    IsVisible : bool
    OPCs : Opc list
  }

  type Scene = {
     Name        : string
     Path        : string     
     Surfaces    : Surface list
     Measurements : MeasurementGroup
  }

  type Model =
    { Scene: Scene }
    
    static member initial =

        let rootGroup = {
            Name = "Measurements"
            Measurements = [
                            {
                                Base = {Id = "Measreument 1"}
                            }]
            Groups = []
        }

        let surfaces = [ 
            {
                Name = "surf 3" 
                IsActive = true
                IsVisible = true
                OPCs = [{
                            Name = "OPC1"
                        }
                        {
                            Name = "OPC2"
                        }]
            }
            {
                Name = "surf 2" 
                IsActive = true
                IsVisible = false
                OPCs = [{
                            Name = "OPC3"
                        }
                        {
                            Name = "OPC4"
                        }]
            }]

        let scene = {
            Name = "HarrysScene"
            Path = "daham"
            Surfaces = surfaces
            Measurements = rootGroup
        }

        {
            Scene = scene          
        }
    

//[<AutoOpen>]
//module Navigations = 
//    
//    type NavigationMode = FreeFly | Explore | Orbit
//
//    type Viewpoint = {
//        Position : V3d
//        LookAt   : V3d
//        Up       : V3d
//        Sky      : V3d
//    }
//
//    type Bookmark = {
//        Name : string
//        Viewpoint : Viewpoint
//        NavMode : NavigationMode
//    } 
//
//    type Waypoint = {
//        Point : Viewpoint
//        Speed : double
//        T     : double
//    }
//
//[<AutoOpen>]
//module Measurements = 
//    type ProjectionMode = Linear | Viewpoint | Ortho
//
//    type MeasurementBase = {
//        Id        : string
//        Trafo     : Trafo3d
//    }
//
//    type Measure = 
//        | Point of V3d 
//        | Line of V3d * V3d
//        | Polyline of V3d list
//        | Polygon of V3d list
//        | DipAndStrike of V3d list
//
//    type Measurement = {
//        Measure     : Measure
//        Base        : MeasurementBase
//        Viewpoint   : Viewpoint
//    }
//
//    type MeasurementGroup =
//        | Group of string * list<MeasurementGroup> * list<Measurement>
//
////Todo model SceneObjects
//[<AutoOpen>]
//module SceneObjects =  
//    type SceneObject = int
//
//[<AutoOpen>]
//module Surfaces = 
//    type OPC = {
//        Id          : string
//    }
//    
//    type SurfaceAttributes = {
//        Id          : string
//        Trafo       : Trafo3d
//        IsActive    : bool
//        IsVisible   : bool
//        OPCs        : OPC list
//    }
//    
//    type SurfaceRenderParameters = {
//        ShadingMode         : string
//        Texture             : string
//        Map                 : string
//        RenderMode          : string
//        TriangleThreshold   : double
//    }
//
//    type Surface = {
//        Attributes       : SurfaceAttributes
//        RenderParameters : SurfaceRenderParameters
//    }
//
//    type SurfaceGroup =
//        | Group of string * list<SurfaceGroup> * list<Surface>
//
//[<AutoOpen>]
//module Paths =
//    type Trajectory = int
//    type CameraPath = int
//    type AnimationPath = int
//
////todo expand ViewPlanner types
//[<AutoOpen>]
//module ViewPlanner = 
//    type Instrument = {
//        Id : string
//    }
//
//    type Rover = {
//        Id          : string
//        Instruments : list<Instrument>
//    }
//
//    type ViewPlan = {
//        Rover     : Rover
//        Trafo     : Trafo3d
//        Viewpoint : Viewpoint
//    }
//
//module ScaleBars = 
//    type Orientation = Up | Right
//
//    type Unit = Mm | Cm | M | Km
//
//    type ScaleBar = {
//        Length      : double
//        Unit        : Unit
//        Orientation : Orientation
//        Position    : V3d
//        Viewpoint   : Viewpoint
//    }
//
//[<AutoOpen>]
//module ViewConfig =
//    type Visual = {
//        LodColorling       : bool
//        AutoClippingPlanes : bool
//        NearPlane          : double
//        FarPlane           : double
//
//        ShowUp             : double
//        ShowNorth          : double
//        ArrowLength        : double
//        LineLength         : double
//        PlaneSize          : double
//
//        MinDipAngle        : double
//        MaxDipAngle        : double
//    }
//
//    type Navigation = {
//        Sensitivity : double
//    }
//
//    type Data = {
//        ComputeNorthUp : bool
//        North          : V3d
//        Up             : V3d
//    }
//
//    type ViewConfig = {
//        Visual      : Visual
//        Navigation  : Navigation
//        Data        : Data
//    }
//
//[<AutoOpen>]
//module Scene =
//    type Scene = {
//        Surfaces     : SurfaceGroup
//        SceneObjects : SceneObject list
//        Measurements : MeasurementGroup      
//        Bookmarks    : Bookmark list
//        ViewConfig   : ViewConfig
//    }
//
//[<AutoOpen>]
//module Application = 
//    type InteractionMode = PickOrbitCenter | PickMeasurement | PlaceSceneObject | PlaceScaleBar | PlaceRover | MayaControl
//
//    type ApplicationState = {
//        NavigationMode : NavigationMode
//        InteractionMode : InteractionMode
//        CurrentScene : Scene
//    }
//    
////Group<list<Measurement>, string>
//
