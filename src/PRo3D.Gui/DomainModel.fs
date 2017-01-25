namespace PRo3DGui

open Fablish
open Aardvark.Base
open Aether

module DomainModel = 

    let Id = ID()
    
    type OPC = {
        name : string
        id   : Id
    }             

    type Trafo = {
         Translation     : Vector3d
         Rotation        : Vector3d
         Scaling         : Vector3d
    }      

    type Viewpoint = {
        Position : V3d
        LookAt   : V3d
        Up       : V3d
        Sky      : V3d
    }

    type Location = Imported | Remote

    type Surface = {
        Id                  : Id
        Name                : string
        Location            : Location
        Path                : string
        IsActive            : Toggle.Model
        IsVisible           : Toggle.Model
        ShadingMode         : Choice.Model
        Texture             : Choice.Model
        Map                 : Choice.Model
        RenderMode          : Choice.Model
        TriangleThreshold   : Numeric.Model
        Trafo               : Transformation.Model
        OPCs                : OPC list
    }
          
    type Surfaces = { 
        Surfaces : list<Surface>
        Id       : Id
    }
    
    type ProjectionMode = Linear | Viewpoint | Ortho

    type MeasurementBase = {
        Id        : string
        Trafo     : Trafo
    }

    type Measure = 
        | Point of V3d 
        | Line of V3d * V3d
        | Polyline of V3d list
        | Polygon of V3d list
        | DipAndStrike of V3d list

    type Measurement = {
        Measure     : Measure
        Base        : MeasurementBase
        Viewpoint   : Viewpoint
    }

    type MeasurementGroup =
        | Group of string * list<MeasurementGroup> * list<Measurement>

    
    type Scene = {
        Name     : string        
        Surfaces : Surfaces
        Id       : Id        
    } 
//    with
//      static member Path_ =
//            (fun scene -> scene.Path), (fun path (scene : Scene) -> { scene with Path = path})
            

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Scene =
        let addsurface (surf : Surface) (scene : Scene) = 
            { scene with Surfaces = { scene.Surfaces with Surfaces = surf :: scene.Surfaces.Surfaces  } }

        let saveScene (path : string) (scene : Scene) =
            failwith "not implemented"

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surface =

        let defaultSurface () = 
            {
                Id                  = Id.New()
                Name                = "default"
                IsActive            = { isActive = false}
                IsVisible           = { isActive = true}
                ShadingMode         = { choices = ["1";"2";"3"]; selected = "1" }
                Texture             = { choices = ["1";"2";"3"]; selected = "1" }
                Map                 = { choices = ["1";"2";"3"]; selected = "1" }
                RenderMode          = { choices = ["1";"2";"3"]; selected = "1" }
                TriangleThreshold   = Numeric.initial
                Trafo               = { translation = { x = Numeric.initial; y= Numeric.initial; z= Numeric.initial }
                                        rotation = { x = Numeric.initial; y= Numeric.initial; z= Numeric.initial }
                                        scale = { x = Numeric.initial; y= Numeric.initial; z= Numeric.initial }
                                        }
                OPCs                = []
                Path                = ""
                Location            = Location.Remote
            }

        let addOpc (opc : OPC) (surf : Surface) =
           { surf with OPCs = opc :: surf.OPCs }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module OPC =

        let fromPaths (paths : list<string>) : list<OPC> =
            paths |> List.map (fun p ->  { name = p; id = Id.New() } )

    type UIState = {
        expanded : Set<Id>
        dragging : Option<int>
        selected : Option<Id>
    }
    
    type Model = { 
        Scene         : Scene        
        Path          : Option<string>
        UI            : UIState
    } with
        static member Scene_ =
            (fun m -> m.Scene), (fun scene m -> { m with Scene = scene})
        static member Path_ =
            (fun m -> m.Path), (fun path (m:Model) -> { m with Path = path})

    let empty = 
      let scene = 
          {
              Name = "New Scene"              
              Surfaces = { Surfaces = []; Id = Id.New() }
              Id = Id.New()
          }
    
      { Scene = scene         
        Path = None
        UI = { expanded = Set.ofList Id.All; dragging = None; selected = None }}              

    let addDiscoveredSurfaces (discovered : DiscoveredSurfaces) (model : Model) =
        let newSurface = { Surface.defaultSurface () with 
                                Name = System.IO.Path.GetFileName(discovered.currentPath) 
                                OPCs = OPC.fromPaths discovered.discoveredSurfaces }
        { model with Scene = Scene.addsurface newSurface model.Scene; }, newSurface       