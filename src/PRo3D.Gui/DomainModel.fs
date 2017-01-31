namespace PRo3DGui

open Fablish
open Aardvark.Base
open Aether
open Aether.Operators
open GuiExperiments

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

        let empty =
            {
                Name = "empty"
                Surfaces = { Surfaces = []; Id = Id.New() }
                Id = Id.New()
            }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surface =

        let defaultSurface () = 
            {
                Id                  = Id.New()
                Name                = "default"
                IsActive            = { isActive = true}
                IsVisible           = { isActive = false}
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

    type Properties =
        | SurfaceProperties of Surface
        | SceneProperties of Scene        
        | NoProperties

    type UIState = {
        expanded : Set<Id>
        dragging : Option<Id>
        selected : Properties        
    } with
        static member selected_ =
            (fun m -> m.selected), (fun sel (m : UIState) -> { m with selected = sel})
    
    type DomainModel = { 
        Scene             : Scene
        Path              : Option<string>
        UI                : UIState
   //     SelectedLense     : Lens<DomainModel, Properties>
    } with
        static member Scene_ =
            (fun m -> m.Scene), (fun scene m -> { m with Scene = scene})
        static member Path_ =
            (fun m -> m.Path), (fun path (m:DomainModel) -> { m with Path = path})
        static member UI_ =
            (fun m -> m.UI), (fun ui m -> { m with UI = ui})
        static member select_ =
             DomainModel.UI_ >-> UIState.selected_

    let empty = 
      let scene = 
          {
              Name = "New Scene"
              Surfaces = { Surfaces = []; Id = Id.New() }
              Id = Id.New()
          }
    
      { Scene = scene         
        Path = None   
        UI = { expanded = Set.ofList Id.All; dragging = None; selected = NoProperties }
        }

    let addDiscoveredSurfaces (discovered : DiscoveredSurfaces) (model : DomainModel) =
        let newSurface = { Surface.defaultSurface () with 
                                Id   = Id.New()
                                Name = System.IO.Path.GetFileName(discovered.currentPath) 
                                OPCs = OPC.fromPaths discovered.discoveredSurfaces }
        { model with Scene = Scene.addsurface newSurface model.Scene; }, newSurface       

    type FullModel = {
        DomainModel       : DomainModel
        importSurfaces    : ImportDiscoverApp.Model               
    } with
        static member DomainModel_ =
            (fun fm -> fm.DomainModel), (fun dm fm -> { fm with DomainModel = dm})
        static member Selected_ = FullModel.DomainModel_ >-> DomainModel.UI_ >-> UIState.selected_

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FullModel = 
        let initial = {
            DomainModel       = empty
            importSurfaces    = ImportDiscoverApp.app.initial               
        }