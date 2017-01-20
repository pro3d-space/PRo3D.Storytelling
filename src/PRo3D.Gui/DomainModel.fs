namespace PRo3DGui

open Fablish

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

    type Surface = {
        Id                  : Id
        Name                : string
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
    
    type Scene = {
        Name     : string
        Path     : string     
        Surfaces : Surfaces
        Id       : Id
    }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Scene =
        let addsurface (surf : Surface) (scene : Scene) = 
            { scene with Surfaces = { scene.Surfaces with Surfaces = surf :: scene.Surfaces.Surfaces  } }

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
            }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module OPC =

        let fromPaths (paths : list<string>) : list<OPC> =
            paths |> List.map (fun p ->  { name = p; id = Id.New() } )

    type UIState = {
        expanded : Set<Id>
        dragging : Option<int>
        selected : Option<Id>
    }
    
    type Model =
      { 
        Scene         : Scene
        UI            : UIState
      }

    let empty = 
      let scene = 
          {
              Name = "New Scene"
              Path = "undefined"
              Surfaces = { Surfaces = []; Id = Id.New() }
              Id = Id.New()
          }
    
      { Scene = scene; UI = { expanded = Set.ofList Id.All; dragging = None; selected = None };}              


    let addDiscoveredSurfaces (discovered : DiscoveredSurfaces) (model : Model) =
        let newSurface = { Surface.defaultSurface () with  Name = System.IO.Path.GetFileName(discovered.currentPath); OPCs = OPC.fromPaths discovered.discoveredSurfaces }
        { model with Scene = Scene.addsurface newSurface model.Scene; }, newSurface
        
//
      
//      static member initial =
//    
//          let shaderChoices : Choice.Model = {
//            choices = ["Lamber"; "Phong"; "Oren"]
//            selected = "Phong"
//          }
//
//          let texChoices : Choice.Model = {
//            choices = ["ortho"; "spectral"]
//            selected = "ortho"
//          }
//
//          let mapChoices : Choice.Model = {
//            choices = ["Accuracy"; "Distance"]
//            selected = "Distance"
//          }
//
//          let modeChoices : Choice.Model = {
//            choices = ["solid"; "wireframe"; "point"]
//            selected = "wireframe"
//          }
//
//          let visible : Toggle.Model = {
//            isActive = true            
//          }         
//
//          let trafo : Transformation.Model = {
//             translation =  { x = Numeric.initial; y= Numeric.initial; z = Numeric.initial }
//             rotation    =  { x = Numeric.initial; y= Numeric.initial; z = Numeric.initial }
//             scale       =  { x = Numeric.initial; y= Numeric.initial; z = Numeric.initial }
//          }
//
//          let surfaces = [
//              {
//                  Id                = Id.New()
//                  Name              = "My favorite Surface" 
//                  IsActive          = {
//                                        isActive = false                                      
//                                      }         
//                  IsVisible         = {
//                                        isActive = true                                        
//                                      }
//                  ShadingMode       = shaderChoices
//                  Texture           = texChoices
//                  Map               = mapChoices
//                  RenderMode        = modeChoices
//                  TriangleThreshold = Numeric.initial
//                  Trafo             = trafo
//                  //AccTest           = acc
//                  OPCs              = []
//              }]
//    
//          let scene = {
//              Name = "HarrysScene"
//              Path = "daham"
//              Surfaces = { Surfaces = surfaces; Id = Id.New() }
//              Id = Id.New()
//          }
//    
//          {
//              Scene = scene
//              UI = { expanded = Set.ofList Id.All; dragging = None; selected = None }
//              Surface = surfaces.Head
//          }              
//
