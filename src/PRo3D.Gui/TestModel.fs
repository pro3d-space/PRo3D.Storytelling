namespace PRo3DGui

open Fablish

module TestModel = 


    let Id = ID()

    
    type OPC = {
        Name : string
        Id   : Id
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

    type UIState = {
        expanded : Set<Id>
        dragging : Option<int>
        selected : Option<Id>
    }
    
    type Model =
      { 
        Scene         : Scene
        UI            : UIState
        Surface       : Surface
      }
      
      static member initial =
    
          let shaderChoices : Choice.Model = {
            choices = ["Lamber"; "Phong"; "Oren"]
            selected = "Phong"
          }

          let texChoices : Choice.Model = {
            choices = ["ortho"; "spectral"]
            selected = "ortho"
          }

          let mapChoices : Choice.Model = {
            choices = ["Accuracy"; "Distance"]
            selected = "Distance"
          }

          let modeChoices : Choice.Model = {
            choices = ["solid"; "wireframe"; "point"]
            selected = "wireframe"
          }

          let visible : Toggle.Model = {
            isActive = true            
          }         

          let trafo : Transformation.Model = {
             translation =  { x = Numeric.initial; y= Numeric.initial; z = Numeric.initial }
             rotation    =  { x = Numeric.initial; y= Numeric.initial; z = Numeric.initial }
             scale       =  { x = Numeric.initial; y= Numeric.initial; z = Numeric.initial }
          }

          let surfaces = [
              {
                  Id                = Id.New()
                  Name              = "My favorite Surface" 
                  IsActive          = {
                                        isActive = false                                      
                                      }         
                  IsVisible         = {
                                        isActive = true                                        
                                      }
                  ShadingMode       = shaderChoices
                  Texture           = texChoices
                  Map               = mapChoices
                  RenderMode        = modeChoices
                  TriangleThreshold = Numeric.initial
                  Trafo             = trafo
                  //AccTest           = acc
                  OPCs              = []
              }]
    
          let scene = {
              Name = "HarrysScene"
              Path = "daham"
              Surfaces = { Surfaces = surfaces; Id = Id.New() }
              Id = Id.New()
          }
    
          {
              Scene = scene
              UI = { expanded = Set.ofList Id.All; dragging = None; selected = None }
              Surface = surfaces.Head
          }              

