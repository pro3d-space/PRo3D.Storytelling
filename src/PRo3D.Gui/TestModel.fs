namespace PRo3DGui

open Fablish

module TestModel = 


    let Id = ID()

    
    type OPC = {
        Name : string
        Id   : Id
    }             

    type Trafo = {
         Translation     : V3dApp2.Model
         Rotation        : V3dApp2.Model
         Scaling         : V3dApp2.Model
    }  

    type Surface = {
        Id                  : Id
        Name                : string
        IsActive            : ToggleApp.Model
        IsVisible           : ToggleApp.Model
        ShadingMode         : Choice.Model
        Texture             : Choice.Model
        Map                 : Choice.Model
        RenderMode          : Choice.Model
        TriangleThreshold   : NumericApp.Model
        Trafo               : TrafoApp2.Model
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

          let visible : ToggleApp.Model = {
            active = true
            label  = "Is Visible"
          }         

          let trafo : TrafoApp2.Model = {
             translation =  { x = NumericApp.initial; y= NumericApp.initial; z = NumericApp.initial }
             rotation    =  { x = NumericApp.initial; y= NumericApp.initial; z = NumericApp.initial }
             scale       =  { x = NumericApp.initial; y= NumericApp.initial; z = NumericApp.initial }
          }

          let surfaces = [
              {
                  Id                = Id.New()
                  Name              = "My favorite Surface" 
                  IsActive          = {
                                        active = false
                                        label  = "Is Active"
                                      }         
                  IsVisible         = {
                                        active = true
                                        label  = "Is Visible"
                                      }
                  ShadingMode       = shaderChoices
                  Texture           = texChoices
                  Map               = mapChoices
                  RenderMode        = modeChoices
                  TriangleThreshold = NumericApp.initial
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

