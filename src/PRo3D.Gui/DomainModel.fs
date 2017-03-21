namespace PRo3DGui

open Aardvark.Base.Incremental
open Fablish
open Aardvark.Base
open Aether
open Aether.Operators
open GuiExperiments


module DomainModel = 

    let mutable Id = ID()
    
    type OPC = {
        name : string
        id   : Id
        path : Path
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
        Description         : string
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
    } with
        member x.ToMod(cache : ReuseCache) : MSurface =
            {
                IsVisible = Mod.init x.IsActive.isActive
            }
    and MSurface = {
        IsVisible : ModRef<bool>
    } with
        member x.Apply(a : Surface, cache : ReuseCache) =
            x.IsVisible.Value <- a.IsVisible.isActive
          
    type Surfaces = { 
        Surfaces : pset<Surface>
        Id       : Id
    }
    and MSurfaces = {
        MSurfaces : ResetSet<Surface>
    } with 
        member x.Apply(s : Surfaces, cache : ReuseCache) =
            x.MSurfaces.Update(s.Surfaces)
    
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
        Name        : string        
        Description : string
        Surfaces    : Surfaces
        Id          : Id
    } 


//    with
//      static member Path_ =
//            (fun scene -> scene.Path), (fun path (scene : Scene) -> { scene with Path = path})
            

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Scene =
        let addsurface (surf : Surface) (scene : Scene) = 
            { scene with Surfaces = { scene.Surfaces with Surfaces = PSet.add surf scene.Surfaces.Surfaces  } }

        let saveScene (path : string) (scene : Scene) =
            failwith "not implemented"        

        let empty =
            {
                Name = "empty"
                Description = ""
                Surfaces = { Surfaces = PSet.empty; Id = Id.New() }
                Id = Id.New()
            }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surface =

        let defaultSurface () = 
            {
                Id                  = Id.New()
                Name                = "default"
                Description         = ""
                IsActive            = { isActive = true}
                IsVisible           = { isActive = true}
                ShadingMode         = { choices = ["lambert";"oren nayar";"footprint"]; selected = "oren nayar" }
                Texture             = { choices = ["ortho";"height"]; selected = "ortho" }
                Map                 = { choices = ["none";"accuracy";"distance"]; selected = "none" }
                RenderMode          = { choices = ["solid";"wireframe";"point"]; selected = "solid" }
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

        let fromPaths (opcPaths : list<string>) : list<OPC> =
            opcPaths |> List.map (fun p ->  { name = System.IO.Path.GetFileName p; id = Id.New(); path = p } )

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

        HackId            : int
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
              Description ="(no description)"
              Surfaces = { Surfaces = PSet.empty; Id = Id.New() }
              Id = Id.New()
          }
    
      { Scene = scene         
        Path = None   
        UI = { expanded = Set.ofList Id.All; dragging = None; selected = NoProperties }
        HackId = 0 
        }

    let addDiscoveredSurfaces (surfaces : list<Path>) (model : DomainModel) =
        let mutable model = model
        let mutable addedSurfaces = []
        for s in surfaces do
            let opcs = ImportDiscoverApp.discover s
            let newSurface = { Surface.defaultSurface () with 
                                    Id   = Id.New()
                                    Path = s
                                    Name = System.IO.Path.GetFileName s
                                    OPCs = OPC.fromPaths opcs }
            addedSurfaces <- newSurface :: addedSurfaces
            model <- { model with Scene = Scene.addsurface newSurface model.Scene; }
        model, addedSurfaces

    open Aardvark.Base.Incremental

    type NearFar = { near : float; far : float }

    type FullModel = {
        DomainModel       : DomainModel
        importSurfaces    : ImportDiscoverApp.Model    
        composedTest : Scratch.DomainTypes2.Generated.ComposedTest.Model               

        pendingSurfaces : list<Surface>

        loadedSurfaceIds : Set<string>
        camera         : Scratch.DomainTypes.Generated.Camera.Model
        nearFar        : NearFar
        renderSurfaces : pset<Aardvark.SceneGraph.ISg>    
    } with
        static member DomainModel_ =
            (fun fm -> fm.DomainModel), (fun dm fm -> { fm with DomainModel = dm})
        static member Selected_ = FullModel.DomainModel_ >-> DomainModel.UI_ >-> UIState.selected_

        member x.ToMod(reuseCache : Aardvark.Base.Incremental.ReuseCache) = 
            { 
                _original = x 
                composedTest = x.composedTest.ToMod(reuseCache)
                path = Mod.init x.DomainModel.Path

                renderSurfaces = ResetSet(x.renderSurfaces)
                camera = x.camera.ToMod(reuseCache)
                nearFar = Mod.init x.nearFar
            }

    and MFullModel = { 
        mutable _original : FullModel
        path : ModRef<Option<string>>
        composedTest : Scratch.DomainTypes2.Generated.ComposedTest.MModel

        camera         : Scratch.DomainTypes.Generated.Camera.MModel
        nearFar : ModRef<NearFar>
        renderSurfaces : ResetSet<Aardvark.SceneGraph.ISg>
    } with
        member x.Apply(arg0 : FullModel,cache : Aardvark.Base.Incremental.ReuseCache) = 
            x.composedTest.Apply(arg0.composedTest,cache)
            x.path.Value <- arg0.DomainModel.Path
            x.renderSurfaces.Update(arg0.renderSurfaces)
            x.camera.Apply(arg0.camera,cache)
            x.nearFar.Value <- arg0.nearFar

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FullModel = 
        let initial = {
            DomainModel       = empty
            importSurfaces    = ImportDiscoverApp.app.initial   
            composedTest      = Scratch.ComposedTestApp.initial            
            renderSurfaces = PSet.empty
            camera = Scratch.FreeFlyCameraApp.initial
            nearFar = { near = 0.1; far = 1000000.0 }
            loadedSurfaceIds = Set.empty

            pendingSurfaces = []
        }