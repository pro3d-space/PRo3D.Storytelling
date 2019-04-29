namespace PRo3D.Viewer

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Mutable
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis

open Aardvark.Base.Frustum
open Aardvark.UI.Trafos
open Aardvark.UI.Animation

open PRo3D
open PRo3d
open PRo3D.Surfaces
open PRo3D.Navigation2
open PRo3D.Groups
open PRo3D.ReferenceSystem
open PRo3d.Align

type TabMenu = 
    | Surfaces    = 0
    | Annotations = 1
    | Viewplanner = 2
    | Bookmarks   = 3
    | Config      = 4

type Interactions =
    | PickExploreCenter     = 0
    | PlaceCoordinateSystem = 1  // compute up north vector at that point
    | DrawAnnotation        = 2
    | PlaceRover            = 3
    | TrafoControls         = 4
    | PlaceSurface          = 5
    | PickAnnotation        = 6
    | PickSurface           = 7
    //| DrawLog               = 7
    //| AlignPick             = 5

//type ViewerMode =
//    | Standard
//    | Instrument
    


type BookmarkAction =
  | AddBookmark 
  | GroupsMessage   of Groups.Action

type PropertyActions =
    | DrawingMessage    of Drawing.Action
    | AnnotationMessage of AnnotationProperties.Action

type ViewerAction =                
    | DrawingMessage            of Drawing.Action
    | AnnotationGroupsMessageViewer   of Groups.Action
    | NavigationMessage         of Navigation.Action
    | AnimationMessage          of AnimationAction
    | ReferenceSystemMessage    of ReferenceSystemApp.Action
    | AnnotationMessage         of AnnotationProperties.Action
    | BookmarkMessage           of BookmarkAction
    | BookmarkUIMessage         of Groups.Action
    | RoverMessage              of RoverApp.Action
    | ViewPlanMessage           of ViewPlan.Action
    | DnSColorLegendMessage     of FalseColorLegendApp.Action
    | SetCamera                 of CameraView        
    | SetCameraAndFrustum       of CameraView * double * double        
    | SetCameraAndFrustum2      of CameraView * Frustum
    | ImportSurface             of list<string>
    | ImportDiscoveredSurfaces  of list<string>
    | ImportDiscoveredSurfacesThreads  of list<string>
    | ImportObject              of list<string>
    | ImportAnnotationGroups    of list<string>
    | ImportSurfaceTrafo        of list<string>
    | ImportRoverPlacement      of list<string>
    | SwitchViewerMode          of ViewerMode
    | DnSProperties             of PropertyActions
    | ConfigPropertiesMessage   of ConfigProperties.Action
    | QuickLoad1
    | QuickLoad2
    | DeleteLast
    | AddSg                     of ISg
    | PickSurface               of SceneHit*string
    | PickObject                of V3d*Guid
    | SaveScene                 of string
    | SaveAs                    of string
    | OpenScene                 of list<string>
    | LoadScene                 of string
    | NewScene
    | KeyDown                   of key : Aardvark.Application.Keys
    | KeyUp                     of key : Aardvark.Application.Keys      
    | SetKind                   of TrafoKind
    | SetInteraction            of Interactions        
    | SetMode                   of TrafoMode
    | Translate                 of string * TrafoController.Action
    | Rotate                    of string * TrafoController.Action
    | SurfaceActions            of SurfaceApp.Action
    | MinervaActions            of PRo3D.Minerva.MinervaAction
    | AlignmentActions          of PRo3d.Align.AlignmentActions
    | SetTabMenu                of TabMenu
    | OpenSceneFileLocation     of string
    | NoAction                  of string
    | OrientationCube           of ISg
    | UpdateDockConfig          of DockConfig
    | AddPage                   of DockElement    
    | ToggleOrientationCube
    | UpdateUserFeedback        of string
    | StartImportMessaging      of list<string>
    | Logging                   of string * ViewerAction
    | ThreadsDone               of string    
    | OnResize                  of V2i


and MailboxState = {
  events  : list<MailboxAction>
  update  : seq<ViewerAction> -> unit
}
and MailboxAction =
  | ViewerAction  of ViewerAction
  | InitMailboxState of MailboxState
  | InitMinervaBox of PRo3D.Minerva.MailboxState
  | DrawingAction of PRo3D.Drawing.Action
 // | MinervaMbAction of PRo3D.Minerva.MailboxAction


//[<DomainType>] 
//type UserFeedback = {
//    loadScene : string
//    saveScene : string
//    noText    : string
//}

[<DomainType>] 
type Scene = {
    navigation        : NavigationModel
    interaction       : InteractionMode
    surfacesModel     : SurfaceModel  
    config            : ViewConfigModel
    scenePath         : Option<string>
    referenceSystem   : ReferenceSystem
    //orientationSystem : ReferenceSystem
    bookmarksModel    : BookmarksModel
    viewPlans         : ViewPlanModel
    dockConfig        : DockConfig
    closedPages       : list<DockElement>
    firstImport       : bool
    userFeedback      : string
    feedbackThreads   : ThreadPool<ViewerAction> 
}

[<DomainType>] 
type SceneHandle = {
    path        : string
    name        : string
    writeDate   : DateTime
}

[<DomainType>] 
type Recent = {
    recentScenes : list<SceneHandle> //hmap<string,SceneHandle>
}

type Properties = 
    | AnnotationProperties of Annotation
    | NoProperties

type WayPoint = {
    name : string
    cv   : CameraView
}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MailboxState = 
  let empty = 
    {
      events = list.Empty
      update = fun _ -> ()
    }

type MessagingMailbox = MailboxProcessor<MailboxAction>

[<DomainType>]
type Model = { 
   
    scene            : Scene
    drawing          : Drawing.DrawingModel   
    properties       : Properties
    interaction      : Interactions
    picking          : bool
    ctrlFlag         : bool
    recent           : Recent
    waypoints        : plist<WayPoint>
    frustum          : Frustum
    overlayFrustum   : Frustum
    aspect           : double    
                     
    trafoKind        : TrafoKind
    trafoMode        : TrafoMode
                     
    alignment        : AlignmentModel
    tabMenu          : TabMenu
                     
    viewerMode       : ViewerMode
                     
    animations       : AnimationModel

    messagingMailbox : MessagingMailbox
    mailboxState     : MailboxState
    
    minervaModel     : PRo3D.Minerva.Model
            
    [<TreatAsValue>]
    past : Option<Drawing.DrawingModel> 

    [<TreatAsValue>]
    future : Option<Drawing.DrawingModel> 
    footPrint           : FootPrint 
}



module Viewer =
    open System.Threading

    let processMailboxAction (state:MailboxState) (cancelTokenSrc:CancellationTokenSource) (inbox:MessagingMailbox) (action : MailboxAction) =
      match action with
      | MailboxAction.InitMailboxState s ->     
        s
      | MailboxAction.DrawingAction a ->        
        a |> ViewerAction.DrawingMessage |> Seq.singleton |> state.update 
        state
      | MailboxAction.ViewerAction a ->        
        a |> Seq.singleton |> state.update 
        state
        

    let initMessageLoop (cancelTokenSrc:CancellationTokenSource) (inbox:MessagingMailbox) =
        let rec messageLoop state = async {
            let! msg = inbox.Receive()
            
            let newState =
                try msg |> processMailboxAction state cancelTokenSrc inbox
                with
                | e ->
                    Log.line "ViewerMgmt Error: Dropping msg (Exception:  %O)" e
                    state
            
            return! messageLoop newState
        }
        messageLoop MailboxState.empty

    let navInit = 
        let init = NavigationModel.initial
        let init = (NavigationModel.Lens.camera |. CameraControllerState.Lens.sensitivity).Set(init, 3.0)
        let init = (NavigationModel.Lens.camera |. CameraControllerState.Lens.panFactor).Set(init, 0.0008)
        let init = (NavigationModel.Lens.camera |. CameraControllerState.Lens.zoomFactor).Set(init, 0.0008)
        init        

    let sceneElm = {id = "scene"; title = (Some "Scene"); weight = 0.4; deleteInvisible = None; isCloseable = None }
    
    let dockConfigFull = 
      config {
          content (                        
              horizontal 1.0 [                                                        
                stack 0.7 None [
                    {id = "render"; title = Some " Main View "; weight = 0.6; deleteInvisible = None; isCloseable = None}
                    {id = "instrumentview"; title = Some " Instrument View "; weight = 0.6; deleteInvisible = None; isCloseable = None}
                ]                            
                vertical 0.3 [
                  stack 0.5 None [                    
                    {id = "surfaces"; title = Some " Surfaces "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "annotations"; title = Some " Annotations "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "minerva"; title = Some " Minerva "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                  ]                          
                  stack 0.5 (Some "config") [
                    {id = "config"; title = Some " Config "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "bookmarks"; title = Some " Bookmarks"; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "viewplanner"; title = Some " ViewPlanner "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "corr_mappings"; title = Some " RockTypes "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "corr_semantics"; title = Some " Semantics "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                  ]
                ]
              ]              
          )
          appName "PRo3D"
          useCachedConfig false
      }

    let dockConfigCore = 
      config {
          content (                        
              horizontal 1.0 [                                                        
                stack 0.7 None [
                    {id = "render"; title = Some " Main View "; weight = 0.6; deleteInvisible = None; isCloseable = None}                       
                ]                            
                vertical 0.3 [
                  stack 0.5 None [                        
                    {id = "surfaces"; title = Some " Surfaces "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "annotations"; title = Some " Annotations "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                  ]                          
                  stack 0.5 (Some "config") [
                    {id = "config"; title = Some " Config "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "bookmarks"; title = Some " Bookmarks"; weight = 0.4; deleteInvisible = None; isCloseable = None }                       
                  ]
                ]
              ]                        
          )
          appName "PRo3D"
          useCachedConfig false
      }

    //let initFeedback = 
    //     {  loadScene = "loading Scene..."
    //        saveScene = "saveing Scene..."
    //        loadOpcs  = "load opcs..."
    //        noText    = ""
    //     }

    let initial msgBox msgBox2 = {           
       // surfaces = HSet.empty
        scene = 
          {
            navigation      = navInit
            interaction     = InteractionMode.PickOrbitCenter
            surfacesModel   = SurfaceModel.initial
            config          = ViewConfigModel.initial
            scenePath       = None
            referenceSystem = ReferenceSystem.initial
            //orientationSystem = ReferenceSystem.initial
            bookmarksModel  = BookmarksModel.initial
            viewPlans       = ViewPlanModel.initial                                        
            dockConfig      = dockConfigFull
            closedPages     = list.Empty 
            firstImport     = true
            userFeedback    = ""
            feedbackThreads = ThreadPool.empty
        }

        drawing         = Drawing.DrawingModel.initialdrawing
        properties      = NoProperties
        interaction     = Interactions.PlaceCoordinateSystem
        picking         = false
        ctrlFlag        = false

        messagingMailbox = msgBox
        mailboxState = MailboxState.empty

        frustum         = Frustum.perspective 60.0 0.1 10000.0 1.0
        overlayFrustum  = Frustum.perspective 20.0 0.1 10000.0 1.0
        aspect          = 1.6

        recent          = { recentScenes = List.empty }
        waypoints       = PList.empty

        trafoKind       = TrafoKind.Rotate
        trafoMode       = TrafoMode.Local

        alignment       = Alignment.initModel

        past            = None
        future          = None

        tabMenu = TabMenu.Surfaces

        animations = 
         { 
           animations = PList.empty
           animation  = Animate.On
           cam        = CameraController.initial.view
         }

        minervaModel = PRo3D.Minerva.Initial.model msgBox2
        
        //instrumentCamera = { CameraController.initial with view = CameraView.lookAt V3d.Zero V3d.One V3d.OOI }        
        //instrumentFrustum = Frustum.perspective 60.0 0.1 10000.0 1.0
        viewerMode = ViewerMode.Standard                
        footPrint = ViewPlanModel.initFootPrint
    }