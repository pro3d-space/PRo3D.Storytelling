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

open PRo3D
open PRo3d
open PRo3D.Surfaces
open PRo3D.Navigation2
//open PRo3D.BookmarksX
open PRo3D.Groups
open PRo3D.ReferenceSystem
open Aardvark.Base.Frustum
open Aardvark.UI.Trafos
open PRo3d.Align
open Aardvark.UI.Animation

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
    //| AlignPick             = 5
    //| PlaceSurface          = 6

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
    | ImportObject              of list<string>
    | ImportAnnotationGroups    of list<string>
    | ImportSurfaceTrafo        of list<string>
    | ImportRoverPlacement      of list<string>
    | SwitchViewerMode
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
    | AlignmentActions          of PRo3d.Align.AlignmentActions
    | SetTabMenu                of TabMenu
    | OpenSceneFileLocation     of string
    | NoAction                  of string
    | OrientationCube           of ISg
    | UpdateDockConfig          of DockConfig
    | AddPage                   of DockElement    
    | ToggleOrientationCube
and MailboxState = {
  events  : list<MailboxAction>
  update  : seq<ViewerAction> -> unit
}
and MailboxAction =
  | ViewerAction  of ViewerAction
  | InitMailboxState of MailboxState
  | DrawingAction of PRo3D.Drawing.Action
  
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

type ViewerMode =
    | Standard
    | Instrument





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
    
    [<TreatAsValue>]
    past : Option<Drawing.DrawingModel> 

    [<TreatAsValue>]
    future : Option<Drawing.DrawingModel> 
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
        

    let messagingMailbox (cancelTokenSrc:CancellationTokenSource) (inbox:MessagingMailbox) =
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
    
    let initialDockConfig = 
      config {
          content (                        
              horizontal 1.0 [                                                        
                stack 0.7 None [
                    {id = "render"; title = Some " Main View "; weight = 0.6; deleteInvisible = None; isCloseable = None}
                    {id = "instrumentview"; title = Some " Instrument View "; weight = 0.6; deleteInvisible = None; isCloseable = None}
                ]                            
                vertical 0.3 [
                  stack 0.5 None [
                    //{id = "scene"; title = Some "Scene"; weight = 0.4; deleteInvisible = None }
                    {id = "surfaces"; title = Some " Surfaces "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "annotations"; title = Some " Annotations "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                  ]                          
                  stack 0.5 None [
                    {id = "config"; title = Some " Config "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "bookmarks"; title = Some " Bookmarks"; weight = 0.4; deleteInvisible = None; isCloseable = None }
                    {id = "viewplanner"; title = Some " ViewPlanner "; weight = 0.4; deleteInvisible = None; isCloseable = None }
                  ]
                ]
              ]              
          //  ]
          )
          appName "PRo3D"
          useCachedConfig false
      }

    let initial msgBox = {           
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
            dockConfig      = initialDockConfig                          
            closedPages     = list.Empty                
        }

        drawing         = Drawing.DrawingModel.initialdrawing
        properties      = NoProperties
        interaction     = Interactions.DrawAnnotation
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
        
        //instrumentCamera = { CameraController.initial with view = CameraView.lookAt V3d.Zero V3d.One V3d.OOI }        
        //instrumentFrustum = Frustum.perspective 60.0 0.1 10000.0 1.0
        viewerMode = ViewerMode.Standard        
    }