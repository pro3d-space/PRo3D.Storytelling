namespace Provenance.Abstraction.Message

open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D.Viewer
open PRo3D.Drawing
open PRo3D.Surfaces
open PRo3D.Groups

open Model
open Provenance.Abstraction.State

type OMessage = AppAction

[<DomainType>]
type Message =
    | AddAnnotation
    | EditAnnotation
    | DeleteAnnotation
    | LoadAnnotations
    | GroupAction
    | EditSurface
    | Unknown 

    override x.ToString () =
        match x with
            | AddAnnotation     -> "A"
            | EditAnnotation    -> "E"
            | DeleteAnnotation  -> "D"
            | LoadAnnotations   -> "L"
            | GroupAction       -> "G"
            | EditSurface       -> "S"
            | Unknown           -> ""

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create (current : State) (next : State) (msg : OMessage) =
        let f1 = current.annotations.flat
        let f2 = next.annotations.flat
        let c1 = HMap.count f1
        let c2 = HMap.count f2

        match msg with
            | ImportAnnotationGroups _
            | DrawingMessage (LoadVersioned _) ->
                LoadAnnotations
            | SurfaceActions (SurfaceApp.SurfacePropertiesMessage (SurfaceProperties.SetPriority _)) 
            | SurfaceActions (SurfaceApp.SurfacePropertiesMessage (SurfaceProperties.ToggleVisible _)) 
            | SurfaceActions (SurfaceApp.GroupsMessage (Groups.Action.ToggleGroup _)) 
            | SurfaceActions (SurfaceApp.GroupsMessage (Groups.Action.ToggleChildVisibility _)) ->
                EditSurface
            | _ ->
                if c2 > c1 then
                    AddAnnotation                   // One or more annotations were added
                else if c2 < c1 then
                    DeleteAnnotation                // One or more annotations were deleted
                else
                    if f1 <> f2 then
                        EditAnnotation              // The map of annotations changed, i.e. at least one annotation was edited
                    else
                        if current <> next then
                            GroupAction             // The annotations remained unchanged, so the grouping must have changed
                        else
                            Unknown                 // No relevant changes detected

    // Returns if two messages of the given type can be coalesced; i.e. can
    // an existing node with the same message be modified instead of adding a new node
    let coalesce = function
        | AddAnnotation -> false
        | _ -> true

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OMessage =

    let isRelevant = function
        | DrawingMessage _
        | AnnotationMessage _ 
        | AnnotationGroupsMessageViewer _ 
        | ImportAnnotationGroups _
        | SurfaceActions (SurfaceApp.SurfacePropertiesMessage (SurfaceProperties.SetPriority _)) 
        | SurfaceActions (SurfaceApp.SurfacePropertiesMessage (SurfaceProperties.ToggleVisible _))
        | SurfaceActions (SurfaceApp.GroupsMessage (Groups.Action.ToggleGroup _)) 
        | SurfaceActions (SurfaceApp.GroupsMessage (Groups.Action.ToggleChildVisibility _)) ->
            true
        | _ -> false

    let isCamera = function
        | SetCamera _
        | SetCameraAndFrustum _
        | SetCameraAndFrustum2 _
        | NavigationMessage _ -> true
        | _ -> false