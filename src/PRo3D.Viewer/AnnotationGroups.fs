namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI

open System

module AnnotationGroups =

    let addGroup (model:AnnotationGroupsModel) (name:string) =
        let newGroup = {
            name = name
            key =  Guid.NewGuid()
            annotations = plist.Empty
            subGroups = plist.Empty
            parentKey = model.activeGroup.id
            visible = true
            }
        model.rootGroup.dDepthFirst().OfType<AnnotationGroup>().SingleOrDefault(x => x.Id == id);

    //let addAnnotation (model:AnnotationGroupsModel) (am:Annotation) =

    type Action =
        | ActiveGroup    of Guid
        | ActiveSurface  of Guid
        
    let update (model : AnnotationGroupsModel) (act : Action) =
        match act with
            | ActiveGroup g         -> { model with activeGroup = g }
            | ActiveSurface s       -> { model with activeSurface = s }

