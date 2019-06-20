namespace Abstraction.State

open Aardvark.Base.Incremental
open Model

open PRo3D.Groups

open Abstraction.Annotations
open Abstraction.Surfaces

type OState = AppModel

[<DomainType>]
type State = {
    annotations : Annotations
    surfaces : Surfaces
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : OState) =
        { annotations = Annotations.create s.drawing.annotations 
          surfaces = Surfaces.create s.scene.surfacesModel.surfaces }

    let restore (current : OState) (s : State) =
        // Check if the priority of a surface changed; this is required since
        // the sg grouping has to be manually recomputed in this case
        // TODO: Fix this in the surfaces app?
        let surfacePriorityChanged =
            let c = create current
            Surfaces.difference' Surface.comparePriorityWithDefault c.surfaces s.surfaces
                |> List.isEmpty |> not

        let surfaceModel = 
            { current.scene.surfacesModel with surfaces = Surfaces.restore current.scene.surfacesModel.surfaces s.surfaces }
                |> if surfacePriorityChanged then SurfaceModel.triggerSgGrouping else id

        { current with drawing = { current.drawing with annotations = Annotations.restore current.drawing.annotations s.annotations } 
                       scene = { current.scene with surfacesModel = surfaceModel } }