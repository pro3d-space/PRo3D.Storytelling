namespace Provenance.Abstraction.State

open Aardvark.Base.Incremental
open Model

open Provenance.Abstraction.Annotations
open Provenance.Abstraction.Surfaces

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
        { current with drawing = { current.drawing with annotations = Annotations.restore current.drawing.annotations s.annotations } 
                       scene = { current.scene with surfacesModel = { current.scene.surfacesModel with surfaces = Surfaces.restore current.scene.surfacesModel.surfaces s.surfaces } } }