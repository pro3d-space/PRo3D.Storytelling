namespace Provenance.Abstraction.Surfaces

open System

open Aardvark.Base
open Aardvark.Base.Incremental

open PRo3D.Surfaces
open PRo3D.Groups

type OSurface = Surface

[<DomainType>]
type Surface = {
    visible : bool
    priority : float
}

type OSurfaces = GroupsModel

[<DomainType; CustomEquality; NoComparison>]
type Surfaces = 
    { flat : hmap<Guid, Surface> }

    override x.GetHashCode () =
        hash x.flat

    override x.Equals y =
        let def = {
            visible = true
            priority = 0.0
        }

        // Comparing surfaces is a bit tricky because we don't actually keep track of all surfaces
        // If a surface is missing in a abstraction state, it is assumed to have the default values defined above
        // Two states differ only if they explicitly assign different values to the same surface, or if one assigns non-default values
        // while the other state does not contain the surface (i.e. implicitly assigns default values)
        let cmp a b =
            (a |> Option.defaultValue def) = (b |> Option.defaultValue def)

        match y with 
            | :? Surfaces as y -> HMap.map2 (fun _ a b -> cmp a b) x.flat y.flat |> HMap.forall (fun _ x -> x)
            | _ -> false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =

    let create (s : OSurface) : Surface = {
        visible  = s.isVisible
        priority = s.priority.value
    }

    let restore (orig : OSurface) (s : Surface) : OSurface = {
        orig with isVisible = s.visible
                  priority = { orig.priority with value = s.priority }
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surfaces =

    let private getSurface = function
        | Surfaces s -> s
        | _ -> raise (ArgumentException ("Expected an annotation"))

    let create (s : OSurfaces) = {
        flat = s.flat |> HMap.map (fun _ l -> l |> getSurface |> Surface.create)
    }
        
    let restore (current : OSurfaces) (surfaces : Surfaces) : OSurfaces =
        let f = 
            current.flat |> HMap.map (fun k s ->
                match surfaces.flat |> HMap.tryFind k with
                    | None -> s
                    | Some x -> Surfaces (Surface.restore (getSurface s) x)
            )

        { current with flat = f }