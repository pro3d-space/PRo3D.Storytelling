namespace Aardvark.Base.Incremental


[<AutoOpen>]
module ModExtensions =
    module Mod =
        let unwrapOption (f : 'a -> IMod<'b>) (x : IMod<'a option>) : IMod<'b option> =
            adaptive {
                let! x = x
                match x with
                | Some x -> return! x |> f |> Mod.map Some
                | None -> return None
            }