namespace Abstraction.RList

open Aardvark.Base

// We want to compare the raw content of lists with the (=) operator; plist contains
// indices that might change by removing and adding elements, while the elements themselves remain
// the same.
[<CustomEquality; NoComparison>]
type rlist<'a when 'a : equality> =
    { inner : 'a plist }

    override x.GetHashCode () =
        x.inner |> PList.toList |> hash

    override x.Equals y =
        match y with 
            | :? rlist<'a> as y -> PList.toList x.inner = PList.toList y.inner
            | _ -> false
