namespace Aardvark.Base

[<AutoOpen>]
module HMapExtensions =
    module HMap =

        let unionMany (maps : hmap<'a, 'b> seq) = 
            Seq.fold HMap.union HMap.empty maps