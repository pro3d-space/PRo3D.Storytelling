namespace PRo3D.BookmarksX

open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open PRo3D
open PRo3D.Navigation2
open PRo3D.ReferenceSystem
open Aardvark.Base
open System

[<DomainType>]
type Bookmark = {
    key             : Guid
    name            : string
    navigationState : NavigationModel
}


