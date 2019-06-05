namespace Bookmarks

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open View
open Provenance

type BookmarkId = 
    private BookmarkId of Guid with

    static member generate () =
        BookmarkId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        BookmarkId v

    static member parse (s : string) =
        s |> Guid.Parse |> BookmarkId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> BookmarkId)
        with
            | _ -> None

    override x.ToString () =
        let (BookmarkId v) = x in string v

[<DomainType>]
type Bookmark = {
    [<PrimaryKey>]
    id : BookmarkId
    title : string
    timestamp : DateTime
    node : Node
    view : ViewParams
}

[<DomainType>]
type Bookmarks = {
    list : Bookmark plist
}

type BookmarkAction =
    | Add
    | Remove        of BookmarkId 
    | Update        of BookmarkId
    | SetTitle      of BookmarkId * string
    | Goto          of BookmarkId
    | MouseEnter    of BookmarkId
    | MouseLeave
    | CreateStory

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bookmark =

    let create (title : string) (node : Node) (view : ViewParams) : Bookmark = {
            id = BookmarkId.generate ()
            title = title
            timestamp = DateTime.Now
            node = node
            view = view
        }