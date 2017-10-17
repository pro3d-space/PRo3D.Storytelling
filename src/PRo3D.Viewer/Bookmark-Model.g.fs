namespace PRo3D.BookmarksX

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.BookmarksX

[<AutoOpen>]
module Mutable =

    
    
    type MBookmark(__initial : PRo3D.BookmarksX.Bookmark) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.ModRef<PRo3D.BookmarksX.Bookmark> = Aardvark.Base.Incremental.Mod.init(__initial)
        let _key = ResetMod.Create(__initial.key)
        let _name = ResetMod.Create(__initial.name)
        let _navigationState = PRo3D.Navigation2.Mutable.MNavigationModel.Create(__initial.navigationState)
        
        member x.key = _key :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.navigationState = _navigationState
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.BookmarksX.Bookmark) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_key,v.key)
                ResetMod.Update(_name,v.name)
                PRo3D.Navigation2.Mutable.MNavigationModel.Update(_navigationState, v.navigationState)
                
        
        static member Create(__initial : PRo3D.BookmarksX.Bookmark) : MBookmark = MBookmark(__initial)
        static member Update(m : MBookmark, v : PRo3D.BookmarksX.Bookmark) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.BookmarksX.Bookmark> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Bookmark =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let key =
                { new Lens<PRo3D.BookmarksX.Bookmark, System.Guid>() with
                    override x.Get(r) = r.key
                    override x.Set(r,v) = { r with key = v }
                    override x.Update(r,f) = { r with key = f r.key }
                }
            let name =
                { new Lens<PRo3D.BookmarksX.Bookmark, Microsoft.FSharp.Core.string>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let navigationState =
                { new Lens<PRo3D.BookmarksX.Bookmark, PRo3D.Navigation2.NavigationModel>() with
                    override x.Get(r) = r.navigationState
                    override x.Set(r,v) = { r with navigationState = v }
                    override x.Update(r,f) = { r with navigationState = f r.navigationState }
                }
    
    
    type MBookmarksModel(__initial : PRo3D.BookmarksX.BookmarksModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.ModRef<PRo3D.BookmarksX.BookmarksModel> = Aardvark.Base.Incremental.Mod.init(__initial)
        let _bookmarks = MList.Create(__initial.bookmarks, (fun v -> MBookmark.Create(v)), (fun (m,v) -> MBookmark.Update(m, v)), (fun v -> v))
        let _selectedBookmark = MOption.Create(__initial.selectedBookmark, (fun v -> MBookmark.Create(v)), (fun (m,v) -> MBookmark.Update(m, v)), (fun v -> v))
        
        member x.bookmarks = _bookmarks :> alist<_>
        member x.selectedBookmark = _selectedBookmark :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.BookmarksX.BookmarksModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_bookmarks, v.bookmarks)
                MOption.Update(_selectedBookmark, v.selectedBookmark)
                
        
        static member Create(__initial : PRo3D.BookmarksX.BookmarksModel) : MBookmarksModel = MBookmarksModel(__initial)
        static member Update(m : MBookmarksModel, v : PRo3D.BookmarksX.BookmarksModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.BookmarksX.BookmarksModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BookmarksModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let bookmarks =
                { new Lens<PRo3D.BookmarksX.BookmarksModel, Aardvark.Base.plist<PRo3D.BookmarksX.Bookmark>>() with
                    override x.Get(r) = r.bookmarks
                    override x.Set(r,v) = { r with bookmarks = v }
                    override x.Update(r,f) = { r with bookmarks = f r.bookmarks }
                }
            let selectedBookmark =
                { new Lens<PRo3D.BookmarksX.BookmarksModel, Microsoft.FSharp.Core.option<PRo3D.BookmarksX.Bookmark>>() with
                    override x.Get(r) = r.selectedBookmark
                    override x.Set(r,v) = { r with selectedBookmark = v }
                    override x.Update(r,f) = { r with selectedBookmark = f r.selectedBookmark }
                }
