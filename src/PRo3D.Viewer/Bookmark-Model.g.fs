namespace PRo3D.BookmarksX

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.BookmarksX

[<AutoOpen>]
module Mutable =

    
    
    type MBookmark(__initial : PRo3D.BookmarksX.Bookmark) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.BookmarksX.Bookmark> = Aardvark.Base.Incremental.EqModRef<PRo3D.BookmarksX.Bookmark>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.BookmarksX.Bookmark>
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
