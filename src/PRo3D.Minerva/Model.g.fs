namespace PRo3D.Minerva

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Minerva

[<AutoOpen>]
module Mutable =

    
    
    type MFeatureCollection(__initial : PRo3D.Minerva.FeatureCollection) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.FeatureCollection> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.FeatureCollection>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.FeatureCollection>
        let _name = ResetMod.Create(__initial.name)
        let _typus = ResetMod.Create(__initial.typus)
        let _boundingBox = ResetMod.Create(__initial.boundingBox)
        let _features = MList.Create(__initial.features)
        
        member x.name = _name :> IMod<_>
        member x.typus = _typus :> IMod<_>
        member x.boundingBox = _boundingBox :> IMod<_>
        member x.features = _features :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.FeatureCollection) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_typus,v.typus)
                ResetMod.Update(_boundingBox,v.boundingBox)
                MList.Update(_features, v.features)
                
        
        static member Create(__initial : PRo3D.Minerva.FeatureCollection) : MFeatureCollection = MFeatureCollection(__initial)
        static member Update(m : MFeatureCollection, v : PRo3D.Minerva.FeatureCollection) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.FeatureCollection> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FeatureCollection =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let name =
                { new Lens<PRo3D.Minerva.FeatureCollection, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let typus =
                { new Lens<PRo3D.Minerva.FeatureCollection, PRo3D.Minerva.Typus>() with
                    override x.Get(r) = r.typus
                    override x.Set(r,v) = { r with typus = v }
                    override x.Update(r,f) = { r with typus = f r.typus }
                }
            let boundingBox =
                { new Lens<PRo3D.Minerva.FeatureCollection, Aardvark.Base.Box2d>() with
                    override x.Get(r) = r.boundingBox
                    override x.Set(r,v) = { r with boundingBox = v }
                    override x.Update(r,f) = { r with boundingBox = f r.boundingBox }
                }
            let features =
                { new Lens<PRo3D.Minerva.FeatureCollection, Aardvark.Base.plist<PRo3D.Minerva.Feature>>() with
                    override x.Get(r) = r.features
                    override x.Set(r,v) = { r with features = v }
                    override x.Update(r,f) = { r with features = f r.features }
                }
    
    
    type MModel(__initial : PRo3D.Minerva.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.Model> = Aardvark.Base.Incremental.EqModRef<PRo3D.Minerva.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Minerva.Model>
        let _data = MFeatureCollection.Create(__initial.data)
        let _queries = ResetMod.Create(__initial.queries)
        let _vplMessages = ResetMod.Create(__initial.vplMessages)
        
        member x.data = _data
        member x.queries = _queries :> IMod<_>
        member x.comm = __current.Value.comm
        member x.vplMessages = _vplMessages :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Minerva.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MFeatureCollection.Update(_data, v.data)
                ResetMod.Update(_queries,v.queries)
                ResetMod.Update(_vplMessages,v.vplMessages)
                
        
        static member Create(__initial : PRo3D.Minerva.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : PRo3D.Minerva.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Minerva.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let data =
                { new Lens<PRo3D.Minerva.Model, PRo3D.Minerva.FeatureCollection>() with
                    override x.Get(r) = r.data
                    override x.Set(r,v) = { r with data = v }
                    override x.Update(r,f) = { r with data = f r.data }
                }
            let queries =
                { new Lens<PRo3D.Minerva.Model, Microsoft.FSharp.Collections.List<System.String>>() with
                    override x.Get(r) = r.queries
                    override x.Set(r,v) = { r with queries = v }
                    override x.Update(r,f) = { r with queries = f r.queries }
                }
            let comm =
                { new Lens<PRo3D.Minerva.Model, Microsoft.FSharp.Core.Option<PRo3D.Minerva.Communication.Communicator.Communicator>>() with
                    override x.Get(r) = r.comm
                    override x.Set(r,v) = { r with comm = v }
                    override x.Update(r,f) = { r with comm = f r.comm }
                }
            let vplMessages =
                { new Lens<PRo3D.Minerva.Model, Aardvark.Base.Incremental.ThreadPool<PRo3D.Minerva.MinervaAction>>() with
                    override x.Get(r) = r.vplMessages
                    override x.Set(r,v) = { r with vplMessages = v }
                    override x.Update(r,f) = { r with vplMessages = f r.vplMessages }
                }
