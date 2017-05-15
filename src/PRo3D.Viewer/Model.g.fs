namespace PRo3D

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    type MModel private(__initial : PRo3D.Model) =
        let mutable __current = __initial
        let _cameraModel = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraModel)
        let _importOpcs = ResetMod(__initial.importOpcs)
        let _surfaces = ResetSet(__initial.surfaces)
        let _bbs = ResetSet(__initial.bbs)
        
        member x.cameraModel = _cameraModel
        member x.importOpcs = _importOpcs :> IMod<_>
        member x.surfaces = _surfaces :> aset<_>
        member x.bbs = _bbs :> aset<_>
        
        member x.Update(__model : PRo3D.Model) =
            if not (Object.ReferenceEquals(__model, __current)) then
                __current <- __model
                _cameraModel.Update(__model.cameraModel)
                _importOpcs.Update(__model.importOpcs)
                _surfaces.Update(__model.surfaces)
                _bbs.Update(__model.bbs)
        
        static member Create(initial) = MModel(initial)
        
        override x.ToString() = __current.ToString()
        member private x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MModel =
        let inline cameraModel (m : MModel) = m.cameraModel
        let inline importOpcs (m : MModel) = m.importOpcs
        let inline surfaces (m : MModel) = m.surfaces
        let inline bbs (m : MModel) = m.bbs
    
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let cameraModel =
                { new Lens<PRo3D.Model, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraModel
                    override x.Set(r,v) = { r with cameraModel = v }
                    override x.Update(r,f) = { r with cameraModel = f r.cameraModel }
                }
            let importOpcs =
                { new Lens<PRo3D.Model, Microsoft.FSharp.Collections.list<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.importOpcs
                    override x.Set(r,v) = { r with importOpcs = v }
                    override x.Update(r,f) = { r with importOpcs = f r.importOpcs }
                }
            let surfaces =
                { new Lens<PRo3D.Model, Aardvark.Base.hset<Aardvark.SceneGraph.ISg>>() with
                    override x.Get(r) = r.surfaces
                    override x.Set(r,v) = { r with surfaces = v }
                    override x.Update(r,f) = { r with surfaces = f r.surfaces }
                }
            let bbs =
                { new Lens<PRo3D.Model, Aardvark.Base.hset<Aardvark.Base.Box3d>>() with
                    override x.Get(r) = r.bbs
                    override x.Set(r,v) = { r with bbs = v }
                    override x.Update(r,f) = { r with bbs = f r.bbs }
                }
