namespace Aardvark.PRo3d

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.PRo3d

[<AutoOpen>]
module Mutable =

    [<StructuredFormatDisplay("{AsString}")>]
    type MUrdar(__initial : Aardvark.PRo3d.Urdar) = 
        let mutable __current = __initial
        let _urdar = ResetMod.Create(__initial.urdar)
        
        member x.urdar = _urdar :> IMod<_>
        
        member x.Update(v : Aardvark.PRo3d.Urdar) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_urdar,v.urdar)
        
        static member Create(v : Aardvark.PRo3d.Urdar) = MUrdar(v)
        static member Update(m : MUrdar, v : Aardvark.PRo3d.Urdar) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Urdar =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let urdar =
                { new Lens<Aardvark.PRo3d.Urdar, Microsoft.FSharp.Core.int>() with
                    override x.Get(r) = r.urdar
                    override x.Set(r,v) = { r with urdar = v }
                    override x.Update(r,f) = { r with urdar = f r.urdar }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MModel(__initial : Aardvark.PRo3d.Model) = 
        let mutable __current = __initial
        let _boxHovered = ResetMod.Create(__initial.boxHovered)
        let _dragging = ResetMod.Create(__initial.dragging)
        let _lastName = MOption.Create(__initial.lastName)
        let _elements = MList.Create(__initial.elements)
        let _hasD3Hate = ResetMod.Create(__initial.hasD3Hate)
        let _boxScale = ResetMod.Create(__initial.boxScale)
        let _objects = MMap.Create(__initial.objects, (fun v -> MUrdar.Create(v)), (fun (m,v) -> MUrdar.Update(m, v)), (fun v -> v))
        
        member x.boxHovered = _boxHovered :> IMod<_>
        member x.dragging = _dragging :> IMod<_>
        member x.lastName = _lastName :> IMod<_>
        member x.elements = _elements :> alist<_>
        member x.hasD3Hate = _hasD3Hate :> IMod<_>
        member x.boxScale = _boxScale :> IMod<_>
        member x.objects = _objects :> amap<_,_>
        
        member x.Update(v : Aardvark.PRo3d.Model) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_boxHovered,v.boxHovered)
                ResetMod.Update(_dragging,v.dragging)
                MOption.Update(_lastName, v.lastName)
                MList.Update(_elements, v.elements)
                ResetMod.Update(_hasD3Hate,v.hasD3Hate)
                ResetMod.Update(_boxScale,v.boxScale)
                MMap.Update(_objects, v.objects)
        
        static member Create(v : Aardvark.PRo3d.Model) = MModel(v)
        static member Update(m : MModel, v : Aardvark.PRo3d.Model) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let boxHovered =
                { new Lens<Aardvark.PRo3d.Model, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.boxHovered
                    override x.Set(r,v) = { r with boxHovered = v }
                    override x.Update(r,f) = { r with boxHovered = f r.boxHovered }
                }
            let dragging =
                { new Lens<Aardvark.PRo3d.Model, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.dragging
                    override x.Set(r,v) = { r with dragging = v }
                    override x.Update(r,f) = { r with dragging = f r.dragging }
                }
            let lastName =
                { new Lens<Aardvark.PRo3d.Model, Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.lastName
                    override x.Set(r,v) = { r with lastName = v }
                    override x.Update(r,f) = { r with lastName = f r.lastName }
                }
            let elements =
                { new Lens<Aardvark.PRo3d.Model, Aardvark.Base.plist<Microsoft.FSharp.Core.string>>() with
                    override x.Get(r) = r.elements
                    override x.Set(r,v) = { r with elements = v }
                    override x.Update(r,f) = { r with elements = f r.elements }
                }
            let hasD3Hate =
                { new Lens<Aardvark.PRo3d.Model, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.hasD3Hate
                    override x.Set(r,v) = { r with hasD3Hate = v }
                    override x.Update(r,f) = { r with hasD3Hate = f r.hasD3Hate }
                }
            let boxScale =
                { new Lens<Aardvark.PRo3d.Model, Microsoft.FSharp.Core.float>() with
                    override x.Get(r) = r.boxScale
                    override x.Set(r,v) = { r with boxScale = v }
                    override x.Update(r,f) = { r with boxScale = f r.boxScale }
                }
            let objects =
                { new Lens<Aardvark.PRo3d.Model, Aardvark.Base.hmap<Microsoft.FSharp.Core.string,Aardvark.PRo3d.Urdar>>() with
                    override x.Get(r) = r.objects
                    override x.Set(r,v) = { r with objects = v }
                    override x.Update(r,f) = { r with objects = f r.objects }
                }
    [<StructuredFormatDisplay("{AsString}")>]
    type MCameraControllerState(__initial : Aardvark.PRo3d.CameraControllerState) = 
        let mutable __current = __initial
        let _view = ResetMod.Create(__initial.view)
        let _dragStart = ResetMod.Create(__initial.dragStart)
        let _look = ResetMod.Create(__initial.look)
        let _zoom = ResetMod.Create(__initial.zoom)
        let _pan = ResetMod.Create(__initial.pan)
        let _forward = ResetMod.Create(__initial.forward)
        let _backward = ResetMod.Create(__initial.backward)
        let _left = ResetMod.Create(__initial.left)
        let _right = ResetMod.Create(__initial.right)
        let _moveVec = ResetMod.Create(__initial.moveVec)
        let _lastTime = MOption.Create(__initial.lastTime)
        let _stash = ResetMod.Create(__initial.stash)
        
        member x.view = _view :> IMod<_>
        member x.dragStart = _dragStart :> IMod<_>
        member x.look = _look :> IMod<_>
        member x.zoom = _zoom :> IMod<_>
        member x.pan = _pan :> IMod<_>
        member x.forward = _forward :> IMod<_>
        member x.backward = _backward :> IMod<_>
        member x.left = _left :> IMod<_>
        member x.right = _right :> IMod<_>
        member x.moveVec = _moveVec :> IMod<_>
        member x.lastTime = _lastTime :> IMod<_>
        member x.stash = _stash :> IMod<_>
        
        member x.Update(v : Aardvark.PRo3d.CameraControllerState) =
            if not (System.Object.ReferenceEquals(__current, v)) then
                __current <- v
                
                ResetMod.Update(_view,v.view)
                ResetMod.Update(_dragStart,v.dragStart)
                ResetMod.Update(_look,v.look)
                ResetMod.Update(_zoom,v.zoom)
                ResetMod.Update(_pan,v.pan)
                ResetMod.Update(_forward,v.forward)
                ResetMod.Update(_backward,v.backward)
                ResetMod.Update(_left,v.left)
                ResetMod.Update(_right,v.right)
                ResetMod.Update(_moveVec,v.moveVec)
                MOption.Update(_lastTime, v.lastTime)
                _stash.Update(v.stash)
        
        static member Create(v : Aardvark.PRo3d.CameraControllerState) = MCameraControllerState(v)
        static member Update(m : MCameraControllerState, v : Aardvark.PRo3d.CameraControllerState) = m.Update(v)
        
        override x.ToString() = __current.ToString()
        member x.AsString = sprintf "%A" __current
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CameraControllerState =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let view =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Aardvark.Base.CameraView>() with
                    override x.Get(r) = r.view
                    override x.Set(r,v) = { r with view = v }
                    override x.Update(r,f) = { r with view = f r.view }
                }
            let dragStart =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Aardvark.Base.V2i>() with
                    override x.Get(r) = r.dragStart
                    override x.Set(r,v) = { r with dragStart = v }
                    override x.Update(r,f) = { r with dragStart = f r.dragStart }
                }
            let look =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.look
                    override x.Set(r,v) = { r with look = v }
                    override x.Update(r,f) = { r with look = f r.look }
                }
            let zoom =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.zoom
                    override x.Set(r,v) = { r with zoom = v }
                    override x.Update(r,f) = { r with zoom = f r.zoom }
                }
            let pan =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.pan
                    override x.Set(r,v) = { r with pan = v }
                    override x.Update(r,f) = { r with pan = f r.pan }
                }
            let forward =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.forward
                    override x.Set(r,v) = { r with forward = v }
                    override x.Update(r,f) = { r with forward = f r.forward }
                }
            let backward =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.backward
                    override x.Set(r,v) = { r with backward = v }
                    override x.Update(r,f) = { r with backward = f r.backward }
                }
            let left =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.left
                    override x.Set(r,v) = { r with left = v }
                    override x.Update(r,f) = { r with left = f r.left }
                }
            let right =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.bool>() with
                    override x.Get(r) = r.right
                    override x.Set(r,v) = { r with right = v }
                    override x.Update(r,f) = { r with right = f r.right }
                }
            let moveVec =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Aardvark.Base.V3i>() with
                    override x.Get(r) = r.moveVec
                    override x.Set(r,v) = { r with moveVec = v }
                    override x.Update(r,f) = { r with moveVec = f r.moveVec }
                }
            let lastTime =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.float>>() with
                    override x.Get(r) = r.lastTime
                    override x.Set(r,v) = { r with lastTime = v }
                    override x.Update(r,f) = { r with lastTime = f r.lastTime }
                }
            let stash =
                { new Lens<Aardvark.PRo3d.CameraControllerState, Microsoft.FSharp.Core.Option<Aardvark.PRo3d.CameraControllerState>>() with
                    override x.Get(r) = r.stash
                    override x.Set(r,v) = { r with stash = v }
                    override x.Update(r,f) = { r with stash = f r.stash }
                }
