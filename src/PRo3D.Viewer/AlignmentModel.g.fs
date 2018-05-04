namespace PRo3d.Align

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3d.Align

[<AutoOpen>]
module Mutable =

    
    
    type MAlignment(__initial : PRo3d.Align.Alignment) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.Align.Alignment> = Aardvark.Base.Incremental.EqModRef<PRo3d.Align.Alignment>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.Align.Alignment>
        let _red = ResetMod.Create(__initial.red)
        let _blue = ResetMod.Create(__initial.blue)
        let _redPoints = MList.Create(__initial.redPoints)
        let _bluePoints = MList.Create(__initial.bluePoints)
        
        member x.red = _red :> IMod<_>
        member x.blue = _blue :> IMod<_>
        member x.redPoints = _redPoints :> alist<_>
        member x.bluePoints = _bluePoints :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Align.Alignment) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_red,v.red)
                ResetMod.Update(_blue,v.blue)
                MList.Update(_redPoints, v.redPoints)
                MList.Update(_bluePoints, v.bluePoints)
                
        
        static member Create(__initial : PRo3d.Align.Alignment) : MAlignment = MAlignment(__initial)
        static member Update(m : MAlignment, v : PRo3d.Align.Alignment) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.Align.Alignment> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Alignment =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let red =
                { new Lens<PRo3d.Align.Alignment, System.Guid>() with
                    override x.Get(r) = r.red
                    override x.Set(r,v) = { r with red = v }
                    override x.Update(r,f) = { r with red = f r.red }
                }
            let blue =
                { new Lens<PRo3d.Align.Alignment, System.Guid>() with
                    override x.Get(r) = r.blue
                    override x.Set(r,v) = { r with blue = v }
                    override x.Update(r,f) = { r with blue = f r.blue }
                }
            let redPoints =
                { new Lens<PRo3d.Align.Alignment, Aardvark.Base.plist<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.redPoints
                    override x.Set(r,v) = { r with redPoints = v }
                    override x.Update(r,f) = { r with redPoints = f r.redPoints }
                }
            let bluePoints =
                { new Lens<PRo3d.Align.Alignment, Aardvark.Base.plist<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.bluePoints
                    override x.Set(r,v) = { r with bluePoints = v }
                    override x.Update(r,f) = { r with bluePoints = f r.bluePoints }
                }
    
    
    type MAlignmentModel(__initial : PRo3d.Align.AlignmentModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3d.Align.AlignmentModel> = Aardvark.Base.Incremental.EqModRef<PRo3d.Align.AlignmentModel>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3d.Align.AlignmentModel>
        let _pickedPoints = MList.Create(__initial.pickedPoints)
        let _alignment = MOption.Create(__initial.alignment, (fun v -> MAlignment.Create(v)), (fun (m,v) -> MAlignment.Update(m, v)), (fun v -> v))
        let _resultTrafo = MOption.Create(__initial.resultTrafo)
        
        member x.pickedPoints = _pickedPoints :> alist<_>
        member x.alignment = _alignment :> IMod<_>
        member x.resultTrafo = _resultTrafo :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3d.Align.AlignmentModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_pickedPoints, v.pickedPoints)
                MOption.Update(_alignment, v.alignment)
                MOption.Update(_resultTrafo, v.resultTrafo)
                
        
        static member Create(__initial : PRo3d.Align.AlignmentModel) : MAlignmentModel = MAlignmentModel(__initial)
        static member Update(m : MAlignmentModel, v : PRo3d.Align.AlignmentModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3d.Align.AlignmentModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AlignmentModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let pickedPoints =
                { new Lens<PRo3d.Align.AlignmentModel, Aardvark.Base.plist<PRo3d.Align.PickSurfacePair>>() with
                    override x.Get(r) = r.pickedPoints
                    override x.Set(r,v) = { r with pickedPoints = v }
                    override x.Update(r,f) = { r with pickedPoints = f r.pickedPoints }
                }
            let alignment =
                { new Lens<PRo3d.Align.AlignmentModel, Microsoft.FSharp.Core.Option<PRo3d.Align.Alignment>>() with
                    override x.Get(r) = r.alignment
                    override x.Set(r,v) = { r with alignment = v }
                    override x.Update(r,f) = { r with alignment = f r.alignment }
                }
            let resultTrafo =
                { new Lens<PRo3d.Align.AlignmentModel, Microsoft.FSharp.Core.Option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.resultTrafo
                    override x.Set(r,v) = { r with resultTrafo = v }
                    override x.Update(r,f) = { r with resultTrafo = f r.resultTrafo }
                }
