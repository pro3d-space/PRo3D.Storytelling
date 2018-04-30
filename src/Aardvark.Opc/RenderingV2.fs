namespace Aardvark.Opc

open System
open System.IO
open System.Threading

open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.Base.Incremental
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Rendering.GL
open Aardvark.Rendering.SceneGraph.HierarchicalLoD

open OpenTK.Graphics.OpenGL4

[<AutoOpen>]
module Helpers =
    let (<*>) (a : IMod<Trafo3d>) (b : IMod<Trafo3d>) = Mod.map2 (*) a b
    let inverse (m : IMod<Trafo3d>) = m |> Mod.map (fun t -> t.Inverse)

type LodParameters = { frustum : Frustum; size : V2i; factor : float; trafo: Trafo3d }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LodParameters =    
    let defaultParams = { frustum = Frustum.perspective 60.0 0.1 1000.0 (3.0 / 2.0); size = V2i(800,600); factor = 1.0; trafo = Trafo3d.Identity }

[<AutoOpen>]
module LodParametersSg =

    type LodParametersApplicator(sg : IMod<ISg>, cam : IMod<LodParameters>) =
        inherit Sg.AbstractApplicator(sg)
        member x.Cam = cam

    [<Semantic>]
    type LodParametersSem() =
        member x.LodParameters(c : LodParametersApplicator) =
            c.Child?LodParameters <- c.Cam
        member x.LodParameters(c : Root<ISg>) = 
            c.Child?LodParameters <- Mod.constant LodParameters.defaultParams

    type ISg with
        member x.LodParameters : IMod<LodParameters> = x?LodParameters

    module Sg =
        let LodParameters (p : IMod<LodParameters>) (sg : ISg) =
            LodParametersApplicator(Mod.constant sg, p) :> ISg

type AttributeParameters = { 
    selectedTexture : option<int>
    selectedScalar  : option<int>
    //minScalar       : float
    //maxScalar       : float
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AttributeParameters =   
    let defaultParams = { selectedTexture = None; selectedScalar = None } //; minScalar = 0.0; maxScalar = 1.0 }

[<AutoOpen>]
module AttributeParametersSg =

    type AttributeParametersApplicator(sg : IMod<ISg>, state : IMod<AttributeParameters>) =
        inherit Sg.AbstractApplicator(sg)
        member x.State = state
        
    [<Semantic>]
    type AttributeParametersSem() =
        member x.AttributeParameters(c : AttributeParametersApplicator) =
            c.Child?AttributeParameters <- c.State
        member x.AttributeParameters(c : Root<ISg>) = 
            c.Child?AttributeParameters <- Mod.constant AttributeParameters.defaultParams

    type ISg with
        member x.AttributeParameters : IMod<AttributeParameters> = x?AttributeParameters

    module Sg =
        let AttributeParameters (p : IMod<AttributeParameters>) (sg : ISg) =
            AttributeParametersApplicator(Mod.constant sg, p) :> ISg


module PatchLod =
  open Aardvark.Base.Rendering
  open Aardvark.Base.Ag
  open Aardvark.SceneGraph.Semantics
  open Aardvark.SceneGraph.Semantics.TrafoSemantics
  open Aardvark.Base.MultimethodTest

  let mutable useAsyncLoading = true

  let rec toRoseTree (p : QTree<'a>) =
      match p with
          | QTree.Leaf v -> Leaf v
          | QTree.Node(v,children) -> Node(v,children |> Array.toList |> List.map toRoseTree)

  type RenderPatch =
      {
          info            : PatchFileInfo
          level           : int
          triangleSize    : float
          trafo           : IMod<Trafo3d>
      }

  type LodDecider = AdaptiveToken -> IMod<Trafo3d> -> IMod<Trafo3d> -> RenderPatch -> IMod<LodParameters> -> bool 

  type PatchNode(signature : IFramebufferSignature, runner : Load.Runner, 
                 baseDir : string, decider : LodDecider, 
                 useCompressedTextures : bool, p : RoseTree<Patch>) =
      interface ISg
      member x.Tree = p        
      member x.Signature = signature
      member x.Runner = runner
      member x.BaseDir = baseDir
      member x.Decider = decider
      member x.UseCompressedTextures = useCompressedTextures
  

  [<Semantic>]
  type PatchNodeSem() =

    let toRenderPatch (modelTrafoStack : list<IMod<Trafo3d>>) (p : Patch) =
      {
          info         = p.info
          level        = p.level
          triangleSize = p.triangleSize
          trafo        = flattenStack (Mod.constant p.info.Local2Global :: modelTrafoStack)
      }

    member x.RenderObjects(n : PatchNode) : aset<IRenderObject> =

      let height = n.Tree |> RoseTree.height

      // Grab inherited attributes
      let runtime         = n.Runtime
      let scope           = Ag.getContext()
      let trafoStack      = n.ModelTrafoStack
      let v               = n.ViewTrafo
      let proj            = n.ProjTrafo
      let ro              = RenderObject.ofScope scope      
      let lodParameters   = n.LodParameters          
      let attributeParams = n.AttributeParameters

      // prepare data for lod metric
      let vp   = Mod.map2 (*) n.ViewTrafo n.ProjTrafo
      let godeeper (self : AdaptiveToken) (p : RenderPatch) =
        n.Decider self n.ViewTrafo n.ProjTrafo p lodParameters

      let preparePatch (p : RenderPatch) =

        let prepareTime = System.Diagnostics.Stopwatch()
        prepareTime.Start()

        let (g, elapsedIndex), createTime = timed (fun () -> 
          Patch.load n.BaseDir p.info)
        
        let textureId : IMod<int> = 
          attributeParams 
            |> Mod.map(fun x ->
                  match x.selectedTexture with
                    | Some i -> i
                    | None -> 0
            )

        let scalarId : IMod<int> =
          attributeParams 
           |> Mod.map(fun x ->
                 match x.selectedScalar with
                   | Some i -> i
                   | None -> -1
           )

        let oldTexture = ref None

        let tex =
          textureId |> Mod.map (fun id ->                            
            let texturePath = Patch.extractTexturePath n.BaseDir p.info id
            let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
            let tex,d =
              if n.UseCompressedTextures then
                  CompressedImage.load runtime texturePath config false
              else 
                  FileTexture(texturePath,config) :> ITexture, { new IDisposable with member x.Dispose() = () }
            match !oldTexture with
              | None -> 
                oldTexture := Some d
                tex
              | Some old -> 
                oldTexture := Some d
                old.Dispose()
                tex
          )

        let scl = 
          scalarId 
            |> Mod.bind (fun id ->                                 
              let x = p.info.Attributes |> List.rev |> List.tryItem id
              match x with
                | Some name ->
                      let arr = Path.combine [n.BaseDir; "Patches"; p.info.Name; name] |> fromFile<double>
                      let buffer = ArrayBuffer(arr.Data |> Array.map (fun a -> float32 a)) :> IBuffer
                      Mod.constant buffer 
                | None -> 
                      let a  = SingleValueBuffer(Mod.constant V4f.OOOO) 
                      a :> IMod<_>
            )
            
        let ro = RenderObject.Clone(ro)

        let prepareBuffer (a : ArrayBuffer) = runtime.PrepareBuffer(a) 

        let idx = ArrayBuffer(g.IndexArray) |> prepareBuffer
        let pos = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.Positions]) |> prepareBuffer
        let tc  = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]) |> prepareBuffer                   
              
        ro.Indices <- BufferView(Mod.constant (idx :> IBuffer), typeof<int>) |> Some
        
        let attributestList = [ 
          DefaultSemantic.Positions, BufferView(Mod.constant (pos :> IBuffer), typeof<V3f>)
          DefaultSemantic.DiffuseColorCoordinates, BufferView(Mod.constant (tc :> IBuffer), typeof<V2f>)    
          Sym.ofString("Scalar"), BufferView(scl, typeof<float32>)
        ]
        
        ro.VertexAttributes <- 
          AttributeProvider.ofList attributestList

        let old = ro.Uniforms
        ro.Uniforms <-
          let t = tex :> IMod
          { new IUniformProvider with
              member x.TryGetUniform(scope, sem) =
                if sem = DefaultSemantic.DiffuseColorTexture then Some t
                else
                  match string sem with
                    | "LoDColor" ->                       
                      let factor = float p.level / (float height - 1.0)
                      let c = hsv2rgb 0.0 factor 1.0                      
                      Mod.constant c :> IMod |> Some
                    | "ModelTrafo" -> p.trafo :> IMod |> Some                    
                    | _ -> 
                      lock old (fun () -> old.TryGetUniform(scope, sem))

              member x.Dispose() =
                  old.Dispose()
          }

        let call =
          DrawCallInfo(
            FaceVertexCount = g.IndexArray.Length,
            InstanceCount   = 1,
            FirstIndex      = 0,
            FirstInstance   = 0,
            BaseVertex      = 0
          )

        ro.DrawCallInfos <- Mod.constant [call]
        ro.Mode <- Mod.constant g.Mode

        ro.Activate <- fun () -> 
          { new IDisposable with
              member x.Dispose() = 
                  //onTextureDispose.Dispose()
                  !oldTexture |> Option.iter (fun d -> d.Dispose())
                  runtime.DeleteBuffer(idx)
                  runtime.DeleteBuffer(pos)
                  runtime.DeleteBuffer(tc)
          }

        let prepared = runtime.PrepareRenderObject(n.Signature, ro)
            
        GL.Sync()

        prepareTime.Stop()
        Statistics.emitLoaded prepareTime.MicroTime createTime elapsedIndex
        prepared
              
      let prepare (patch : RenderPatch) =
        load {
            let res = preparePatch patch
            do! Load.disposeOnCancel res 
            return res
        } |> n.Runner.Enqueue

      let rec flattenHorizon (t : RoseTree<RenderPatch>) =
        match t with
          | Empty -> []
          | Leaf v -> [v]
          | Node(c,[]) -> [c]
          | Node(c,xs) ->
              xs |> List.collect flattenHorizon

      let syncRenderObjects =
        let lodedTree =
          n.Tree
            |> RoseTree.map (toRenderPatch trafoStack)
            |> Mod.constant
            |> RoseTree.filter godeeper
        let objects = 
          ASet.custom (fun token state ->
            let currentTree    = lodedTree.GetValue token
            let currentObjects = flattenHorizon currentTree |> HRefSet.ofList
            let delta          = HRefSet.computeDelta state currentObjects 
            printfn "delta=%A" delta.Count
            delta
          )

        objects 
          |> ASet.mapUse (fun a -> use __ = runtime.ContextLock in preparePatch a)
          |> ASet.map (fun a -> a :> IRenderObject)

      let asyncRenderObjects =
        n.Tree
          |> RoseTree.map (toRenderPatch trafoStack)
          |> Mod.constant
          |> RoseTree.filter godeeper
          |> RoseTree.mapAsync prepare
          |> RoseTree.flatten (fun p -> p :> IRenderObject)
      
      match useAsyncLoading with
        | false -> syncRenderObjects
        | true -> asyncRenderObjects


module Sg =
    open PatchLod

    let patchLod (signature : IFramebufferSignature) (runner : Load.Runner) (baseDir : string) (decider : LodDecider) (useCompressedTextures : bool) (p : RoseTree<Patch>) =
        PatchNode(signature, runner, baseDir, decider, useCompressedTextures, p) :> ISg