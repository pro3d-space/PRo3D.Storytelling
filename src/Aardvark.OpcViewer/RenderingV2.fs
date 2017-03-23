namespace Aardvark.Opc

open System
open System.IO
open System.Threading

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Rendering.GL
open Aardvark.Rendering.SceneGraph.HierarchicalLoD

open OpenTK.Graphics.OpenGL4

[<AutoOpen>]
module Helpers =
    let (<*>) (a : IMod<Trafo3d>) (b : IMod<Trafo3d>) = Mod.map2 (*) a b
    let inverse (m : IMod<Trafo3d>) = m |> Mod.map (fun t -> t.Inverse)

module PatchLod =
    open Aardvark.Base.Rendering
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph.Semantics
    open Aardvark.SceneGraph.Semantics.TrafoSemantics

    let rec toRoseTree (p : QTree<'a>) =
        match p with
            | QTree.Leaf v -> Leaf v
            | QTree.Node(v,children) -> Node(v,children |> Array.toList |> List.map toRoseTree)

    type RenderPatch =
        {
            info        : PatchFileInfo
            level       : int
            trafo       : IMod<Trafo3d>
        }

    type LodDecider = AdaptiveToken -> IMod<Trafo3d> -> RenderPatch -> bool 

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
                info        = p.info
                level       = p.level
                trafo       = flattenStack (Mod.constant p.info.Local2Global :: modelTrafoStack)
            }

        member x.RenderObjects(n : PatchNode) : aset<IRenderObject> =

            let height = n.Tree |> RoseTree.height

            // Grab inherited attributes
            let runtime        = n.Runtime
            let scope          = Ag.getContext()
            let trafoStack     = n.ModelTrafoStack
            let v              = n.ViewTrafo
            let proj           = n.ProjTrafo
            let ro             = RenderObject.ofScope scope

            // prepare data for lod metric
            let vp   = Mod.map2 (*) n.ViewTrafo n.ProjTrafo
            let godeeper (self : AdaptiveToken) (p : RenderPatch) =
                n.Decider self vp p 

            let preparePatch (p : RenderPatch) =

                    let prepareTime = System.Diagnostics.Stopwatch()
                    prepareTime.Start()

                    let (g, elapsedIndex), createTime = timed (fun () -> 
                            Patch.load n.BaseDir p.info)

                    let texturePath = Patch.extractTexture n.BaseDir p.info

                    let tex,onTextureDispose = 
                        let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
                        if n.UseCompressedTextures then
                            CompressedImage.load runtime texturePath config false
                        else 
                            FileTexture(texturePath,config) :> ITexture, { new IDisposable with member x.Dispose() = () }

                    let ro = RenderObject.Clone(ro)

                    let prepareBuffer (a : ArrayBuffer) = runtime.PrepareBuffer(a) 

                    let idx     = ArrayBuffer(g.IndexArray) 
                                    |> prepareBuffer
                    let pos     = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.Positions]) 
                                    |> prepareBuffer
                    let tc      = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]) 
                                    |> prepareBuffer
                          
                    ro.Indices <- BufferView(Mod.constant (idx :> IBuffer), typeof<int>) |> Some

                    ro.VertexAttributes <- 
                        AttributeProvider.ofList [ 
                            DefaultSemantic.Positions, BufferView(Mod.constant (pos :> IBuffer), typeof<V3f>)
                            DefaultSemantic.DiffuseColorCoordinates, BufferView(Mod.constant (tc :> IBuffer), typeof<V2f>)
                        ]

                    let old = ro.Uniforms
                    ro.Uniforms <-
                        let t = Mod.constant tex :> IMod
                        { new IUniformProvider with
                            member x.TryGetUniform(scope, sem) =
                                if sem = DefaultSemantic.DiffuseColorTexture then Some t
                                else
                                    match string sem with
                                        | "LoDColor" -> 
                                            let min = 120.0
                                            let size = -120.0
                                            let factor = float p.level / (float height - 1.0)
                                            let c = hsv2rgb 230.0 (1.0 - factor) 1.0
                                            //let c = hsv2rgb (min + factor * size) 0.6 1.0
                                            Mod.constant c :> IMod |> Some
                                        | "ModelTrafo" -> p.trafo :> IMod |> Some
                                        | "ModelViewTrafo" -> p.trafo <*> v :> IMod |> Some
                                        | "ModelViewProjTrafo" -> p.trafo <*> vp :> IMod |> Some
                                        | "ModelTrafoInv" -> p.trafo |> inverse :> IMod |> Some
                                        | "ModelViewTrafoInv" -> p.trafo <*> v |> inverse :> IMod |> Some
                                        | "ModelViewProjTrafoInv" -> p.trafo <*> vp |> inverse :> IMod |> Some
                                        | _ -> 
                                            lock old (fun () -> old.TryGetUniform(scope, sem))

                            member x.Dispose() =
                                old.Dispose()
                        }

                    let call =
                        DrawCallInfo(
                            FaceVertexCount = g.IndexArray.Length,
                            InstanceCount = 1,
                            FirstIndex = 0,
                            FirstInstance = 0,
                            BaseVertex = 0
                        )

                    ro.DrawCallInfos <- Mod.constant [call]
                    ro.Mode <- Mod.constant g.Mode

                    ro.Activate <- fun () -> 
                        { new IDisposable with
                            member x.Dispose() = 
                                onTextureDispose.Dispose()
                                runtime.DeleteBuffer(idx)
                                runtime.DeleteBuffer(pos)
                                runtime.DeleteBuffer(tc)
                        }

                    let prepared =  runtime.PrepareRenderObject(n.Signature, ro)
                        
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


            n.Tree
                |> RoseTree.map (toRenderPatch trafoStack)
                |> Mod.constant
                |> RoseTree.filter godeeper
                |> RoseTree.mapAsync prepare
                |> RoseTree.flatten (fun p -> p :> IRenderObject)


module Sg =
    open PatchLod

    let patchLod (signature : IFramebufferSignature) (runner : Load.Runner) (baseDir : string) (decider : LodDecider) (useCompressedTextures : bool) (p : RoseTree<Patch>) =
        PatchNode(signature, runner, baseDir, decider, useCompressedTextures, p) :> ISg