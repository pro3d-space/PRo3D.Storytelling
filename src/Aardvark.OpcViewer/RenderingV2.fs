namespace Aardvark.Opc

open System
open System.IO
open System.Threading

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering.SceneGraph.HierarchicalLoD
open Aardvark.Rendering.GL
open Aardvark.SceneGraph

open OpenTK.Graphics.OpenGL4

module Statistics = 

    type Stats = { loadedObjects : int; elapsed : MicroTime; elapsedCreate : MicroTime; 
                   prepareTexture : MicroTime; prepareBuffer : MicroTime
                   elapsedIndex   : MicroTime }

    let empty = { loadedObjects = 0; elapsed = MicroTime.Zero; elapsedCreate = MicroTime.Zero; prepareTexture = MicroTime.Zero; prepareBuffer = MicroTime.Zero; elapsedIndex = MicroTime.Zero }
    let lockObj = obj()
    let mutable stats = empty
    let mutable initialized = false

    let start () =
        async {
            do! Async.SwitchToNewThread()
            while true do
                System.Threading.Thread.Sleep 1000
                let old = lock lockObj (fun () -> let r = stats in stats <- empty; r)
                printfn "Status: %A, resulting in %A per object (create %A, GPU buffers: %A, GPU textures: %A)" old  (old.elapsed / float old.loadedObjects) (old.elapsedCreate / float old.loadedObjects) (old.prepareBuffer / float old.loadedObjects) (old.prepareTexture / float old.loadedObjects)
        } |> Async.Start 

    let emitLoaded (timeSpan : MicroTime) (createTime : MicroTime) (buffers : MicroTime) (textures : MicroTime) (elapsedIndex : MicroTime) =  
        if not initialized then initialized <- true; start() 
        lock lockObj (fun () -> 
            stats <- { stats with 
                        loadedObjects = stats.loadedObjects + 1
                        elapsed       = stats.elapsed + timeSpan
                        elapsedCreate = stats.elapsedCreate + createTime
                        prepareBuffer = stats.prepareBuffer + buffers
                        prepareTexture = textures 
                        elapsedIndex   = stats.elapsedIndex + elapsedIndex
                     }
        )


module PatchLod =
    open Aardvark.Base.Rendering
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph.Semantics
    open Aardvark.SceneGraph.Semantics.TrafoSemantics

    let rec toRoseTree (p : QTree<'a>) =
        match p with
            | QTree.Leaf v -> Leaf v
            | QTree.Node(v,children) -> Node(v,children |> Array.toList |> List.map toRoseTree)

    type PatchNode(signature : IFramebufferSignature, runner : Load.Runner, baseDir : string, p : RoseTree<Patch>) =
        interface ISg
        member x.Tree = p
        member x.Signature = signature
        member x.Runner = runner
        member x.BaseDir = baseDir
    
    type RenderPatch =
        {
            info        : PatchFileInfo
            level       : int
            trafo       : IMod<Trafo3d>
        }
            
    type Prepared = { prepared : IPreparedRenderObject; d : IDisposable } with
        member x.Dispose() = x.prepared.Dispose(); x.d.Dispose()
        interface IDisposable with
            member x.Dispose() = x.Dispose()


    let mutable preparedCnt = 0

    open Aardvark.SceneGraph.Semantics.TrafoSemantics
    [<Semantic>]
    type PatchNodeSem() =
        let computeScreenSpaceArea (b : Box3d) (viewProj : Trafo3d) =
            let viewSpacePoints =
                b.ComputeCorners()
                    |> Array.map (fun v ->
                        viewProj.Forward.TransformPosProj(v)
                    )

            let poly = viewSpacePoints |> Array.map Vec.xy |> Polygon2d

            let clipped = 
                poly.ComputeConvexHullIndexPolygon().ToPolygon2d()//.ConvexClipped(Box2d(-V2d.II, V2d.II))
    
            let bounds = Box3d(viewSpacePoints)
            if Box3d(-V3d.III, V3d.III).Intersects(bounds) |> not then 0.0
            else clipped.ComputeArea()

        let toRenderPatch (modelTrafoStack : list<IMod<Trafo3d>>) (p : Patch) =
            {
                info        = p.info
                level       = p.level
                trafo       = flattenStack (Mod.constant p.info.Local2Global :: modelTrafoStack)
            }

        let hsv2rgb (h : float) (s : float) (v : float) =
            let h = h % 360.0
            let hi = floor ( h / 60.0 ) |> int
            let f = h % 60.0
            let p = v * (1.0-s)
            let q = v * (1.0 - s * f)
            let t = v * (1.0 - s * ( 1.0 - f ))
            match hi with
                | 1 -> C3f(q,v,p)
                | 2 -> C3f(p,v,t)
                | 3 -> C3f(p,q,v)
                | 4 -> C3f(t,p,v)
                | 5 -> C3f(v,p,q)
                | _ -> C3f(v,t,p)

        member x.RenderObjects(n : PatchNode) : aset<IRenderObject> =
            let runtime = n.Runtime
            let scope = Ag.getContext()
            let trafoStack = n.ModelTrafoStack
            let v = n.ViewTrafo
            let proj = n.ProjTrafo
            let vp = n.ViewTrafo <*> n.ProjTrafo
            let ro = RenderObject.ofScope scope

            let godeeper (self : IAdaptiveObject) (p : RenderPatch) =
                let m = p.trafo.GetValue self
                let vp = vp.GetValue self
                let mvp = m * vp
                let area = computeScreenSpaceArea p.info.LocalBoundingBox mvp
                    
                if p.level <= 1 then true
                else
                    log area > log 0.8
                //area > 3.5

            let queue = ConcurrentHashQueue<_>()

            let preparePatch (p : RenderPatch) =

                    let sw = System.Diagnostics.Stopwatch()
                    sw.Restart()
                    //use ctx = runtime.ContextLock
                    let createTime = System.Diagnostics.Stopwatch()
                    createTime.Start()
                    let g,elapsedIndex = Patch.load n.BaseDir p.info
                    createTime.Stop()
                    let t = p.info.Textures |> List.head
                    let sourcePath = Path.combine [n.BaseDir; "Images"; t.fileName]
                    let extensions = [ ".dds"; ".tif"; ".tiff"]

                    let rec tryFindTex exts path =
                        match exts with
                            | x::xs -> 
                                let cuzrrent = System.IO.Path.ChangeExtension(path,x)
                                if File.Exists cuzrrent then cuzrrent else tryFindTex xs path
                            | [] -> failwithf "texture not found: %s" path


                    let fullPath = tryFindTex extensions sourcePath
                    let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
                    let cacheFileName = sprintf "%s.cache" fullPath
                    let cacheInfo = sprintf "%s.cacheInfo" fullPath
                    //if File.Exists cacheFileName then File.Delete cacheFileName

                    let prepareTexture = System.Diagnostics.Stopwatch()
                    prepareTexture.Start()
                    let tex,killTexture = 
                        //FileTexture(fullPath,config) :> ITexture, { new IDisposable with member x.Dispose() = () }
                        CompressedImage.load runtime fullPath config false
                    //GL.Sync()
                    prepareTexture.Stop()

                    let ro = RenderObject.Clone(ro)

                    let prepareBuffer (a : ArrayBuffer) =
                        runtime.PrepareBuffer(a) 

                    let prepareBuffers = System.Diagnostics.Stopwatch()
                    prepareBuffers.Start()

                    let idx     = ArrayBuffer(g.IndexArray) 
                                    |> prepareBuffer
                    let pos     = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.Positions]) 
                                    |> prepareBuffer
                    let tc      = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]) 
                                    |> prepareBuffer
                    //GL.Sync()
                    prepareBuffers.Stop()
                          
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
                                            let c = hsv2rgb ((float p.level / 10.0) * 270.0) 1.0 1.0
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
                                killTexture.Dispose()
                                runtime.DeleteBuffer(idx)
                                runtime.DeleteBuffer(pos)
                                runtime.DeleteBuffer(tc)
                        }

                    let prepared =  runtime.PrepareRenderObject(n.Signature, ro)
                        
                    GL.Sync()

                    sw.Stop()
                    Statistics.emitLoaded sw.MicroTime createTime.MicroTime prepareBuffers.MicroTime prepareTexture.MicroTime elapsedIndex

                    Interlocked.Increment(&preparedCnt) |> ignore
                    { prepared = prepared; d = { new IDisposable with member x.Dispose() = Interlocked.Decrement(&preparedCnt) |> ignore } }

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
                |> RoseTree.flatten (fun p -> p.prepared :> IRenderObject)


module Sg =
    let patchLod (signature : IFramebufferSignature) (runner : Load.Runner) (baseDir : string) (p : RoseTree<Patch>) =
        PatchLod.PatchNode(signature, runner, baseDir, p) :> ISg