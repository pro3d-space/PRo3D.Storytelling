#if INTERACTIVE
#r "../../bin/Debug/Aardvark.Base.dll"
#r "../../bin/Debug/Aardvark.Base.TypeProviders.dll"
#r "../../bin/Debug/Aardvark.Base.FSharp.dll"
#r "System.Xml.Linq.dll"
#else
namespace Aardvark.Opc
#endif

open System
open System.IO

open System.Runtime.InteropServices
open Aardvark.Base

[<AutoOpen>]
module private Aara =
    let readerChars2String (f : Stream)  = 
        let cnt = f.ReadByte()
        let target = Array.zeroCreate cnt
        let read = f.Read(target,0,cnt)
        if cnt <> read then failwith ""
        System.Text.Encoding.Default.GetString target

    let loadFromStream<'a when 'a : unmanaged> (f : Stream) =
        let binaryReader = new BinaryReader(f,Text.Encoding.ASCII, true)
        let typeName = readerChars2String f
        let dimensions = f.ReadByte() |> int
        let sizes = [| for d in 0 .. dimensions - 1 do yield binaryReader.ReadInt32() |]

        let elementCount = sizes |> Array.fold ((*)) 1
        if typeof<'a>.Name <> typeName then failwith "wrong type"
    
        let result = Array.zeroCreate<'a> elementCount
        let buffer = Array.zeroCreate<byte> (1 <<< 16)

        let gc = GCHandle.Alloc(result, GCHandleType.Pinned)
        try
            let mutable ptr = gc.AddrOfPinnedObject()
            let mutable remaining = sizeof<'a> * result.Length
            while remaining > 0 do
        
                let s = f.Read(buffer, 0, buffer.Length)
                Marshal.Copy(buffer, 0, ptr, s)
                ptr <- ptr + nativeint s
                remaining <- remaining - s
        finally
            gc.Free()


        let dim =
            match sizes with
                | [| x |] -> V3i(x,1,1)
                | [| x; y |] -> V3i(x,y,1)
                | [| x; y; z |] -> V3i(x,y,z)
                | _ -> failwith ""

        Volume<'a>(result, dim)

    let fromFile<'a when 'a : unmanaged> (s : string) =
        use fs = File.OpenRead(s)
        loadFromStream<'a> fs

    let isNan (v : V3f) = V3f.AnyEqual(nanf,v)


    let createIndex (vi : Matrix<V3f>) =
        let dx = vi.Info.DX
        let dy = vi.Info.DY
        let dxy = dx + dy
        let resultIndices = System.Collections.Generic.List<int>()

        vi.SubMatrix(V2l.Zero,vi.Size-V2l.II).ForeachXYIndex(fun x y index -> 

            let i00 = index
            let i10 = index + dy
            let i01 = index + dx
            let i11 = index + dxy

            let p00 = vi.[i00] |> isNan
            let p01 = vi.[i10] |> isNan
            let p10 = vi.[i01] |> isNan
            let p11 = vi.[i11] |> isNan

            if p00 || p01 || p10 || p11 then ()
            else 
                resultIndices.Add(int i00)
                resultIndices.Add(int i10)
                resultIndices.Add(int i11)

                resultIndices.Add(int i00)
                resultIndices.Add(int i11)
                resultIndices.Add(int i01)
        )
        resultIndices.ToArray()

type Thunk<'a>(f : unit -> 'a) =    
    let mutable value = None

    member x.Value =
        lock x (fun () -> 
            match value with
                | Some v -> v
                | None -> 
                    let r = f () 
                    value <- Some r
                    r
        )

    member x.Reset() =
        lock x (fun () -> 
            value <- None
        )

type Patch =
    {
        geometry : Thunk<IndexedGeometry>
        level : int
        info : PatchFileInfo
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Patch =
    let load (level : int) (p : PatchFileInfo) =
        
        let geometry = 
            Thunk<_>(fun () -> 
                let positions = Path.combine [p.BaseDir; p.Positions] |> fromFile<V3f>
                let coordinates = Path.combine [p.BaseDir; (List.head p.Coordinates)] |> fromFile<V2f>

                let coordinates = coordinates.Data |> Array.map (fun v -> V2f(v.X, 1.0f-v.Y))

                //let normals = Path.combine [p.BaseDir; "Normals.aara"] |> fromFile<V3f>
                let index = createIndex (positions.AsMatrix())

                IndexedGeometry(
                    Mode = IndexedGeometryMode.TriangleList,
                    IndexArray = index,
                    IndexedAttributes =
                        SymDict.ofList [
                            DefaultSemantic.Positions, positions.Data :> Array
                            DefaultSemantic.DiffuseColorCoordinates, coordinates :> Array
                            //DefaultSemantic.Normals, normals.Data :> Array
                        ]
                )
            )

        { geometry = geometry; level = level; info = p }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchHierarchy =
    open XmlHelpers
    open System.Xml
    open System.Xml.Linq

    let private ofDoc (x : XDocument) =
        let hierarchy = x.Descendants(xname "PatchHierarchy") |> Seq.head
        let rootPatch = hierarchy |> elem "RootPatch" |> xvalue

        let map = hierarchy |> elem' "SubPatchMap"
        let items = 
            match map with
                | Some subs -> subs.Descendants(xname "item") |> Seq.toList
                | None -> []

        let children = 
            items |> List.map (fun e -> 
                let key = elem "key" e |> xvalue
                let values = elem "val" e
                let nw = elem' "nw" values |> Option.map xvalue 
                let ne = elem' "ne" values |> Option.map xvalue 
                let sw = elem' "sw" values |> Option.map xvalue 
                let se = elem' "se" values |> Option.map xvalue 
                key, Array.choose id [|nw;ne;sw;se |]
            ) |> Dictionary.ofList


        let rec mkTree (name : string) =
            match children.TryGetValue name with
                | (true,v) -> 
                    Node(name, v |> Array.map mkTree)
                | _ -> Leaf name

        mkTree rootPatch

    let load (fileName : string) =
        let dir = Path.GetDirectoryName(Path.GetDirectoryName fileName)
        XDocument.Load(fileName) 
            |> ofDoc
            |> QTree.mapLevel 0 (fun level p -> p |> PatchFileInfo.load dir |> Patch.load level)


module NaivePatchLoD =
    open Aardvark.Base
    open Aardvark.SceneGraph
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph.Semantics

    let colors =
        [ 0, C4b.Green
          1, C4b.Blue
          2, C4b.Yellow
          3, C4b.Red ] |> Map.ofList

    let patch2Sg (depth : int) (patch : Patch) =
        let tex = patch.info.Textures |> List.head 
        let tiff = Path.combine [patch.info.BaseDir; tex.fileName]
        //let fullPath = System.IO.Path.ChangeExtension(fullPath,".dds")

        let extensions = [ ".dds"; ".tif"; ".tiff"]

        let rec tryFindTex exts path =
            match exts with
                | x::xs -> 
                    let cuzrrent = System.IO.Path.ChangeExtension(path,x)
                    if File.Exists cuzrrent then cuzrrent else tryFindTex xs path
                | [] -> failwithf "texture not found: %s" path


        let textureImage = tryFindTex extensions tiff

        patch.geometry.Value
            |> Sg.ofIndexedGeometry
            |> Sg.uniform "LoDColor" (Mod.constant (Map.find depth colors))
            |> Sg.diffuseFileTexture' textureImage true
            |> Sg.trafo (Mod.constant patch.info.Local2Global)


    let computeScreenSpaceArea (b : Box3d) (viewProj : Trafo3d) =
        let viewSpacePoints =
            b.ComputeCorners()
                |> Array.map (fun v ->
                    viewProj.Forward.TransformPosProj(v).XY
                )

        let poly = viewSpacePoints |> Polygon2d

        let clipped = 
            poly.ComputeConvexHullIndexPolygon().ToPolygon2d()//.ConvexClipped(Box2d(-V2d.II, V2d.II))

        clipped.ComputeArea()


    let toSg (preTrafo : Trafo3d) (s : QTree<Patch>) : ISg =
        let rec mk d s =
            match s with
                | Node(p, children) ->
                    let decider (scope : NaiveLod.LodScope) = 
//                        let bb = p.info.
//                        let mvp : Trafo3d = scope.ModelViewTrafo.GetValue()

                        let bbT = p.info.GlobalBoundingBox.Transformed(preTrafo) //.Transformed(scope.trafo)
                        let cam = scope.cameraPosition
//                        let screenSpaceBox = 
//                            bb.ComputeCorners() 
//                            |> Array.map (fun p -> mvp.Forward.TransformPosProj(p))
//                            |> Box3d
                        let d = (bbT.Center - cam).Length 

                        let area = computeScreenSpaceArea bbT (scope.ViewProjTrafo.GetValue())

                        //printfn "d=%A" d
                        area > 3.5
                        //screenSpaceBox.SizeX < 0.3 && screenSpaceBox.SizeY < 0.3
                    let self = patch2Sg
                    NaiveLod.Sg.loD (patch2Sg d p) (children |> Array.map (mk (d+1)) |> Sg.ofSeq) decider
                | Leaf v -> patch2Sg d v 
        mk 0 s
    

module PatchRendering =
    open System.Threading
    open System.Threading.Tasks
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics

    type ILodData =
        abstract member BoundingBox : Box3d
        abstract member Traverse : (LodDataNode -> (LodDataNode -> list<'a>) -> 'a) -> 'a
        abstract member GetData : node : LodDataNode -> Async<Option<IndexedGeometry>>
                  

    type LodNode( data : ILodData, fboSig : IFramebufferSignature) =
        interface ISg
        member x.Data = data
        member x.FboSig = fboSig

    let disposeOnCancel<'a when 'a :> IDisposable> (f : unit -> 'a) : Async<'a> =
        async {
            let mutable call = None
            let! _ = Async.OnCancel(fun () -> call |> Option.iter Disposable.dispose)
            let r = f()
            call <- Some (r :> IDisposable)
            return r
        }

    type AsyncMod<'a>(inner : IMod<'a>) =
        inherit Mod.AbstractMod<'a>()

        let sem = new SemaphoreSlim(1)

        override x.Mark() =
            sem.Release() |> ignore
            true

        override x.Compute() =
            inner.GetValue(x)

        member x.GetValueAsync() =
            async {
                let! _ = Async.AwaitIAsyncResult (sem.WaitAsync())
                return x.GetValue()
            }

    type RoseTree<'a> =
        | Empty
        | Leaf of 'a
        | Node of 'a * list<RoseTree<'a>>

    let mutable cancelledCnt = 0

    type Task<'a> with
        member x.PeekResult =
            if x.Status = TaskStatus.RanToCompletion then Some (Some x.Result)
            elif x.Status = TaskStatus.Canceled || x.Status = TaskStatus.Faulted then Some None
            else None

    type Loady<'a, 'b>(tag : 'b, trigger : IAdaptiveObject, release : 'a -> unit, run : Async<'a>) =
        let mutable task : Option<Task<'a>> = None
        let mutable cancel = new CancellationTokenSource()

        let run =  
            async {
                let! r = run
                transact (fun () -> trigger.MarkOutdated())
                return r
            }


        member x.Tag = tag

        member x.Peek =
            match task with
                | Some t -> t.PeekResult |> Option.bind id
                | _ -> None

        member x.Start() =
            match task with
                | None ->
                    let t = Async.StartAsTask(run, cancellationToken = cancel.Token)
                    task <- Some t
                | Some _ -> 
                    ()

        member x.Stop() =
            match task with
                | Some t ->
                    match t.PeekResult with
                        | Some None ->
                            () // cancelled
                        | Some (Some r) ->
                            release t.Result
                        | None ->
                            let afterCompleted =
                                async {
                                    let! r = Async.AwaitTask t
                                    release r
                                }
                            Async.Start afterCompleted
                            cancel.Cancel()

                    task <- None
                | _ ->
                    ()
                 
    module Loady =
        let start (tag : 'b) (trigger : IAdaptiveObject) (run : Async<'a>) =
            let l = Loady(tag, trigger, (fun a -> (a :> IDisposable).Dispose()), run)
            l.Start()
            l

//        let start' (tag : 'b) (trigger : IAdaptiveObject) (run : Async<'a>) =
//            let l = Loady(tag, trigger, ignore, run)
//            l.Start()
//            l


    module RoseTree =
        open System.Collections.Generic

        let rec traverse<'a, 'b> (equal : 'b -> 'a -> bool) (create : 'a -> 'b) (destroy : 'b -> unit) (ref : RoseTree<'b>) (t : RoseTree<'a>) : RoseTree<'b> =
            let traverse = traverse equal create destroy

            match ref, t with
                | Empty, Empty -> 
                    Empty

                | Empty, Leaf v ->
                    v |> create |> Leaf

                | Empty, Node(v, children) ->
                    let n = v |> create
                    Node(n, children |> List.map (traverse Empty))

                | Leaf v, Empty ->
                    destroy v
                    Empty

                | Leaf l, Leaf r ->
                    if equal l r then 
                        Leaf l
                    else 
                        destroy l
                        r |> create |> Leaf

                | Leaf l, Node(r, children) ->
                    if equal l r then
                        Node(l, children |> List.map (traverse Empty))
                    else
                        destroy l
                        let n = r |> create
                        Node(n, children |> List.map (traverse Empty))

                | Node(v,c), Empty ->
                    destroy v
                    c |> List.iter (fun c -> traverse c Empty |> ignore)
                    Empty

                | Node(v,c), Leaf r ->
                    c |> List.iter (fun c -> traverse c Empty |> ignore)
                    if equal v r then
                        Leaf v
                    else
                        destroy v
                        r |> create |> Leaf
                                            
                | Node(lv,lc), Node(rv,rc) ->
                    if equal lv rv then
                        Node(lv, List.map2 traverse lc rc)
                    else
                        destroy lv
                        let nv = rv |> create
                        Node(nv, List.map2 traverse lc rc)
    
        let filter (godeeper : IAdaptiveObject -> 'a -> bool) (tree : IMod<RoseTree<'a>>) : IMod<RoseTree<'a>> =
            Mod.custom (fun self ->
                let rec traverse (t : RoseTree<'a>) =
                    match t with
                        | Empty -> Empty
                        | Leaf v -> Leaf v
                        | Node(v,children) ->
                            if godeeper self v then
                                Node(v,List.map traverse children)
                            else
                                Leaf v

                let t = tree.GetValue self
                traverse t
            )
            
        let  mutable runningLoadies = 0

        let mapAsync<'a,'b when 'b :> IDisposable and 'a : equality> (create : 'a -> Async<'b>) (tree : IMod<RoseTree<'a>>) : IMod<RoseTree<Option<'b>>> =
            let current = ref Empty

            let l = Mod.custom (fun self ->

                let equal (l : Loady<'b,'a>) (r : 'a) =
                    l.Tag = r
                
                let create (v : 'a) = 
                    runningLoadies <- runningLoadies + 1
                    Loady.start v self (create v)

                let destroy (v : Loady<'b, 'a>) =
                    runningLoadies <- runningLoadies - 1
                    //printfn "running loadies: %A" runningLoadies
                    v.Stop()

                let t = tree.GetValue self

                let n = traverse equal create destroy !current t
                current := n
                n
            )

            let current = ref Empty
            Mod.custom (fun self ->
                    
                let lt = l.GetValue(self)

                let allReady (c : list<RoseTree<Loady<'b, 'a>>>) =
                    c |> List.forall (fun t ->
                        match t with
                            | Empty -> true
                            | Leaf v -> Option.isSome v.Peek
                            | Node(v,_) -> Option.isSome v.Peek
                    )

                let rec peek (t : RoseTree<Loady<'b, 'a>>) =
                    match t with
                        | Empty -> Empty
                        | Leaf l ->
                            match l.Peek with
                                | Some v -> Leaf (Some v)
                                | None -> Empty
                        | Node(v, children) ->
                            match v.Peek with
                                | Some v ->
                                    if allReady children then
                                        Node(None, List.map peek children)
                                    else
                                        Leaf(Some v)
                                | None ->
                                    Empty

                peek lt

            )

        let flatten mapF (f : IMod<RoseTree<Option<'a>>>) =
            let old = ref Empty
            ASet.custom (fun self -> 
                let current = f.GetValue self
                let deltas = List<_>()
                let create v = 
                    match v with
                        | Some v -> deltas.Add(Add (mapF v))
                        | None -> ()
                    v
                let destroy v =
                    match v with
                        | Some v -> deltas.Add (Rem (mapF v))
                        | None -> ()

                let u = traverse (=) create destroy !old current
                old := u
                deltas |> CSharpList.toList
            )

        let rec map (f : 'a -> 'b) (t : RoseTree<'a>) =
            match t with
                | Empty -> Empty
                | Leaf v -> Leaf (f v)
                | Node(v,c) -> Node(f v, List.map (map f) c)   


    module PatchLod =
        open Aardvark.Base.Rendering
        open Aardvark.Base.Ag
        open Aardvark.SceneGraph
        open Aardvark.SceneGraph.Semantics

        let rec toRoseTree (p : QTree<'a>) =
            match p with
                | QTree.Leaf v -> Leaf v
                | QTree.Node(v,children) -> Node(v,children |> Array.toList |> List.map toRoseTree)

        type PatchNode(signature : IFramebufferSignature, p : RoseTree<Patch>) =
            interface ISg
            member x.Tree = p
            member x.Signature = signature
    

        type RenderPatch =
            {
                geometry    : Thunk<IndexedGeometry>
                info        : PatchFileInfo
                level       : int
                trafo       : IMod<Trafo3d>
            }
            
        type Prepared = { prepared : IPreparedRenderObject; d : IDisposable } with
            member x.Dispose() = x.prepared.Dispose(); x.d.Dispose()
            interface IDisposable with
                member x.Dispose() = x.Dispose()

        type PrepareObj(f : unit -> Prepared) =
            let cts = System.Threading.Tasks.TaskCompletionSource()

            member x.Run() =
                let r = f ()
                cts.SetResult(r)
                ()

            member x.Task = Async.AwaitTask cts.Task


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
                    geometry    = p.geometry
                    info        = p.info
                    level       = p.level
                    trafo       = flattenStack (Mod.constant p.info.Local2Global :: modelTrafoStack)
                }

            let colors =
                [ 0, C4b.Green
                  1, C4b.Blue
                  2, C4b.Yellow
                  3, C4b.Red
                  4, C4b.Red 
                  5, C4b.Red 
                  6, C4b.Red 
                  7, C4b.Red  ] |> Map.ofList

            member x.RenderObjects(n : PatchNode) : aset<IRenderObject> =
                let runtime = n.Runtime
                let scope = Ag.getContext()
                let trafoStack = n.ModelTrafoStack
                let v = n.ViewTrafo
                let p = n.ProjTrafo
                let vp = v <*> p
                let ro = RenderObject.ofScope scope

                let godeeper (self : IAdaptiveObject) (p : RenderPatch) =
                    let m = p.trafo.GetValue self
                    let vp = vp.GetValue self
                    let mvp = m * vp
                    let area = computeScreenSpaceArea p.info.LocalBoundingBox mvp
                    
                    area > 3.5

                let queue = ConcurrentHashQueue<_>()

                let preparePatch (p : RenderPatch) =
//                        let! _ = Async.OnCancel(fun () -> 
//                            match !self with
//                                | None -> ()
//                                | Some v -> 
//                                    v.Dispose()
//                                    p.geometry.Reset()
//                        )

                        let g = p.geometry.Value
                        let t = p.info.Textures |> List.head
                        let sourcePath = Path.combine [p.info.BaseDir; t.fileName]
                        let extensions = [ ".dds"; ".tif"; ".tiff"]

                        let rec tryFindTex exts path =
                            match exts with
                                | x::xs -> 
                                    let cuzrrent = System.IO.Path.ChangeExtension(path,x)
                                    if File.Exists cuzrrent then cuzrrent else tryFindTex xs path
                                | [] -> failwithf "texture not found: %s" path


                        let fullPath = tryFindTex extensions sourcePath
                            
                        let ro = RenderObject.Clone(ro)

//                        let pos = g.IndexedAttributes.[DefaultSemantic.Positions] |> unbox<V3f[]>
//                        let bounds = pos |> Array.filter (fun v -> v.AnyEqual(nanf) |> not) |> Array.map V3d |> Box3d
//                        printfn "%A" bounds

                        let prepareBuffer b = b :> IBuffer //runtime.PrepareBuffer b
                        let prepareTexture t = t :> ITexture //runtime.PrepareTexture t

                        let idx     = prepareBuffer(ArrayBuffer(g.IndexArray))
                        let pos     = prepareBuffer(ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.Positions]))
                        let tc      = prepareBuffer(ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]))
                        let tex     = prepareTexture(FileTexture(fullPath, true))
                          
                        ro.Indices <- BufferView(Mod.constant idx, typeof<int>) |> Some

                        ro.VertexAttributes <- 
                            AttributeProvider.ofList [ 
                                DefaultSemantic.Positions, BufferView(Mod.constant pos, typeof<V3f>)
                                DefaultSemantic.DiffuseColorCoordinates, BufferView(Mod.constant tc, typeof<V2f>)
                            ]

                        let old = ro.Uniforms
                        ro.Uniforms <-
                            let t = Mod.constant tex :> IMod
                            { new IUniformProvider with
                                member x.TryGetUniform(scope, sem) =
                                    if sem = DefaultSemantic.DiffuseColorTexture then Some t
                                    else
                                        match string sem with
                                            | "LoDColor" -> colors.[p.level] |> Mod.constant :> IMod |> Some
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
                                    p.geometry.Reset()
                            }

                        let prepared =  runtime.PrepareRenderObject(n.Signature, ro)
                        //self := Some prepared
                        { prepared = prepared; d = { new IDisposable with member x.Dispose() = Interlocked.Decrement(&preparedCnt) |> ignore } }

                let sem = new SemaphoreSlim(0)

                let prepare (p : RenderPatch)  =   
                    let self : ref<Option<_>> = ref None
                    let p = PrepareObj((fun () -> let pr = preparePatch p in preparedCnt <- preparedCnt + 1; self := Some pr; pr))
                    queue.Enqueue(p) |> ignore
                    sem.Release() |> ignore
                    async {
                        let! _ = Async.OnCancel(fun () -> 
                            printfn "cancelled something: %d" preparedCnt
                            queue.Remove(p) |> ignore
                            match !self with
                                | Some v -> 
                                    printfn "disposed due to cancelling"
                                    v.Dispose()
                                | _ -> ()
                        )
                        let! res = p.Task
                        //self := Some res
                        return res
                    }


                let runner =
                    async {
                        do! Async.SwitchToNewThread()
                        
                        while true do
                            //use context = runtime.ContextLock
                            sem.Wait()
                            match queue.TryDequeue() with
                                | (true,v) -> v.Run() |> ignore   
                                | _ -> ()
                    } |> Async.Start

                n.Tree
                    |> RoseTree.map (toRenderPatch trafoStack)
                    |> Mod.constant
                    |> RoseTree.filter godeeper
                    |> RoseTree.mapAsync prepare
                    |> RoseTree.flatten (fun p -> p.prepared :> IRenderObject)
