#nowarn "9"
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
open Aardvark.Base.Incremental.Load
open Aardvark.Rendering.GL
open OpenTK.Graphics.OpenGL4
open FSharp.NativeInterop

open Examples.TextureLoaders

module NativeSupport =
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool ReadFile(Microsoft.Win32.SafeHandles.SafeFileHandle handle, IntPtr buffer, uint32 numBytesToRead, int* numBytesRead, IntPtr overlapped);


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
        let buffer = Array.zeroCreate<byte> (1 <<< 22)

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

    let inline isNan (v : V3f) = V3f.AnyEqual(nanf,v)


    let createIndex (vi : Matrix<V3f>) =
        let dx = vi.Info.DX
        let dy = vi.Info.DY
        let dxy = dx + dy
        let mutable arr = Array.zeroCreate (int (vi.SX - 1L) * int (vi.SY - 1L) * 6)
        let mutable cnt = 0
        
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
                arr.[cnt + 0] <- (int i00)
                arr.[cnt + 1] <- (int i10)
                arr.[cnt + 2] <- (int i11)
                arr.[cnt + 3] <- (int i00)
                arr.[cnt + 4] <- (int i11)
                arr.[cnt + 5] <- (int i01)
                cnt <- cnt + 6
        )
        Array.Resize(&arr, cnt)
        arr

type Patch =
    {
        level : int
        info : PatchFileInfo
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Patch =
    let ofInfo (level : int) (p : PatchFileInfo) = { level = level; info = p }
        
    let load (p : PatchFileInfo)  =
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



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchHierarchy =
    open XmlHelpers
    open System.Xml
    open System.Xml.Linq
    open MBrace.FsPickler


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

    let binarySerializer = FsPickler.CreateBinarySerializer()
    let load (fileName : string) =
        let dir = Path.GetDirectoryName(Path.GetDirectoryName fileName)
        let cachefile = Path.combine [dir; "hierarchy.cache" ]
        if File.Exists cachefile then
            Log.startTimed "loading from cache."
            let r = File.readAllBytes cachefile |> binarySerializer.UnPickle
            Log.stop()
            r
        else
            Log.startTimed "loading from hierarchy"
            let hierarchy = 
                XDocument.Load(fileName) 
                    |> ofDoc
                    |> QTree.mapLevel 0 (fun level p -> p |> PatchFileInfo.load dir |> Patch.ofInfo level)
            Log.startTimed "writing cache"
            binarySerializer.Pickle hierarchy |> File.writeAllBytes cachefile
            Log.stop()
            Log.stop()
            hierarchy


module NaivePatchLoD =
    open Aardvark.Base
    open Aardvark.SceneGraph
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph.Semantics

    let colors =
        [ 0, C4b.Green
          1, C4b.Blue
          2, C4b.Yellow
          3, C4b.Red
          4, C4b.VRVisGreen
          5, C4b.Yellow ] |> Map.ofList

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

        let ex = Path.GetExtension textureImage
        if ex = ".dds" then printfn "asdfasdasd"

        Patch.load patch.info
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
        failwith ""
//        let rec mk d s =
//            match s with
//                | Node(p, children) ->
//                    let decider (scope : NaiveLod.LodScope) = 
////                        let bb = p.info.
////                        let mvp : Trafo3d = scope.ModelViewTrafo.GetValue()
//
//                        let bbT = p.info.GlobalBoundingBox.Transformed(preTrafo) //.Transformed(scope.trafo)
//                        let cam = scope.cameraPosition
////                        let screenSpaceBox = 
////                            bb.ComputeCorners() 
////                            |> Array.map (fun p -> mvp.Forward.TransformPosProj(p))
////                            |> Box3d
//                        let d = (bbT.Center - cam).Length 
//
//                        let area = computeScreenSpaceArea bbT (scope.ViewProjTrafo.GetValue())
//
//                        //printfn "d=%A" d
//                        area > 3.5
//                        //screenSpaceBox.SizeX < 0.3 && screenSpaceBox.SizeY < 0.3
//                    let self = patch2Sg
//                    NaiveLod.Sg.loD (patch2Sg d p) (children |> Array.map (mk (d+1)) |> Sg.ofSeq) decider
//                | Leaf v -> patch2Sg d v 
        //mk 0 s
    

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

        let mapAsync<'a,'b when 'b :> IDisposable and 'a : equality> (create : 'a -> ILoadTask<'b>) (tree : IMod<RoseTree<'a>>) : IMod<RoseTree<Option<'b>>> =
            let current = ref Empty

            let l = Mod.custom (fun self ->

                let equal (l : ILoadTask<'b>) (r : 'a) =
                    System.Object.Equals(l.Tag,r)
                
                let create (v : 'a) = 
                    runningLoadies <- runningLoadies + 1
                    let r = create v
                    r.Tag <- v
                    r.Start()
                    r

                let destroy (v : ILoadTask<'b>) =
                    runningLoadies <- runningLoadies - 1
                    v.Stop()

                let t = tree.GetValue self

                let n = traverse equal create destroy !current t
                current := n
                n
            )

            let current = ref Empty
            Mod.custom (fun self ->
                    
                let lt = l.GetValue(self)

                let allReady (c : list<RoseTree<ILoadTask<'b>>>) =
                    c |> List.forall (fun t ->
                        match t with
                            | Empty -> true
                            | Leaf v -> Option.isSome (v.GetValue(self))
                            | Node(v,_) -> Option.isSome (v.GetValue(self))
                    )

                let rec peek (t : RoseTree<ILoadTask<'b>>) =
                    match t with
                        | Empty -> Empty
                        | Leaf l ->
                            match l.GetValue(self) with
                                | Some v -> Leaf (Some v)
                                | None -> Empty
                        | Node(v, children) ->
                            match v.GetValue(self) with
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

    module Statistics = 

        type Stats = { loadedObjects : int; elapsed : TimeSpan }

        let empty = { loadedObjects = 0; elapsed = TimeSpan.Zero }
        let lockObj = obj()
        let mutable stats = empty
        let mutable initialized = false

        let start () =
            async {
                do! Async.SwitchToNewThread()
                while true do
                    System.Threading.Thread.Sleep 1000
                    let old = lock lockObj (fun () -> let r = stats in stats <- empty; r)
                    printfn "Status: %A, resulting in %Ams per object" old (float old.elapsed.TotalMilliseconds / float old.loadedObjects)
            } |> Async.Start

        let emitLoaded (timeSpan : TimeSpan) =  
            if not initialized then initialized <- true; start() 
            lock lockObj (fun () -> 
                stats <- { stats with loadedObjects = stats.loadedObjects + 1; elapsed = stats.elapsed + timeSpan }
            )

        

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
                info        : PatchFileInfo
                level       : int
                trafo       : IMod<Trafo3d>
            }
            
        type Prepared = { prepared : IPreparedRenderObject; d : IDisposable } with
            member x.Dispose() = x.prepared.Dispose(); x.d.Dispose()
            interface IDisposable with
                member x.Dispose() = x.Dispose()

//        type PrepareObj(f : unit -> Prepared) =
//            let cts = System.Threading.Tasks.TaskCompletionSource()
//
//            member x.Run() =
//                let r = f ()
//                cts.SetResult(r)
//                ()
//
//            member x.Task = Async.AwaitTask cts.Task


        let mutable preparedCnt = 0
        let mutable runner : Load.Runner = Unchecked.defaultof<_> 

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

                        let sw = System.Diagnostics.Stopwatch()
                        sw.Restart()
                        //use ctx = runtime.ContextLock

                        let g = Patch.load p.info
                        let t = p.info.Textures |> List.head
                        let sourcePath = Path.combine [p.info.BaseDir; t.fileName]
                        let extensions = [ ".texCache"; ".tif"; ".tiff"]

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


                        let loadAndCache () = 
                            let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = true }
                            let sourceTex = FileTexture(fullPath, config) :> ITexture
                            let prepared = runtime.PrepareTexture(sourceTex)
                            let handle = prepared.Handle |> unbox<int>
                            GL.BindTexture(TextureTarget.Texture2D, handle)
                            //let mutable internalF = 0
                            //GL.GetTexParameterI(TextureTarget.Texture2D, GetTextureParameter.TextureInternalFormat, &internalF)
                            //GL.Check "getTexP"
                            let ifmt = unbox<PixelInternalFormat> prepared.Format
                            let mipLevels =
                                let mutable autoGen = 0
                                GL.GetTexParameterI(TextureTarget.Texture2D, GetTextureParameter.GenerateMipmap, &autoGen)
                                if autoGen = 1 then
                                    let r = 1.0 + floor (log ( max (float prepared.Size.X) (float prepared.Size.Y)))
                                    int r
                                else
                                    let mutable mipLevels = 0
                                    GL.GetTexParameterI(TextureTarget.Texture2D, OpenTK.Graphics.OpenGL4.All.TextureImmutableLevels |> unbox, &mipLevels)
                                    max 1 mipLevels

                            let mipSizes = 
                                [ 0 .. mipLevels - 1 ] 
                                    |> List.map (fun l -> 
                                        let mutable size = 0
                                        GL.GetTexLevelParameter(TextureTarget.Texture2D,l,GetTextureParameter.TextureCompressedImageSize,&size)
                                        size
                                    )
                            let totalSize = List.sum mipSizes
                            let originalName = Path.GetFileName(fullPath)
                            let cacheFileName = Path.Combine(Path.GetDirectoryName(fullPath), sprintf "%s.cache" originalName)
                            printfn "caching to: %s" cacheFileName
                            let cacheFileInfo = Path.Combine(Path.GetDirectoryName(fullPath), sprintf "%s.cacheInfo" originalName)
                                    
                            use infoFile = new System.IO.BinaryWriter(File.OpenWrite(cacheFileInfo))
                            infoFile.Write(10)
                            infoFile.Write(totalSize)
                            infoFile.Write(prepared.Size.X)
                            infoFile.Write(prepared.Size.Y)
                            infoFile.Write(ifmt |> int)
                            infoFile.Write(mipLevels)

                            let basePtr = Marshal.AllocHGlobal(totalSize)
                            let mutable ptr = basePtr
                            let mutable level = 0
                            let mutable offset = 0
                            for size in mipSizes do
                                infoFile.Write(offset)
                                infoFile.Write(size)
                                GL.GetCompressedTexImage(TextureTarget.Texture2D, level, ptr)
                                ptr <- ptr + nativeint size
                                offset <- offset + size
                                level <- level + 1
                            GL.Check "download compressed image failed"
                            GL.BindTexture(TextureTarget.Texture2D, 0)

                            use cacheFileStream = File.OpenWrite(cacheFileName)
                            use ustream = new UnmanagedMemoryStream(basePtr |> NativePtr.ofNativeInt, int64 totalSize)
                            ustream.CopyTo(cacheFileStream)
                            cacheFileStream.Close()
                            infoFile.Close()
                            Marshal.FreeHGlobal(basePtr)
                            GL.Sync()
                            prepared :> ITexture, { new IDisposable with member x.Dispose() = () }

                        let loadFromCache (infoFile : System.IO.BinaryReader) =
                            let mutable totalSize = infoFile.ReadInt32()
                            let width = infoFile.ReadInt32()
                            let height = infoFile.ReadInt32()
                            let format = infoFile.ReadInt32() |> unbox<PixelInternalFormat>
                            let mips = infoFile.ReadInt32() 
                            let offsets = [ for i in 0 .. mips - 1 do yield infoFile.ReadInt32(), infoFile.ReadInt32() ]
                            infoFile.Close()

//                                    use mapped = System.IO.MemoryMappedFiles.MemoryMappedFile.CreateFromFile(cacheFileName)
//                                    let acc = mapped.CreateViewAccessor()
//                                    let mutable sourcePtr = NativePtr.ofNativeInt 0n
//                                    acc.SafeMemoryMappedViewHandle.AcquirePointer(&sourcePtr)
                            use cacheStream = File.OpenRead(cacheFileName)
                            let sourcePtr = Marshal.AllocHGlobal totalSize
                            let mutable bytesRead = NativePtr.alloc 4 
                            let ok = NativeSupport.ReadFile(cacheStream.SafeFileHandle,sourcePtr,uint32 totalSize,bytesRead,IntPtr.Zero)
                            let read = NativePtr.read bytesRead 
                            if read <> totalSize then failwith ""

                            let handle = GL.GenTexture()
                            let mutable level = 0
                            for offset,size in offsets do
                                let pbo = GL.GenBuffer()
                                GL.BindBuffer(BufferTarget.PixelUnpackBuffer, pbo)
                                GL.BufferStorage(BufferTarget.PixelUnpackBuffer, nativeint size, 0n, BufferStorageFlags.MapWriteBit)
                                let ptr = GL.MapBuffer(BufferTarget.PixelUnpackBuffer, BufferAccess.WriteOnly)

                                let actualSource = nativeint offset + sourcePtr

                                Marshal.Copy(actualSource, ptr, size)
                                GL.UnmapBuffer(BufferTarget.PixelUnpackBuffer) |> ignore

                                GL.Check "copied to buffer"

                                GL.BindTexture(TextureTarget.Texture2D, handle)
                                GL.Check "compressedTexImage"
                                GL.CompressedTexImage2D(TextureTarget.Texture2D, level, format, width, height, 0, size, 0n)
                                GL.Check "compressedTexImage"
                                GL.BindTexture(TextureTarget.Texture2D, 0)
                                GL.BindBuffer(BufferTarget.PixelUnpackBuffer, 0)
                                GL.DeleteBuffer(pbo)
                                level <- level + 1

                            Marshal.FreeHGlobal sourcePtr

                            let dispose = 
                                { new IDisposable with
                                    member x.Dispose() =
                                        use ctx = runtime.ContextLock
                                        GL.DeleteTexture(handle)
                                }

                            let glRuntime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>
                                    
                            GL.BindTexture(TextureTarget.Texture2D, handle)
                            if config.wantMipMaps && mips <= 1 then
                                GL.GenerateMipmap(GenerateMipmapTarget.Texture2D)
                                let newSize = 4 * totalSize / 3
                                //updateTexture t.Context t.SizeInBytes newSize
                                totalSize <- newSize //t.SizeInBytes <- newSize
                            else
                                GL.TexParameterI(TextureTarget.Texture2D, TextureParameterName.TextureMaxLevel, [| 0 |])
                                GL.TexParameterI(TextureTarget.Texture2D, TextureParameterName.TextureMaxLod, [| 0 |])
                            GL.BindTexture(TextureTarget.Texture2D, 0)

                            GL.Sync()

                            let t = Texture(
                                        glRuntime.Context,
                                        handle,
                                        TextureDimension.Texture2D,
                                        mips,
                                        1,
                                        V3i(width,height,1),
                                        1,
                                        format |> unbox<TextureFormat>,
                                        int64 totalSize,
                                        false)
                            t :> ITexture, dispose

                        let tex,killTexture = 
                            FileTexture(fullPath,config) :> ITexture, { new IDisposable with member x.Dispose() = () }
//                            match File.Exists cacheFileName with
//                                | true  -> 
//                                    use infoFile = new System.IO.BinaryReader(File.OpenRead(cacheInfo))
//                                    let version = infoFile.ReadInt32()
//                                    if version <> 10 then
//                                        infoFile.Close()
//                                        loadAndCache()
//                                    else
//                                        loadFromCache infoFile
//                                | _ -> 
//                                    loadAndCache ()
      
                        let ro = RenderObject.Clone(ro)


                        let idx     = ArrayBuffer(g.IndexArray) :> IBuffer
                        let pos     = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.Positions]) :> IBuffer
                        let tc      = ArrayBuffer(g.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]) :> IBuffer
                          
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

                        ro.Activate <- fun () -> killTexture

                        let prepared =  runtime.PrepareRenderObject(n.Signature, ro)
                        
                        GL.Sync()

                        sw.Stop()
                        Statistics.emitLoaded sw.Elapsed

                        Interlocked.Increment(&preparedCnt) |> ignore
                        { prepared = prepared; d = { new IDisposable with member x.Dispose() = Interlocked.Decrement(&preparedCnt) |> ignore } }

                let prepare (patch : RenderPatch) =
                    load {
                        let res = preparePatch patch
                        do! Load.disposeOnCancel res 
                        return res
                    } |> runner.Enqueue


                n.Tree
                    |> RoseTree.map (toRenderPatch trafoStack)
                    |> Mod.constant
                    |> RoseTree.filter godeeper
                    |> RoseTree.mapAsync prepare
                    |> RoseTree.flatten (fun p -> p.prepared :> IRenderObject)
