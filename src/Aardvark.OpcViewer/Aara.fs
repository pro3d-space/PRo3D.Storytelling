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

[<AutoOpen>]
module private Aara =

    let readerChars2String (f : Stream)  = 
        let cnt = f.ReadByte()
        let target = Array.zeroCreate cnt
        let read = f.Read(target,0,cnt)
        if cnt <> read then failwith ""
        System.Text.Encoding.Default.GetString target

    let loadRaw<'a  when 'a : (new : unit -> 'a) and 'a : struct and 'a :> ValueType> (elementCount : int) (f : Stream)  =
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
        result

    let loadRaw2<'a  when 'a : unmanaged> (elementCount : int) (f : Stream) : 'a[] =
        let target = Array.zeroCreate elementCount
        target.UnsafeCoercedApply<byte>(fun arr ->
            let r = f.Read(arr, 0, arr.Length)
            if r <> arr.Length then failwith "asdfj2"
        )
        target
//        let byteSize = elementCount * sizeof<'a>
//        let buffer = Array.zeroCreate<byte> byteSize
//        let r = f.Read(buffer, 0, byteSize)
//        if r <> byteSize then failwith "asdfj2"
//        buffer.UnsafeCoerce()



    let loadFromStream<'a when 'a : unmanaged> (f : Stream) =
        let binaryReader = new BinaryReader(f,Text.Encoding.ASCII, true)
        let typeName = readerChars2String f
        let dimensions = f.ReadByte() |> int
        let sizes = [| for d in 0 .. dimensions - 1 do yield binaryReader.ReadInt32() |]

        let elementCount = sizes |> Array.fold ((*)) 1

        
        let result =
            if typeof<'a>.Name = typeName then
                loadRaw2<'a> elementCount f
            else
                match typeName with
                    | "V3d" -> f |> loadRaw<V3d> elementCount |> PrimitiveValueConverter.arrayConverter typeof<V3d>
                    | "V2d" -> f |> loadRaw<V2d> elementCount |> PrimitiveValueConverter.arrayConverter typeof<V2d>
                    | _ -> failwith ""



        let dim =
            match sizes with
                | [| x |] -> V3i(x,1,1)
                | [| x; y |] -> V3i(x,y,1)
                | [| x; y; z |] -> V3i(x,y,z)
                | _ -> failwith ""

        Volume<'a>(result, dim)

    let fromFile<'a when 'a : unmanaged and 'a : (new : unit -> 'a) and 'a : struct and 'a :> ValueType> (s : string) =
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

        
    let load (baseDir : string) (p : PatchFileInfo)  =
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let positions = Path.combine [baseDir; "Patches"; p.Name; p.Positions] |> fromFile<V3f>
        let coordinates = Path.combine [baseDir; "Patches"; p.Name; (List.head p.Coordinates)] |> fromFile<V2f>
        sw.Stop()

        let coordinates = coordinates.Data |> Array.map (fun v -> V2f(v.X, 1.0f-v.Y))

        let index = createIndex (positions.AsMatrix())

        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray = index,
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, positions.Data :> Array
                    DefaultSemantic.DiffuseColorCoordinates, coordinates :> Array
                ]
        ), sw.MicroTime

    let extractTexture (baseDir : string) (patchInfo : PatchFileInfo) =
        let t = patchInfo.Textures |> List.head
        let sourcePath = Path.combine [ baseDir; "Images"; t.fileName]
        let extensions = [ ".dds"; ".tif"; ".tiff"]

        let rec tryFindTex exts path =
            match exts with
                | x::xs -> 
                    let current = System.IO.Path.ChangeExtension(path,x)
                    if File.Exists current then current else tryFindTex xs path
                | [] -> failwithf "texture not found: %s" path


        tryFindTex extensions sourcePath



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchHierarchy =
    open XmlHelpers
    open System.Xml
    open System.Xml.Linq
    open MBrace.FsPickler

    let private binarySerializer = FsPickler.CreateBinarySerializer()

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
        let cachefile = Path.combine [dir; "hierarchy.cache" ]
        if File.Exists cachefile then
            let r = File.readAllBytes cachefile |> binarySerializer.UnPickle
            dir, r
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
            dir, hierarchy


