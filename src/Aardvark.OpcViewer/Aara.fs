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


type Patch =
    {
        geometry : Lazy<IndexedGeometry>
        info : PatchFileInfo
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Patch =
    let load (p : PatchFileInfo) =
        let geometry = 
            lazy (
                let positions = Path.combine [p.BaseDir; p.Positions] |> fromFile<V3f>
                let coordinates = Path.combine [p.BaseDir; (List.head p.Coordinates)] |> fromFile<V2f>
                let normals = Path.combine [p.BaseDir; "Normals.aara"] |> fromFile<V3f>
                let index = createIndex (positions.AsMatrix())

                IndexedGeometry(
                    Mode = IndexedGeometryMode.TriangleList,
                    IndexArray = index,
                    IndexedAttributes =
                        SymDict.ofList [
                            DefaultSemantic.Positions, positions.Data :> Array
                            DefaultSemantic.DiffuseColorCoordinates, coordinates.Data :> Array
                            DefaultSemantic.Normals, normals.Data :> Array
                        ]
                )
            )

        { geometry = geometry; info = p }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchHierarchy =
    open XmlHelpers
    open System.Xml
    open System.Xml.Linq

    let private ofDoc (x : XDocument) =
        let hierarchy = x.Descendants(xname "PatchHierarchy") |> Seq.head
        let rootPatch = hierarchy |> elem "RootPatch" |> xvalue

        let map = hierarchy |> elem "SubPatchMap"
        let items = map.Descendants(xname "item") |> Seq.toList
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
            |> QTree.map (PatchFileInfo.load dir >> Patch.load)

