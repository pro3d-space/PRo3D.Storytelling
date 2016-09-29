#if INTERACTIVE
#r "../../bin/Debug/Aardvark.Base.dll"
#r "../../bin/Debug/Aardvark.Base.TypeProviders.dll"
#r "../../bin/Debug/Aardvark.Base.FSharp.dll"
#r "System.Xml.Linq.dll"
#else
namespace Aardvark.Opc
#endif

open System
open System.Xml
open System.Xml.Linq
open Aardvark.Base

module private XmlHelpers = 
    
    let xname str = XName.op_Implicit str
    let xattr (elem: XElement) sname = elem.Attribute(xname sname).Value
    let elem name (v : XElement) = 
        match v.Elements(xname name) |> Seq.toList with
            | []-> failwithf "could not get value: %s" name
            | [x] -> x
            | _ -> failwithf "could not get value (not unique): %s" name
    let elem' name (v : XElement) = 
        match v.Elements(xname name) |> Seq.toList with
            | []-> None
            | [x] -> Some x
            | _ -> None
    let elems name (p : XElement) = 
        p.Elements (xname name) 

    let xvalue (p : XElement) = p.Value.Trim()

    let prop name (p : XElement) = 
        (elem name p).Value.Trim()

type Texture = { fileName : string; weights : string }
type VertexOrder = RowMajor | ColumnMajor 
type GeometryType = QuadList
type DiffuseColorCoordinates = string

type PatchFileInfo =
    {
        BaseDir             : string

        TagList             : list<string>
        GeometryType        : GeometryType
        QuadVertexSortOrder : VertexOrder

        Local2Global        : Trafo3d
        GlobalBoundingBox   : Box3d
        LocalBoundingBox    : Box3d

        Local2Global2d      : Trafo3d
        GlobalBoundingBox2d : Box3d
        LocalBoundingBox2d  : Box3d

        Positions           : string
        Positions2d         : string
        Normals             : string
        Offsets             : string

        Coordinates         : list<DiffuseColorCoordinates>
        Textures            : list<Texture>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PatchFileInfo =
    open XmlHelpers

    [<AutoOpen>]
    module Parsers =
        //open FParsec
        //open FParsec.CharParsers

        let majority = function
            | "RowMajor" -> RowMajor
            | "ColumnMajor" -> ColumnMajor
            | s -> failwithf "could not parse majority: %s" s

        let geomtryType = function
            | "QuadList" -> QuadList
            | s -> failwithf "could not parse geometry type: %s" s

        let (|Int32|_|) (s : string) = 
            match System.Int32.TryParse ( s.Trim() ) with
                | (true,v) -> Some v
                | _ -> None

        let tup2V2i (s : string) = 
            match s.Split(',') with
                | [|Int32 x; Int32 y|] -> V2i(x,y)
                | _ -> failwithf "Could not parse: %A" s 

    let private ofXDoc (doc : XDocument) (baseDir) (name : string) =
        let patch = doc.Descendants(xname "Aardvark").Descendants(xname "Patch") |> Seq.head

        let trafo (m : M44d) =
            Trafo3d(m, m.Inverse)

        let getTex xs = 
            { fileName = Path.combine [baseDir; "Images"; prop "DiffuseColor1Texture" xs]
              weights  = prop "DiffuseColor1Weights" xs }

        {
            BaseDir             = Path.combine [baseDir; "Patches"; name]
            TagList             = patch |> elems "TagList" |> Seq.toList |> List.map xvalue   

            GeometryType        = patch |> prop "GeometryType"        |> geomtryType
            QuadVertexSortOrder = patch |> prop "QuadVertexSortOrder" |> majority
                                                                      
            Local2Global        = patch |> prop "Local2Global"        |> M44d.Parse |> trafo
            GlobalBoundingBox   = patch |> prop "GlobalBoundingBox"   |> Box3d.Parse
            LocalBoundingBox    = patch |> prop "LocalBoundingBox"    |> Box3d.Parse

            Local2Global2d      = patch |> prop "Local2Global"        |> M44d.Parse |> trafo
            GlobalBoundingBox2d = patch |> prop "GlobalBoundingBox"   |> Box3d.Parse
            LocalBoundingBox2d  = patch |> prop "LocalBoundingBox"    |> Box3d.Parse

            Positions           = patch |> prop "Positions"   
            Positions2d         = ""
            Normals             = ""
            Offsets             = ""
            Textures            =  patch |> elems "Textures" |> Seq.map getTex |> Seq.toList
            Coordinates         =  
                patch |> elems "Coordinates" |> Seq.toList 
                      |> List.map (string << prop "DiffuseColor1Coordinates")
        }    

    let load (dir : string) (f : string) =
        let doc   = XDocument.Load (Path.combine [dir; "Patches"; f; "Patch.xml"])
        ofXDoc doc dir f



type QTree<'a> = Node of 'a * array<QTree<'a>>
                | Leaf of 'a

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module QTree =
    let rec map f t = 
        match t with
            | Leaf v -> Leaf (f v)
            | Node(v,children) ->
                Node(f v, children |> Array.map (map f))

    let rec mapLevel (l : int) f t = 
        match t with
            | Leaf v -> Leaf (f l v)
            | Node(v,children) ->
                Node(f l v, children |> Array.map (mapLevel (l + 1) f))


//
//module Test =
//    open Aardvark.Base
//    open Aardvark.Base.Rendering
//
//    let inter = """
//    type IA =
//        abstract member B : unit -> int
//"""
//    let impl = """
//type A_() =
//    interface IA with
//        member x.B () = 2
//"""
//    let mkT = """A_() :> IA; """
//
//    let mk i =
//        impl.Replace("_", sprintf "_%d" i)
//
//    let mkC i =
//        mkT.Replace("_", sprintf "_%d" i)
//
//    let writeTestCase file = 
//        System.IO.File.AppendAllText(file,inter)
//        for i in 0 .. 5000 do
//            let impl = mk i
//            System.IO.File.AppendAllText(file,impl) 
//
//        System.IO.File.AppendAllText(file, "\nlet a = [|")
//        for i in 0 .. 1000 do 
//            let mker = mkC i
//            System.IO.File.AppendAllText(file, mker )
//        System.IO.File.AppendAllText(file, "|]")
//
//    let test2 () =
//        
//        let mutable l = 0
//        for u in Wahh.UHHH.a do
//            l <- u.B()
//
//        let sw = System.Diagnostics.Stopwatch()
//
//        let mutable jasdf = Unchecked.defaultof<_>
//
//        sw.Start()
//        for i in 0 .. 1000000 do
//            for u in Wahh.UHHH.a do
//                jasdf <- u
//        sw.Stop()
//        let a = sw.Elapsed.TotalMilliseconds
//        printfn "empty loop elapsed: %A" a
//
//        sw.Restart()
//        for i in 0 .. 1000000 do
//            for u in Wahh.UHHH.a do
//                l <- u.B()
//        sw.Stop()
//        printfn "complete loop took: %A" sw.Elapsed.TotalMilliseconds
//        printfn "%A µs" (sw.Elapsed.TotalMilliseconds - a)
//
//
//    open System.Reflection
//    open System.Threading
//    open System.Reflection.Emit
//
//    let test3 () =
////        let assemblyBuilder = Thread.GetDomain().DefineDynamicAssembly("asdjf", AssemblyBuilderAccess.Run);
////        let modd = assemblyBuilder.DefineDynamicModule("tmpModule")
//        ()