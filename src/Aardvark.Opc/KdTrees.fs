namespace Aardvark.Opc

[<AutoOpen>]
module Serialization =
    open MBrace.FsPickler
    open System.IO
        
    let registry = new CustomPicklerRegistry()    
    let cache = PicklerCache.FromCustomPicklerRegistry registry

    let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)    

    let save path (d : 'a) =    
        let arr = binarySerializer.Pickle d    
        File.WriteAllBytes(path, arr);
        d

    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)

[<AutoOpen>]
module KdTrees =
    open MBrace.FsPickler
    open MBrace.FsPickler.Combinators                                                                                                                                                                                                                                                                                                                                                
    open Aardvark.VRVis
    open Aardvark.Geometry
    open Aardvark.Base
    open Aardvark.Base.Coder

    type LazyKdTree = {
        kdTree        : option<ConcreteKdIntersectionTree>
        affine        : Trafo3d
        boundingBox   : Box3d        
        kdtreePath    : string
        objectSetPath : string
    }

    type InCoreKdTree = {
        kdTree      : ConcreteKdIntersectionTree
        boundingBox : Box3d
    }

    type Level0KdTree = 
        | LazyKdTree of LazyKdTree
        | InCoreKdTree of InCoreKdTree

    let makeInCoreKd a = 
        {
            kdTree = new ConcreteKdIntersectionTree()
            boundingBox = a
        }

    let makeLazyTree a b c d =
        {
            kdTree = None
            affine    = a
            boundingBox  = b
            kdtreePath   = c
            objectSetPath  = d    
        }

    let incorePickler : Pickler<InCoreKdTree> =
        Pickler.product makeInCoreKd
        ^. Pickler.field (fun s -> s.boundingBox)     Pickler.auto<Box3d>

    let lazyPickler : Pickler<LazyKdTree> =
        Pickler.product makeLazyTree
        ^+ Pickler.field (fun (s:LazyKdTree) -> s.affine)       Pickler.auto<Trafo3d>
        ^+ Pickler.field (fun (s:LazyKdTree) -> s.boundingBox)  Pickler.auto<Box3d>
        ^+ Pickler.field (fun s -> s.kdtreePath)                Pickler.string
        ^. Pickler.field (fun s -> s.objectSetPath)             Pickler.string

    let level0KdTreePickler : Pickler<Level0KdTree> =
        Pickler.sum (fun x k1 k2->
            match x with
                | InCoreKdTree k -> k1 k
                | LazyKdTree k -> k2 k)
        ^+ Pickler.case InCoreKdTree incorePickler
        ^. Pickler.case LazyKdTree lazyPickler
    
    //FsPickler.RegisterPicklerFactory (fun _ -> level0KdTreePickler )
    registry.RegisterFactory (fun _ -> level0KdTreePickler)

    let loadKdtree p =
        //Log.startTimed "loading tree"
        use b = new BinaryReadingCoder(System.IO.File.OpenRead(p))
        let mutable treeOida = Unchecked.defaultof<KdIntersectionTree>
        b.CodeT(&treeOida)
        //Log.stop()
        //  treeOida
        ConcreteKdIntersectionTree(treeOida, Trafo3d.Identity)