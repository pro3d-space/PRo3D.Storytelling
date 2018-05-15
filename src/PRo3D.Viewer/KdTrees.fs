namespace Aardvark.VRVis.Opc

[<AutoOpen>]
module KdTrees =
    open MBrace.FsPickler
    open MBrace.FsPickler.Combinators                                                                                                                                                                                                                                                                                                                                                
    open Aardvark.VRVis
    open Aardvark.Geometry
    open Aardvark.Base
    open Aardvark.Base.Coder
    open Aardvark.SceneGraph.Opc

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
        
    let loadKdtree p =
        //Log.startTimed "loading tree"
        use b = new BinaryReadingCoder(System.IO.File.OpenRead(p))
        let mutable treeOida = Unchecked.defaultof<KdIntersectionTree>
        b.CodeT(&treeOida)
        //Log.stop()        
        ConcreteKdIntersectionTree(treeOida, Trafo3d.Identity)

    let save path (b : BinarySerializer) (d : 'a) =    
        let arr =  b.Pickle d
        System.IO.File.WriteAllBytes(path, arr);
        d
    
    let loadAs<'a> path (b : BinarySerializer) : 'a =
        let arr = System.IO.File.ReadAllBytes(path)
        b.UnPickle arr

    let loadKdTrees' (h : PatchHierarchy) (trafo:Trafo3d) (load : bool) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
                    
        //ObjectBuilder

        let masterKdPath = 
          mode |> ViewerModality.matchy
            (h |> PatchHierarchy.getMasterKdTreePath) 
            (h |> PatchHierarchy.getMasterKdTreePath2d)

        let cacheFile = System.IO.Path.ChangeExtension(masterKdPath, ".cache")

        if System.IO.File.Exists(cacheFile) then
            Log.line "Found lazy kdtree cache"
            if load then
              loadAs<array<Box3d*Level0KdTree>> cacheFile b |> HMap.ofArray
            else
              HMap.empty
        else
            
            Log.line "did not find lazy kdtree cache"
            let patchInfos =
                h.tree |> QTree.getLeaves |> Seq.map(fun x -> x.info)

            let kd0Paths = 
                patchInfos 
                  |> Seq.map(fun x -> 
                    mode 
                    |> ViewerModality.matchy 
                       (x.Name |> PatchHierarchy.getPatchKdTreePath h) 
                       (x.Name |> PatchHierarchy.getPatchKdTreePath2d h)
                    )

            let kd0PathsExist = 
                kd0Paths |> Seq.forall(System.IO.File.Exists)
        
            match (System.IO.File.Exists(masterKdPath), kd0PathsExist) with
                | (true, false) -> 
                     Log.line "Found master kdtree - loading incore"
                     let tree = loadKdtree masterKdPath                     
                     let kd = {
                        kdTree = tree;
                        boundingBox = tree.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
                     }
                     HMap.add kd.boundingBox (InCoreKdTree kd) HMap.empty
                | (true, true) ->                                                        
                     Log.line "Found master kdtree and patch trees - building lazy kdtree cache"
                             
                     let num = kd0Paths |> List.ofSeq |> List.length

                     let bla = 
                        kd0Paths
                            |> Seq.zip patchInfos
                            |> Seq.mapi(
                                fun i (info, kdPath) ->                                
                                    let t = loadKdtree kdPath
                                    let patchPosF = 
                                      mode |> ViewerModality.matchy (PatchHierarchy.getPatchPositionPath) 
                                                                    (PatchHierarchy.getPatch2dPositionPath)

                                    let lazyTree : LazyKdTree = {
                                        kdTree        = None
                                        objectSetPath = info.Name |> patchPosF h
                                        kdtreePath    = kdPath
                                        affine        = 
                                          mode 
                                            |> ViewerModality.matchy info.Local2Global info.Local2Global2d
                                        boundingBox   = t.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
                                    }
                                    Log.line "%f %%" (float i / float num)
                                    (lazyTree.boundingBox, (LazyKdTree lazyTree))
                            )
                            |> Seq.toArray 

                     bla |> save cacheFile b |> ignore
                        
                     if load then
                       bla |> HMap.ofArray
                     else
                       HMap.empty
                | _ ->
                    Log.warn "Could not find level 0 kdtrees"
                    HMap.empty
    
    let loadKdTrees (h : PatchHierarchy) (trafo:Trafo3d) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
      loadKdTrees' (h) (trafo) (true) mode b