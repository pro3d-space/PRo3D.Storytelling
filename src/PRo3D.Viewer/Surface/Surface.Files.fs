namespace PRo3D.Surfaces

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Opc

open Aardvark.VRVis.Opc

open Aardvark.UI.Operators
open Aardvark.UI.Trafos  

open PRo3D
open PRo3D.Groups


module DebugKdTreesX = 
    open Aardvark.VRVis.Opc.KdTrees
    open MBrace.FsPickler
    open Aardvark.Geometry
   
    let getInvalidIndices3f (positions : V3f[]) =
        positions |> List.ofArray |> List.mapi (fun i x -> if x.AnyNaN then Some i else None) |> List.choose id    

    let loadTriangles (kd : LazyKdTree) =
        
        let positions = kd.objectSetPath |> Aara.fromFile<V3f>
                
        let invalidIndices = getInvalidIndices3f positions.Data |> List.toArray
        let size = positions.Size.XY.ToV2i()
        let indices = CSharpUtils.IndexComputation.ComputeIndexArray(size,invalidIndices);
                  
        Log.warn "num of inv_indices: %A" invalidIndices.Length
        Log.warn "num of indices: %A" indices.Length
               
        let data = 
            positions.Data |> Array.map (fun x ->  x.ToV3d() |> kd.affine.Forward.TransformPos)

        let triangles =             
          indices 
            |> Seq.map(fun x -> data.[x])
            |> Seq.chunkBySize 3
            |> Seq.map(fun x -> Triangle3d(x))
            |> Seq.filter(fun x -> (triangleIsNan x |> not)) |> Seq.toArray
            |> TriangleSet
    
        triangles

    let loadObjectSet (cache : hmap<string, ConcreteKdIntersectionTree>) (lvl0Tree : Level0KdTree) =             
      match lvl0Tree with
        | InCoreKdTree kd -> 
          kd.kdTree, cache
        | LazyKdTree kd ->             
          let kdTree, cache =
            match kd.kdTree with
            | Some k -> k, cache
            | None -> 
              let tree = cache |> HMap.tryFind (kd.boundingBox.ToString())
              match tree with
              | Some t ->                 
                t, cache
              | None ->                                     
                Log.line "cache miss %A- loading kdtree" kd.boundingBox
            
                let mutable tree = KdTrees.loadKdtree kd.kdtreePath      
                let triangles = kd |> loadTriangles
                
                tree.KdIntersectionTree.ObjectSet <- triangles
                            
                let key = tree.KdIntersectionTree.BoundingBox3d.ToString()                
                tree, (HMap.add key tree cache)
          kdTree, cache

    let getTriangle (set : TriangleSet) (index : int) : Triangle3d =
      let pi = index * 3
      let pl = set.Position3dList
      Triangle3d(pl.[pi], pl.[pi+1], pl.[pi + 2])

    let isNotOversized (size) (triangle:Triangle3d) =      
      ((V3d.Distance(triangle.P0, triangle.P1) < size) && 
       (V3d.Distance(triangle.P0, triangle.P2) < size) &&
       (V3d.Distance(triangle.P1, triangle.P2) < size))

    let intersectKdTrees bb (hitObject : PRo3D.Surfaces.Surface) (cache : hmap<string, ConcreteKdIntersectionTree>) (ray : FastRay3d) (kdTreeMap: hmap<Box3d, KdTrees.Level0KdTree>) = 

        let kdtree, c = kdTreeMap |> HMap.find bb |> loadObjectSet cache

        let kdi = kdtree.KdIntersectionTree 
        let mutable hit = ObjectRayHit.MaxRange
                        
        try
          let hitFilter = //true means being omitted
            fun (a:IIntersectableObjectSet) (b:int) _ _ -> 
            let triangles = a :?> TriangleSet //TODO TO crashes if not encountering a triangleset
            b |> getTriangle triangles |> isNotOversized hitObject.triangleSize.value |> not // = tooBig            

          if kdi.Intersect(ray, null, Func<IIntersectableObjectSet,int,int, RayHit3d,bool>(hitFilter), 0.0, Double.MaxValue, &hit) then              
              Some (hit.RayHit.T, hitObject),c
          else            
              None,c
        with 
          | e -> 
            Log.error "[DebugKdtrees] error in kdtree intersection" 
            Log.error "%A" e
            None,c

//    let loadKdTrees' (h : PatchHierarchy) (trafo:Trafo3d) (load : bool) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
                      
//          //ObjectBuilder
  
//          let masterKdPath = 
//            mode |> ViewerModality.matchy
//              (h.kdTreeAggZero_FileAbsPath) 
//              (h.kdTreeAggZero2d_FileAbsPath)
  
//          let cacheFile = System.IO.Path.ChangeExtension(masterKdPath, ".cache")
  
//          if System.IO.File.Exists(cacheFile) then
//              Log.line "Found lazy kdtree cache"
//              if load then
//                loadAs<list<Box3d*Level0KdTree>> cacheFile b |> HMap.ofList
//              else
//                HMap.empty
//          else
              
//              Log.line "did not find lazy kdtree cache"
//              let patchInfos =
//                  h.tree |> QTree.getLeaves |> Seq.toList |> List.map(fun x -> x.info)
  
//              let kd0Paths = 
//                  patchInfos 
//                    |> List.map(fun x -> h.kdTree_FileAbsPath x.Name 0 mode) |> List.sort 
  
//              let kd0PathsExist = 
//                  kd0Paths |> List.filter(System.IO.File.Exists) |> List.length > 0
          
//              match (System.IO.File.Exists(masterKdPath), kd0PathsExist) with
//                  | (true, false) -> 
//                       Log.line "Found master kdtree - loading incore"
//                       let tree = loadKdtree masterKdPath                     
//                       let kd = {
//                          kdTree = tree;
//                          boundingBox = tree.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
//                       }
//                       HMap.add kd.boundingBox (InCoreKdTree kd) HMap.empty
//                  | (true, true) ->   
//                       Log.line "Found master kdtree and patch trees"
//                       Log.startTimed "building lazy kdtree cache"
                               
//                       let num = kd0Paths |> List.ofSeq |> List.length                     
  
//                       let bla = 
//                          kd0Paths
//                              |> List.zip patchInfos
//                              |> List.mapi(
//                                  fun i (info, kdPath) ->                                
//                                      let t = loadKdtree kdPath
//                                      let pos =
//                                        match mode with
//                                        | XYZ -> info.Positions
//                                        | SvBR -> info.Positions2d.Value
  
//                                      let lazyTree : LazyKdTree = {
//                                          kdTree        = None
//                                          objectSetPath = h.opcPaths.Patches_DirAbsPath +/ info.Name +/ pos
//                                          kdtreePath    = kdPath
//                                          affine        = 
//                                            mode 
//                                              |> ViewerModality.matchy info.Local2Global info.Local2Global2d
//                                          boundingBox   = t.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
//                                      }
//                                      Report.Progress(float i / float num)
  
//                                      (lazyTree.boundingBox, (LazyKdTree lazyTree))
//                              )                            
                        
//                       Log.stop()
                        
//                       bla |> save cacheFile b |> ignore
                          
//                       if load then
//                         bla |> HMap.ofList
//                       else
//                         HMap.empty
//                  | _ ->
//                      Log.warn "Could not find level 0 kdtrees"
//                      HMap.empty
      
//    let loadKdTrees (h : PatchHierarchy) (trafo:Trafo3d) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
//        loadKdTrees' (h) (trafo) (true) mode b
  
module Files = 
       open System.IO
       open Aardvark.Prinziple

       type DiscoverFolder = 
          | OpcFolder of string
          | Directory of string  
       
       /// <summary>
       /// checks if "path" is a valid opc folder containing "images", "patches", and patchhierarchy.xml
       /// </summary>
       let isOpcFolder (path : string) = 
           let imagePath = Path.combine [path; "images"]
           let patchPath = Path.combine [path; "patches"]
           (Directory.Exists imagePath) &&
           (Directory.Exists patchPath) && 
            File.Exists(patchPath + "\\patchhierarchy.xml")

       /// <summary>
       /// checks if "path" is a valid surface folder
       /// </summary>        
       //let isSurfaceFolder (path : string) =
       //    Directory.GetDirectories(path) |> Seq.forall isOpcFolder
       let isSurfaceFolder (path : string) =
            let subdirs = Directory.GetDirectories(path)
            match subdirs.IsEmptyOrNull() with
                | true -> false
                | _ -> subdirs |> Seq.forall isOpcFolder

       let discover (p : string -> bool) path : list<string> =
         if Directory.Exists path then
           Directory.EnumerateDirectories path                     
             |> Seq.filter p            
             |> Seq.toList
         else List.empty

       /// returns all valid surface folders in "path"   
       let discoverSurfaces path = 
         discover isSurfaceFolder path          

       let discoverOpcs path = 
         discover isOpcFolder path

       /// <summary>
       /// should open zip and call isOPCFolder
       /// </summary>        
       let isZippedOpcFolder (path : string) = 
         File.Exists(Path.ChangeExtension(path, "zip"))
       
       let private retrieve (f :string -> string) (path : string) : list<string> =
           if path = String.Empty then failwith "encountered empty opc path"

           if isOpcFolder path then
               [path.Replace(@"\", @"\\\\")]
           elif isZippedOpcFolder path then                
               [path.Replace(@"\", @"\\\\").Replace(".zip", "")]
           elif Directory.Exists path then
               let opcDirs = 
                 Directory.GetDirectories(path)
                   |> Seq.map (fun a -> a.Replace(@"\", @"\\\\"))
                   |> Seq.filter isOpcFolder
                   |> Seq.map f
                   |> Seq.toList            

               let zipped =
                 Directory.GetFiles(path)
                   |> Seq.map (fun a -> a.Replace(@"\", @"\\\\"))
                   |> Seq.filter isZippedOpcFolder
                   |> Seq.map (fun a -> a.Replace(".zip", ""))
                   |> Seq.map f
                   |> Seq.toList            

               opcDirs @ zipped
           else []

       let toDiscoverFolder path = 
        if path |> isOpcFolder || path |> isZippedOpcFolder then
          OpcFolder path
        else
          Directory path

       let tryDirectoryExists path = 
        if Directory.Exists path then Some path else None

       let rec superDiscovery (input : string) :  string * list<string> =
        match input |> toDiscoverFolder with
        | Directory path -> 
          let opcs = 
            path 
              |> Directory.EnumerateDirectories 
              |> Seq.toList 
              |> List.map(fun x -> superDiscovery x |> snd) |> List.concat 
          input, opcs
        | OpcFolder p -> input,[p]

       let rec superDiscoveryMultipleSurfaceFolder (input : string) : list<string> =
        let isSurfFolder = input |> isSurfaceFolder
        match isSurfFolder with
            | true -> [input]
            | _-> input 
                    |> Directory.EnumerateDirectories 
                    |> Seq.toList 
                    |> List.map(fun x -> superDiscoveryMultipleSurfaceFolder x ) |> List.concat

       /// <summary>
       /// returns all valid OPCs in "path"
       /// </summary>
       let getOPCPaths path = 
         path |> retrieve (id)
           
       /// <summary>
       /// returns all valid OPCs in "path"
       /// </summary>
       let getOPCNames path = 
         path |> retrieve (Path.GetFileName)            

       let expandNamesToPaths (path : string) (names : List<string>) =
           names |> List.map(fun a -> (Path.combine [path; a]).Replace(@"\", @"\\\\"))
       
       let getSurfaceFolder (surface:PRo3D.Surfaces.Surface) (scenePath:option<string>) =
         if surface.relativePaths 
         then
           match scenePath with
             | Some sp -> 
                 let x = Path.Combine((Path.GetDirectoryName sp),"Surfaces")
                 let relPath = Path.Combine(x, surface.name)
                 if Directory.Exists relPath then Some relPath else None
             | None -> None
         else
           match surface.surfaceType with
             | SurfaceType.SurfaceOBJ -> 
               let p = Path.GetDirectoryName surface.importPath
               if Directory.Exists p then Some p else None
             |_-> 
               let p = surface.importPath
               if Directory.Exists p then Some p else None

       /// strips away parts of a file path until the remaining depth is reached
       let relativePath (path : string) (remaining : int) = 
           path.Split(Path.DirectorySeparatorChar) 
               |> List.ofArray 
               |> List.rev 
               |> List.take remaining 
               |> List.rev 
               |> Path.combine

       let sceneRelativePath (path : string) = 
           relativePath path 5

       let surfaceRelativePath (path : string) =
           relativePath path 4

       let expandLazyKdTreePaths (scenePath : option<string>) (surfaces : hmap<Guid, PRo3D.Surfaces.Surface>) (sgSurfaces : hmap<Guid, SgSurface>) =
          sgSurfaces 
             |> HMap.map (fun _ s -> 
                 match s.picking with
                    | Picking.KdTree ks ->
                        let surf = surfaces |> HMap.find s.surface
                        let kd = 
                            ks |> HMap.map (fun _ k -> 
                                    match k with 
                                        | KdTrees.Level0KdTree.LazyKdTree lk when surf.relativePaths && scenePath.IsSome -> // scene is portable                                        
                                            let path = Path.Combine((Path.GetDirectoryName scenePath.Value),"Surfaces")
                                            let kdTreeSub   = lk.kdtreePath    |> sceneRelativePath
                                            let triangleSub = lk.objectSetPath |> sceneRelativePath
                                         
                                            KdTrees.LazyKdTree { 
                                                lk with 
                                                    kdtreePath    = Path.Combine(path, kdTreeSub)
                                                    objectSetPath = Path.Combine(path, triangleSub)
                                            }
                                        | KdTrees.Level0KdTree.LazyKdTree lk -> // surfaces have absolute paths
                 
                                            let kdTreeSub   = lk.kdtreePath    |> surfaceRelativePath
                                            let triangleSub = lk.objectSetPath |> surfaceRelativePath                                                                                
                 
                                            KdTrees.LazyKdTree {
                                                lk with 
                                                    kdtreePath    = Path.Combine(surf.importPath, kdTreeSub)
                                                    objectSetPath = Path.Combine(surf.importPath, triangleSub)
                                            }
                                        | KdTrees.Level0KdTree.InCoreKdTree ik -> KdTrees.InCoreKdTree ik // kdtrees can be loaded as is
                            )
                        { s with picking = Picking.KdTree kd }
                    | Picking.PickMesh ms -> s
           )                

       let makeSurfaceRelative guid (surfaceModel : SurfaceModel) (scenePath : option<string>) =
           match scenePath with
               | Some sp ->
                   let surf = SurfaceModel.getSurface surfaceModel guid
                   match surf with
                       | Some s ->
                           // create target dir
                           let targetSurfaceDir = Path.Combine((Path.GetDirectoryName sp),"Surfaces")
                           if not (Directory.Exists targetSurfaceDir) then
                               Directory.CreateDirectory(targetSurfaceDir) |> ignore

                           // copy data
                           match s with
                               |Leaf.Surfaces sf -> 
                                   let targetDir = Path.Combine(targetSurfaceDir, sf.name)
                                   Copy.copyAll sf.importPath targetDir true

                                   // update opc paths and scene
                                   let s' = { sf with opcPaths = expandNamesToPaths targetDir sf.opcNames; relativePaths = true }
                                   surfaceModel |> SurfaceModel.updateSingleSurface s'
                               | _ ->
                                   Log.error "surface %s not found" (guid.ToString())
                                   surfaceModel
                       | None ->
                           Log.error "surface %s not found" (guid.ToString())
                           surfaceModel
               | None -> 
                   Log.error "can't make surface relative. no valid scene path has not been saved yet."
                   surfaceModel

  