namespace PRo3D.Surfaces

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
open Aardvark.Opc.Internal
open Aardvark.SceneGraph

open PRo3D
open PRo3D.Groups

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =
    open Aardvark.SceneGraph.SgFSharp.Sg
    open Aardvark.UI.Trafos        

    module Sg =
      open Aardvark.Prinziple

      let mutable hackRunner = None

      let createPlainSceneGraph (runtime : IRuntime) (signature : IFramebufferSignature) (scene : OpcScene) (createKdTrees)
          : (ISg * list<PatchHierarchy> * hmap<Box3d,Level0KdTree>) =

          let runner = 
              match hackRunner with
                  | None -> 
                      printfn "create runner"
                      let  r = runtime.CreateLoadRunner 2
                      hackRunner <- Some (r)
                      r
                  | Some h -> h


          let bb = scene.boundingBox      
          let preTransform = scene.preTransform
      
          let patchHierarchies = 
            scene.patchHierarchies |> Seq.map Prinziple.registerIfZipped |> Seq.map PatchHierarchy.load |> Seq.toList
          
          let getPatchKdTreePath (h:PatchHierarchy) patchName = 
              let rootpatch = h.tree |> QTree.getRoot 

              let fileName = sprintf "%s-0.aakd" patchName            
              Path.combine [h.baseDir; "patches"; rootpatch.info.Name; fileName]

          let getPatchPositionPath (h:PatchHierarchy) patchName =
              let fileName = "positions.aara"
              Path.combine [h.baseDir; "patches"; patchName; fileName]

          let getMasterKdTreePath (h:PatchHierarchy) =
              let rootpatch = h.tree |> QTree.getRoot 
                                      
              let fileName = sprintf "%s-0.aakd" rootpatch.info.Name            
              Path.combine [h.baseDir; "patches"; rootpatch.info.Name; fileName]      

          Log.startTimed "Creating KdTree"
          let loadKdTrees (h : PatchHierarchy) : hmap<Box3d,Level0KdTree> =
                          
              let masterKdPath = 
                  h |> getMasterKdTreePath

              let cacheFile = System.IO.Path.ChangeExtension(masterKdPath, ".cache")

              if System.IO.File.Exists(cacheFile) then
                  Log.line "Found lazy kdtree cache"
                  try
                      (Serialization.loadAs<array<Box3d*Level0KdTree>> cacheFile) |> HMap.ofArray
                  with
                    | _ -> 
                      Log.error "could not load kdtree cache file. please delete %A" cacheFile
                      HMap.empty
              else
                  
                  Log.line "did not find lazy kdtree cache"
                  let patchInfos =
                      h.tree |> QTree.getLeaves |> Seq.map(fun x -> x.info)

                  let kd0Paths = 
                      patchInfos |> Seq.map(fun x -> x.Name |> getPatchKdTreePath h)

                  let kd0PathsExist = 
                      kd0Paths |> Seq.forall(System.IO.File.Exists)
              
                  match (System.IO.File.Exists(masterKdPath), kd0PathsExist) with
                      | (true, false) -> 
                           Log.line "Found master kdtree - loading incore"
                           let tree = loadKdtree masterKdPath                     
                           let kd = {
                              kdTree = tree;
                              boundingBox = tree.KdIntersectionTree.BoundingBox3d.Transformed(scene.preTransform)
                           }
                           HMap.add kd.boundingBox (InCoreKdTree kd) HMap.empty
                      | (true, true) ->                                                        
                           Log.line "Found master kdtree and patch trees"
                                                    
                           let bla = 
                              kd0Paths
                                  |> Seq.zip patchInfos
                                  |> Seq.map(
                                      fun (info, kdPath) ->                                
                                          let t = loadKdtree kdPath
                                          let lazyTree : LazyKdTree = {
                                              kdTree        = None
                                              objectSetPath = info.Name |> getPatchPositionPath h
                                              kdtreePath    = kdPath
                                              affine        = info.Local2Global
                                              boundingBox   = t.KdIntersectionTree.BoundingBox3d.Transformed(scene.preTransform)
                                          }
                                          (lazyTree.boundingBox, (LazyKdTree lazyTree))
                                  )
                                  |> Seq.toArray 

                           bla |> Serialization.save cacheFile |> ignore 
                              
                           bla |> HMap.ofArray
                      | _ ->
                          Log.warn "Could not find level 0 kdtrees"
                          HMap.empty
          
          let kdTreesPerHierarchy =
              [| for h in patchHierarchies do
                  if createKdTrees then   
                      let kdMap = loadKdTrees h 

                      yield kdMap
                  else 
                      yield HMap.empty
              |]

          let totalKdTrees = kdTreesPerHierarchy.Length
          Log.line "creating %d kdTrees" totalKdTrees

          let kdTrees = 
              kdTreesPerHierarchy                     
                  |> Array.Parallel.mapi (fun i e ->
                      Log.start "creating kdtree #%d of %d" i totalKdTrees
                      let r = e
                      Log.stop()
                      r
                  )
                  |> Array.fold (fun a b -> HMap.union a b) HMap.empty

          Log.stop()


          // create level of detail hierarchy (Sg)
          let g = 
            patchHierarchies 
            |> List.map (fun h ->                     
              let sg = 
                Sg.patchLod signature runner h.baseDir 
                  scene.lodDecider scene.useCompressedTextures                                 
                  (PatchLod.toRoseTree h.tree)
              sg) 
            |> Sg.ofList
          

          let lodVisEnabled = Mod.init false
          
          let sg = g |> Sg.transform preTransform
                                  
          sg, patchHierarchies, kdTrees
    
    module SurfaceAttributes = 
        open System.Xml
            
        let get (name : string) (node : XmlNode)=
            node.SelectSingleNode(name).InnerText.Trim()

        let parseMap (index : int)(node : XmlNode) : ScalarLayer =        
            { 
                label        = node |> get "Label" 
                actualRange  = node |> get "ChannelsActualRange"  |> Range1d.Parse
                definedRange = node |> get "ChannelsDefinedRange" |> Range1d.Parse
                index        = index
            }

        let parseTexture (index : int)(node : XmlNode) : TextureLayer =
            { 
                label = node |> get "Label" 
                index = index
            }

        let parseLayer (index : int)(node : XmlNode) : AttributeLayer = 
            let typ = node |> get "Type"
            match typ with
                | "Texture" -> TextureLayer (parseTexture index node)
                | "Map"     -> ScalarLayer (parseMap index node)
                | _         -> failwith "type not supported"

        let layers (doc : XmlDocument) =
            let nodes = doc.SelectNodes "/Aardvark/SurfaceAttributes/AttributeLayers/AttributeLayer"
            nodes
             |> Seq.cast<XmlNode>
             |> Seq.mapi parseLayer

        let read (path:string) =
            let doc = new XmlDocument() in doc.Load path
            doc |> layers
    
    module Files = 

        /// checks if "path" is a valid opc folder
        let isOPCPath (path : string) = 
            let imagePath = Path.combine [path; "images"]
            let patchPath = Path.combine [path; "patches"]
            let dirsExist = (System.IO.Directory.Exists imagePath) && (System.IO.Directory.Exists patchPath)
            let storageExists = System.IO.File.Exists(patchPath + "\\patchhierarchy.xml")
            if dirsExist && storageExists then
                true
            else false

        /// checks if "path" is a valid surface folder
        let isSurfaceFolder (path : string) =
            let mutable isValid = false
            let subfolders = System.IO.Directory.GetDirectories(path)
            for s in subfolders do
                isValid <- (isOPCPath(s) || isValid)
            isValid
                    
        /// returns all valid OPCs in "path"
        let getOPCPaths path = 
            if path = String.Empty then failwith "encountered empty opcpath"

            if isOPCPath path then
                [path.Replace(@"\", @"\\\\")]
            elif System.IO.Directory.Exists path then
                System.IO.Directory.GetDirectories(path)
                    |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                    |> Seq.choose (fun a -> if isOPCPath a then Some a else None)
                    |> Seq.toList 
            else []

        /// returns all valid OPCs in "path"
        let getOPCNames path = 
            if path = String.Empty then failwith "encountered empty opcpath"

            if isOPCPath path then
                [path.Replace(@"\", @"\\\\")]
            elif System.IO.Directory.Exists path then
                System.IO.Directory.GetDirectories(path)
                    |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                    |> Seq.choose (fun a -> if isOPCPath a then Some a else None)
                    |> Seq.map (fun a -> System.IO.Path.GetFileName a; )
                    |> Seq.toList 
            else []

        let expandNamesToPaths (path : string) (names : List<string>) =
            names |> List.map(fun a -> 
                let p = (Path.combine [path; a]).Replace(@"\", @"\\\\")
                //Log.warn "%s" p
                p)

        /// returns all valid surface folders in "path"   
        let discover path = 
            if System.IO.Directory.Exists path then
                System.IO.Directory.EnumerateDirectories path 
                    // |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                    |> Seq.filter (fun a -> isSurfaceFolder a)
                    |> Seq.toList 
            else []

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
                    | SurfaceType.SurfaceOBJ -> let p = System.IO.Path.GetDirectoryName surface.importPath
                                                if Directory.Exists p then Some p else None
                    |_-> let p = surface.importPath
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

        let expandLazyKdTreePaths (scenePath : option<string>) (surfaces : hmap<Guid, PRo3D.Surfaces.Surface>) (sgSurfaces : hmap<Guid, SgSurfaces>) =
           let sgS = sgSurfaces |> HMap.filter (fun id sg -> (sgSurface.isSgOPC sg))
           sgS 
              |> HMap.map (fun _ s -> 
                  let s = (sgSurface.toSgOPC s)
                  let surf = surfaces |> HMap.find s.surface
                  let kd =
                      s.kdTree |> HMap.map (fun _ k -> 
                          match k with 
                              | Level0KdTree.LazyKdTree lk when surf.relativePaths && scenePath.IsSome -> // scene is portable                                        
                                  let path = Path.Combine((Path.GetDirectoryName scenePath.Value),"Surfaces")
                                  let kdTreeSub   = lk.kdtreePath    |> sceneRelativePath
                                  let triangleSub = lk.objectSetPath |> sceneRelativePath
                                          
                                  LazyKdTree { 
                                      lk with 
                                          kdtreePath    = Path.Combine(path, kdTreeSub)
                                          objectSetPath = Path.Combine(path, triangleSub)
                                  }
                              | Level0KdTree.LazyKdTree lk -> // surfaces have absolute paths
                  
                                  let kdTreeSub   = lk.kdtreePath    |> surfaceRelativePath
                                  let triangleSub = lk.objectSetPath |> surfaceRelativePath                                                                                
                  
                                  LazyKdTree {
                                      lk with 
                                          kdtreePath    = Path.Combine(surf.importPath, kdTreeSub)
                                          objectSetPath = Path.Combine(surf.importPath, triangleSub)
                                  }
                              | Level0KdTree.InCoreKdTree ik -> InCoreKdTree ik // kdtrees can be loaded as is
                      )
                  (SgSurfaces.SgSurfaceOPC { s with kdTree = kd })
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
                    Log.error "can't make surface relative. not valid scene path has not been saved yet."
                    surfaceModel

    type SgHelper = {
        surf        : PRo3D.Surfaces.Surface
        bb          : Box3d
        sg          : ISg        
        kdtree      : hmap<Box3d,Level0KdTree>
    }

    let assertInvalidBB (bb:Box3d) = 
        if(bb.IsInvalid) then 
            failwith (sprintf "invalid bounding box %s" (bb.ToString()))
        else bb
   
    let combineLeafBBs (hierarchies : list<PatchHierarchy>) =
         hierarchies
            |> List.map(fun d -> d.tree |> QTree.getLeaves)
            |> Seq.map (fun p -> p |> Seq.map(fun d -> assertInvalidBB d.info.GlobalBoundingBox)) 
            |> Seq.concat
            |> Box3d.ofSeq

    let combineRootBBs (hierarchies : list<PatchHierarchy>) =
         hierarchies
            |> List.map(fun d -> d.tree |> QTree.getRoot)                  
            |> Seq.map (fun p -> assertInvalidBB p.info.GlobalBoundingBox)
            |> Box3d.ofSeq

    /// creates a surface from opc folder path
    let mk (stype:SurfaceType) path =                 

        let names = Files.getOPCNames path                
        {
            guid            = Guid.NewGuid()
            name            = IO.Path.GetFileName path
            importPath      = path
            opcNames        = names
            opcPaths        = names |> Files.expandNamesToPaths path
            fillMode        = FillMode.Fill
            cullMode        = CullMode.None
            isVisible       = true
            relativePaths   = false
            quality         = Init.quality
            priority        = Init.priority
            preTransform    = Trafo3d.Identity            
            scalarLayers    = PList.empty
            selectedScalar  = None
            textureLayers   = PList.empty
            selectedTexture = None            

            triangleSize = Init.triangleSize
            surfaceType = stype

        }       

    let sgOfSurface (s:PRo3D.Surfaces.Surface) sg (bb:Box3d) (kd:hmap<Box3d,Level0KdTree>) = 
    
        let pose = Pose.translate V3d.Zero // bb.Center
        let trafo = { TrafoController.initial with pose = pose; previewTrafo = Pose.toTrafo pose; mode = TrafoMode.Local }

        let sgSurfaceOpc = {
                surface    = s.guid
                sceneGraph = sg
                globalBB   = bb
                kdTree     = kd
                trafo      = trafo
            }
        (SgSurfaces.SgSurfaceOPC sgSurfaceOpc)

    let transformBox (trafo:Trafo3d) (bb:Box3d) =
        bb.Transformed(trafo)
              
    let createSgSurfaces runtime signature surfaces =        
        let sghs =
            surfaces
                |> PList.toList
                |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths; preTransform = d.preTransform })
                |> List.map (fun d -> Sg.createPlainSceneGraph runtime signature d true)
                |> List.zip (surfaces |> PList.toList)
                |> List.map(fun (surf, (sg, hierarchies, kdtree)) -> 
                    let bb = hierarchies |> combineLeafBBs |> transformBox surf.preTransform
                    { surf = surf; bb = bb; sg = sg; kdtree = kdtree })

        let sgSurfaces =
            sghs 
              |> List.map (fun d -> sgOfSurface d.surf d.sg d.bb d.kdtree)
              |> List.map (fun d -> (d.surface, d))
              |> HMap.ofList       

        sgSurfaces

    module ObjectFiles =

        open Aardvark.SceneGraph.IO

        let loadObject (s:PRo3D.Surfaces.Surface) : SgSurfaceObj =
            //Ag.initialize()
            let test = s.importPath
            let obj = Loader.Assimp.load (s.importPath)
            //let obj = Aardvark.SceneGraph.IO.Loader.Assimp.load @"C:\Users\laura\VRVis\Data\OBJ\WusonOBJ.obj"
            let meshes = 
                [
                    for mesh in obj.meshes do
                        yield mesh.geometry.Sg |> Sg.noEvents
                ] |> Sg.ofList

            let bb = obj.bounds
            let sg = 
                obj
                |> Sg.adapter
                |> Sg.requirePicking
                |> Sg.noEvents
                //|> Sg.trafo scaleTrafo
                // flip the z coordinates (since the model is upside down)
                //|> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
                |> Sg.scale 3.0

                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! DefaultSurfaces.normalMap
                    do! DefaultSurfaces.simpleLighting
                }
            let pose = Pose.translate V3d.Zero // bb.Center
            let trafo = { TrafoController.initial with pose = pose; previewTrafo = Pose.toTrafo pose; mode = TrafoMode.Local }
            {
                surface     = s.guid    
                trafo       = trafo
                globalBB    = bb
                sceneGraph  = sg
                pickMesh    = meshes
            }
          
       
        let createSgObjects runtime signature surfaces =        
            let sghs =
                surfaces
                    |> PList.toList
                    |> List.map (fun d -> (loadObject d))

            let sgObjects =
                sghs 
                  |> List.map (fun d -> (d.surface, SgSurfaces.SgSurfaceOBJ d))
                  |> HMap.ofList       

            sgObjects




       
                  
 module SurfaceApp =
    open System.Diagnostics
    open Aardvark.Rendering.GL.Compiler

    open PRo3DCompability    
    open Aardvark.VRVis
    open Aardvark.Geometry
    
    type Action =
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | FlyToSurface              of Guid
        | MakeRelative              of Guid
        | RemoveSurface             of Guid*list<Index>
        | PickSurface               of SceneHit*string
        | OpenFolder                of Guid
        | GroupsMessage             of Groups.Action  
        | PickObject                of V3d

    let triangleIsNan (t:Triangle3d) =
        t.P0.AnyNaN || t.P1.AnyNaN || t.P2.AnyNaN

    let getInvalidIndices (positions : V3d[]) =
        positions |> List.ofArray |> List.mapi (fun i x -> if x.AnyNaN then Some i else None) |> List.choose id

    let loadTriangles (kd : LazyKdTree) = 
        
        let positions = kd.objectSetPath |> Aara.fromFile<V3f>

        let data = 
            positions.Data |> Array.map (fun x ->  x.ToV3d() |> kd.affine.Forward.TransformPos)

        let invalidIndices = getInvalidIndices data
        let index = PRo3DCSharp.ComputeIndexArray(positions.Size.XY.ToV2i(), invalidIndices)                
                
        let triangles =             
            index 
                |> Seq.map(fun x -> data.[x])
                |> Seq.chunkBySize 3                               
                |> Seq.map(fun x -> Triangle3d(x))
                |> Seq.filter(fun x -> triangleIsNan x |> not) |> Seq.toArray
                |> TriangleSet

        triangles

    let loadObjectSet (cache : hmap<string, ConcreteKdIntersectionTree>) (lvl0Tree : Level0KdTree)=       
    
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
                                    //Log.line "cache hit %A" kd.boundingBox
                                    t, cache
                                | None ->                                     
                                    Log.line "cache miss %A- loading kdtree" kd.boundingBox

                                    let mutable tree = KdTrees.loadKdtree kd.kdtreePath                                    
                                    tree.KdIntersectionTree.ObjectSet <- (kd |> loadTriangles)

                                    let key = tree.KdIntersectionTree.BoundingBox3d.ToString()
                                                                        
                                    tree, (HMap.add key tree cache)
                kdTree, cache

    let intersectKdTrees (kdTreeMap: hmap<Box3d, Level0KdTree>) bb (surface : PRo3D.Surfaces.Surface) (cache : hmap<string, ConcreteKdIntersectionTree>) (ray : FastRay3d) = 

        let kdtree, c = kdTreeMap |> HMap.find bb |> loadObjectSet cache        

        let kdi = kdtree.KdIntersectionTree 
        let mutable hit = ObjectRayHit.MaxRange
                
        let objFilter a b = true
        try
           // let hitFilter a b c d = false
            if kdi.Intersect(ray, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then
                //let point = ray.Ray.GetPointOnRay(hit.RayHit.T)
                Some (hit.RayHit.T, surface),c
            else            
                None,c
        with 
          | _ -> 
            Report.Error "null ref exception in kdtree intersection" 
            None,c

    let doKdTreeIntersection (m : SurfaceModel) (t : Trafo3d) (r : FastRay3d) cache = 
        let mutable cache = cache
        let sgSurfaces = m.sgSurfaces 
                            |> HMap.filter( fun id sg -> sgSurface.isSgOPC sg )
                            |> HMap.map( fun id sg -> sgSurface.toSgOPC sg )
        sgSurfaces
            |> HMap.toSeq
            |> Seq.map (fun (_,d) -> d.kdTree, (m.surfaces.flat |> HMap.find d.surface) |> Leaf.toSurface)            
            |> Seq.sortByDescending(fun (_,surf) -> surf.priority.value)
            |> Seq.choose (
                fun (kd ,surf) ->
                    if kd.IsEmpty then 
                        Log.error "no kdtree loaded for %s" surf.name
                        None
                    else                        
                        //get bbs that are hit
                        let hitBoxes =
                            kd  |> HMap.toSeq 
                                |> Seq.map fst
                                |> Seq.filter(
                                    fun x -> 
                                       let mutable t = 0.0
                                       let r' = r.Ray.Transformed(surf.preTransform.Backward) //combine pre and current transform
                                       x.Intersects(r', &t)
                                )                                                
                        //intersect corresponding kdtrees
                        let closestHit =
                            hitBoxes
                                |> Seq.map(
                                    fun bb -> 
                                        //Log.line "intersection: %s; pr: %f" surf.name surf.priority.value                                        

                                        let hit, c = 
                                            r.Ray.Transformed(surf.preTransform.Backward) //combine pre and current transform
                                            |> FastRay3d 
                                            |> intersectKdTrees kd bb surf cache

                                        cache <- c
                                        hit
                                        )
                                |> Seq.choose(fun x -> x)
                                |> Seq.sortBy(fun (t,_)-> t)
                                |> Seq.tryHead                                               

                        closestHit
                )
            |> Seq.tryPick(fun d -> Some d), cache    

    let updateTrafo (trafo:SurfaceTrafo) (surfaces:hmap<string,PRo3D.Surfaces.Surface>) (model:SurfaceModel) =                                
      match surfaces.TryFind(trafo.id) with
        | Some s -> 
          let f = (fun _ -> { s with preTransform = trafo.trafo } |> Leaf.Surfaces)
          let g = Groups.updateLeaf s.guid f model.surfaces
          Log.line "MeasurementImporter: matched and updated %s" s.name
          { model with surfaces = g} 
        | None -> model
                 
    let updateTrafos (trafos:plist<SurfaceTrafo>) (model:SurfaceModel) =
      let surfaces = 
        model.surfaces.flat 
          |> HMap.toList 
          |> List.map(fun (_,v) -> 
             let surf = v |> Leaf.toSurface
             (surf.name, surf))
          |> HMap.ofList
      
      let rec update (p : list<SurfaceTrafo>) (model:SurfaceModel)  =
          match p with
            | x::rest -> 
              match rest with
                | [] -> updateTrafo x surfaces model
                | _ ->  update rest (updateTrafo x surfaces model) 
            | _ -> model
      update (trafos |> PList.toList) model

    let update (model:SurfaceModel) (action:Action) (scenePath : option<string>) = 
        match action with
            | SurfacePropertiesMessage msg ->                
                let m = 
                  match model.surfaces.singleSelectLeaf with
                    | Some s -> 
                      let surface = model.surfaces.flat |> HMap.find s |> Leaf.toSurface                        
                      let s' = SurfaceProperties.update surface msg
                      model |> SurfaceModel.updateSingleSurface s'                        
                    | None -> model
                match msg with
                  | SurfaceProperties.SetPriority _ -> m |> SurfaceModel.triggerSgGrouping 
                  | _ -> m            
            | MakeRelative id ->                
                let model = Surface.Files.makeSurfaceRelative id model scenePath
                let sgSurfaces = model.sgSurfaces |> Surface.Files.expandLazyKdTreePaths scenePath (model.surfaces.flat |> Leaf.toSurfaces)
                { model with sgSurfaces = sgSurfaces}
            | RemoveSurface (id,path) -> //model
                let groups = Groups.removeLeaf model.surfaces id path true
                let sg' = model.sgSurfaces |> HMap.remove id
                { model with surfaces = groups; sgSurfaces = sg'} |> SurfaceModel.triggerSgGrouping              
            | OpenFolder id ->
                let surf = id |> SurfaceModel.getSurface model
                match surf with
                    | Some s -> 
                        match s with 
                            | Leaf.Surfaces sf ->
                                let path = Surface.Files.getSurfaceFolder sf scenePath
                                match path with
                                    | Some p -> 
                                        Process.Start("explorer.exe", p) |> ignore
                                        model
                                    | None -> model      
                            | _ -> failwith "can only contain surfaces"
                    | None -> model
            | GroupsMessage msg -> 
                let groups = Groups.update model.surfaces msg

                match msg with
                  | Groups.Action.RemoveGroup _ | Groups.Action.RemoveChild _ ->
                    let sgs = 
                        model.sgSurfaces 
                            |> HMap.filter(fun k _ -> groups.flat |> HMap.containsKey k)

                    { model with surfaces = groups; sgSurfaces = sgs } |> SurfaceModel.triggerSgGrouping                  
                  | _ -> { model with surfaces = groups }
            | PickObject p -> model   
            | _ -> model

    let absRelIcons (m : MSurface)=
        adaptive {
            let! absRelIcons = 
                m.relativePaths 
                    |> Mod.map(fun x ->                                 
                        let icon = if x then "suitcase icon" else "cloud download icon"                                
                        AttributeMap.ofList [
                            clazz icon;                                                    
                            onClick (fun x -> MakeRelative (m.guid |> Mod.force))
                        ]         
                    )
            return absRelIcons
        }    

    let isSelected (model : MGroupsModel) (s : MSurface) =
        let id = s.guid |> Mod.force
            
        model.selectedLeaves
          |> ASet.map(fun x -> x.id = id)
          |> ASet.contains true
    
    let mkColor (model : MGroupsModel) (s : MSurface) =
        isSelected model s 
          |> Mod.bind (fun x -> if x then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White)

    let lastSelected (model : MGroupsModel) (s : MSurface) =
            let id = s.guid |> Mod.force
            model.singleSelectLeaf 
                |> Mod.map( fun x -> match x with 
                                        | Some xx -> xx = id
                                        | _ -> false )

    let isSingleSelect (model : MGroupsModel) (s : MSurface) =
            model.singleSelectLeaf |> Mod.map( fun x -> 
                match x with 
                  | Some selected -> selected = (s.guid |> Mod.force)
                  | None -> false )

    let viewSurfacesInGroups (path:list<Index>) (model:MGroupsModel) (singleSelect : MSurface*list<Index> -> 'outer) (multiSelect : MSurface*list<Index> -> 'outer) (lift : Groups.Action -> 'outer) (surfaceIds: alist<Guid>) : alist<DomNode<'outer>> = 
       alist {

         let surfaces = 
            surfaceIds 
               |> AList.chooseM (fun x -> model.flat |> AMap.tryFind x)

         let surfaces = 
            surfaces
               |> AList.map (fun x -> Mod.force x)
               |> AList.choose( fun x -> match x with
                                           | MSurfaces s -> Some s
                                           | _-> None )
         for s in surfaces do

             let singleSelect = fun _ -> singleSelect(s,path)
             let multiSelect  = fun _ -> multiSelect(s,path)
             let! selected = s |> isSingleSelect model

             let! c = mkColor model s
             let bgc = sprintf "color: %s" (Html.ofC4b c)
             let infoc = sprintf "color: %s" (Html.ofC4b C4b.White)
             let! key = s.guid
                                                                             
             let! absRelIcons = absRelIcons s

             let visibleIcon = 
                amap {
                  yield onMouseClick (fun _ -> lift <| Groups.ToggleChildVisibility (key,path))
                  let! visible = s.isVisible
                  if visible then 
                    yield clazz "unhide icon" 
                  else 
                    yield clazz "hide icon"
                } |> AttributeMap.ofAMap

             yield div [clazz "item"; style infoc] [
                 i [clazz "large cube middle aligned icon"; onClick multiSelect;style bgc][] 
                 div [clazz "content"; style infoc] [
                     let desc = s.name
                     yield Incremental.div (AttributeMap.ofList [style infoc])(
                         alist {
                             if selected then
                                 yield div[onClick singleSelect][ u[][Incremental.text desc]]
                             else
                                 yield div[onClick singleSelect][Incremental.text desc]
 
                             yield i [clazz "home icon"                                                
                                      onClick (fun _ -> FlyToSurface key) ][] |> UI.wrapToolTip "FlyTo"                                                     

                             yield i [clazz "folder icon"                                                
                                      onClick (fun _ -> OpenFolder key) ][] |> UI.wrapToolTip "open folder"

                             yield Incremental.i (absRelIcons) (AList.empty)
                             yield Incremental.i visibleIcon AList.empty |> UI.wrapToolTip "Toggle Visible"
                         } 
                     ) 
                                    
                 ]
             ]
       }
           
    let rec viewTree path (group : MNode) (model : MGroupsModel) : alist<DomNode<Action>> =

        alist {

            let! s = model.activeGroup
            let color = sprintf "color: %s" (Html.ofC4b C4b.White)                
            let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subNodes    

            let activeIcon =
                adaptive {                    
                    let! group  =  group.key
                    return if (s.id = group) then "circle icon" else "circle thin icon"
                }

            let setActive = Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)
            let activeAttributes = 
                Groups.mkGroupAttributeMap activeIcon (GroupsMessage setActive)
                                   
            let toggleIcon =
                adaptive {
                    let! toggle = group.visible
                    return if toggle then "unhide icon" else "hide icon"
                }

            let toggleAttributes = Groups.mkGroupAttributeMap toggleIcon (GroupsMessage(Groups.ToggleGroup path))
               
            let desc =
                div [style color] [       
                    Incremental.text group.name
                    Incremental.i activeAttributes AList.empty |> UI.wrapToolTip "Set active"
                        
                    i [clazz "plus icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.AddGroup path))] []
                    |> UI.wrapToolTip "Add Group"                                          

                    Incremental.i toggleAttributes AList.empty |> UI.wrapToolTip "Toggle Group"
                ]
                 
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                    let! selected = group.expanded
                    if selected 
                    then yield clazz "icon large outline open folder"
                    else yield clazz "icon large outline folder"
                    yield style "overflow-y : visible"
                } |> AttributeMap.ofAMap
            
            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = group.expanded
                    if isExpanded then yield style "visible"
                    else yield style "hidden"
                }         

            let singleSelect = 
                    fun (s:MSurface,path:list<Index>) -> 
                        Action.GroupsMessage(Groups.SingleSelectLeaf (path, s.guid |> Mod.force, ""))

            let multiSelect = 
                fun (s:MSurface,path:list<Index>) -> 
                    Action.GroupsMessage(Groups.AddLeafToSelection (path, s.guid |> Mod.force, ""))

            let lift = fun (a:Groups.Action) -> (GroupsMessage a)

            yield div [ clazz "item"] [
                    Incremental.i itemAttributes AList.empty
                    div [ clazz "content" ] [                         
                        div [ clazz "description noselect"] [desc]
                        Incremental.div (AttributeMap.ofAMap childrenAttribs) (                          
                            alist { 
                                let! isExpanded = group.expanded
                                if isExpanded then yield! children
                                
                                if isExpanded then 
                                    yield! viewSurfacesInGroups path model singleSelect multiSelect lift group.leaves
                            }
                        )  
                                
                    ]
            ]
                
        }


    let viewSurfacesGroups (model:MSurfaceModel) = 
        require Html.semui (
            TreeView.view [] (viewTree [] model.surfaces.rootGroup model.surfaces)
        )    
        

    //let viewSurfaceProperties (model:MSurfaceModel) =
    //    adaptive {
    //        let! bla = model.surfaces.lastSelectedChild
    //        let opt = 
    //            match bla with
    //                | Some s -> SurfaceProperties.view s
    //                | None   -> div[][]

    //        return (opt |> UI.map SurfacePropertiesMessage)
    //    }    

    let lscToDom lsc isSome = 
        adaptive {
            match lsc with
                | Some b ->   
                    let! b' = b
                    match b' with 
                        | MSurfaces bm -> return isSome bm
                        | _ -> return div[][]                                                             
                | None   -> return div[][] 
        }

    let viewSurfaceProperties (model:MSurfaceModel) =
        adaptive {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let x = match surf with | MSurfaces s -> s | _ -> failwith "wrong type"
                                       
                    return SurfaceProperties.view x |> UI.map SurfacePropertiesMessage
                | None -> return div[][] |> UI.map SurfacePropertiesMessage 
        }                          
        
    let surfaceGroupProperties (model:MSurfaceModel) =
        adaptive {                                
            return (Groups.viewUI model.surfaces|> UI.map GroupsMessage)
        } 
    
    let surfacesLeafButtonns (model:MSurfaceModel) = 
        let ts = model.surfaces.activeChild
        let sel = model.surfaces.singleSelectLeaf
        adaptive {  
            let! ts = ts
            let! sel = sel
            match sel with
                | Some x -> return (Groups.viewLeafButtons ts |> UI.map GroupsMessage)
                | None -> return div[][]
        } 

    let surfacesGroupButtons (model:MSurfaceModel) = 
        let ts = model.surfaces.activeGroup
        adaptive {  
            let! ts = ts
            return (Groups.viewGroupButtons ts |> UI.map GroupsMessage)
        } 
    
    let surfaceUI (model:MSurfaceModel) =
        let item2 = 
            model.surfaces.lastSelectedItem 
                |> Mod.map (fun x -> 
                    match x with 
                        | SelectedItem.Group -> surfaceGroupProperties model
                        | _ -> viewSurfaceProperties model
                )
        let buttons = 
            model.surfaces.lastSelectedItem 
                |> Mod.bind (fun x -> 
                    match x with 
                        | SelectedItem.Group -> surfacesGroupButtons model
                        | _ -> surfacesLeafButtonns model
                )
        div[][                            
            yield GuiEx.accordion "Surfaces" "Cubes" true [ 
                //Groups.viewSelectionButtons |> UI.map GroupsMessage
                viewSurfacesGroups model ]
            yield GuiEx.accordion "Properties" "Content" true [
                        Incremental.div AttributeMap.empty (alist{ let! item = item2
                                                                   let! i = item
                                                                   yield i})
            ]
            yield GuiEx.accordion "Actions" "Asterisk" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
                ]     
        ]
    
    