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

module DebugKdTrees = 
  open Aardvark.VRVis.Opc.KdTrees
  open MBrace.FsPickler

  let loadKdTrees' (h : PatchHierarchy) (trafo:Trafo3d) (load : bool) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
                    
        //ObjectBuilder

        let masterKdPath = 
          mode |> ViewerModality.matchy
            (h.kdTreeAggZero_FileAbsPath) 
            (h.kdTreeAggZero2d_FileAbsPath)

        let cacheFile = System.IO.Path.ChangeExtension(masterKdPath, ".cache")

        if System.IO.File.Exists(cacheFile) then
            Log.line "Found lazy kdtree cache"
            if load then
              loadAs<list<Box3d*Level0KdTree>> cacheFile b |> HMap.ofList
            else
              HMap.empty
        else
            
            Log.line "did not find lazy kdtree cache"
            let patchInfos =
                h.tree |> QTree.getLeaves |> Seq.toList |> List.map(fun x -> x.info)

            let kd0Paths = 
                patchInfos 
                  |> List.map(fun x -> h.kdTree_FileAbsPath x.Name 0 mode) |> List.sort 

            let kd0PathsExist = 
                kd0Paths |> List.filter(System.IO.File.Exists) |> List.length > 0
        
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
                     Log.line "Found master kdtree and patch trees"
                     Log.startTimed "building lazy kdtree cache"
                             
                     let num = kd0Paths |> List.ofSeq |> List.length                     

                     let bla = 
                        kd0Paths
                            |> List.zip patchInfos
                            |> List.mapi(
                                fun i (info, kdPath) ->                                
                                    let t = loadKdtree kdPath
                                    let pos =
                                      match mode with
                                      | XYZ -> info.Positions
                                      | SvBR -> info.Positions2d.Value

                                    let lazyTree : LazyKdTree = {
                                        kdTree        = None
                                        objectSetPath = h.opcPaths.Patches_DirAbsPath +/ info.Name +/ pos
                                        kdtreePath    = kdPath
                                        affine        = 
                                          mode 
                                            |> ViewerModality.matchy info.Local2Global info.Local2Global2d
                                        boundingBox   = t.KdIntersectionTree.BoundingBox3d.Transformed(trafo)
                                    }
                                    Report.Progress(float i / float num)

                                    (lazyTree.boundingBox, (LazyKdTree lazyTree))
                            )                            
                      
                     Log.stop()
                      
                     bla |> save cacheFile b |> ignore
                        
                     if load then
                       bla |> HMap.ofList
                     else
                       HMap.empty
                | _ ->
                    Log.warn "Could not find level 0 kdtrees"
                    HMap.empty
    
  let loadKdTrees (h : PatchHierarchy) (trafo:Trafo3d) (mode:ViewerModality) (b : BinarySerializer) : hmap<Box3d,Level0KdTree> =
      loadKdTrees' (h) (trafo) (true) mode b


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =
    open Aardvark.SceneGraph.SgFSharp.Sg
                      
    module Files = 
       open System.IO
       open Aardvark.Prinziple
       
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
       let isSurfaceFolder (path : string) =
           Directory.GetDirectories(path) |> Seq.forall isOpcFolder

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

       let expandLazyKdTreePaths (scenePath : option<string>) (surfaces : hmap<Guid, PRo3D.Surfaces.Surface>) (sgSurfaces : hmap<Guid, SgSurfaces>) =
          let sgS = sgSurfaces |> HMap.filter (fun id sg -> (sgSurface.isSgOPC sg))
          sgS 
             |> HMap.map (fun _ s -> 
                 let s = (sgSurface.toSgOPC s)
                 let surf = surfaces |> HMap.find s.surface
                 let kd =
                     s.kdTree |> HMap.map (fun _ k -> 
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

    module Sg =
      open Aardvark.Prinziple
      open Aardvark.VRVis.Opc      

      type SgHelper = {
        surf        : PRo3D.Surfaces.Surface
        bb          : Box3d
        sg          : ISg        
        kdtree      : hmap<Box3d,KdTrees.Level0KdTree>
      }

      let mutable hackRunner = None

      let createPlainSceneGraph (runtime : IRuntime) (signature : IFramebufferSignature) (scene : OpcScene) (createKdTrees)
          : (ISg * list<PatchHierarchy> * hmap<Box3d, KdTrees.Level0KdTree>) =

          let runner = 
              match hackRunner with
                  | None -> 
                      printfn "create runner"
                      let  r = runtime.CreateLoadRunner 2
                      hackRunner <- Some (r)
                      r
                  | Some h -> h

          let preTransform = scene.preTransform
      
          let patchHierarchies = 
            scene.patchHierarchies 
              |> Seq.map Prinziple.registerIfZipped 
              |> Seq.map (fun x -> PatchHierarchy.load Serialization.binarySerializer.Pickle Serialization.binarySerializer.UnPickle (OpcPaths x))
              |> Seq.toList
                              
          let kdTreesPerHierarchy =
              [| for h in patchHierarchies do
                  if createKdTrees then   
                      let kdMap = DebugKdTrees.loadKdTrees h preTransform ViewerModality.XYZ Serialization.binarySerializer

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
          
          // create level of detail hierarchy (Sg)
          //Sg.patchLod signature runner patchHierarchy.opcPaths.Opc_DirAbsPath
          let g = 
            patchHierarchies 
            |> List.map (fun h ->                     
              let sg = 
                Sg.patchLod signature runner h.opcPaths.Opc_DirAbsPath
                  scene.lodDecider scene.useCompressedTextures
                  ViewerModality.XYZ
                  (PatchLod.toRoseTree h.tree)
              sg) 
            |> Sg.ofList
          

          let lodVisEnabled = Mod.init false
          
          let sg = g |> Sg.transform preTransform
                                  
          sg, patchHierarchies, kdTrees
    
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
              isActive        = true
              relativePaths   = false
              quality         = Init.quality
              priority        = Init.priority
              scaling         = Init.scaling
              preTransform    = Trafo3d.Identity            
              scalarLayers    = hmap.Empty //PList.empty
              selectedScalar  = None
              textureLayers   = PList.empty
              selectedTexture = None            
      
              triangleSize = Init.triangleSize
              surfaceType = stype
      
              colorCorrection = Init.initColorCorrection
          }       
      
      let sgOfSurface (s:PRo3D.Surfaces.Surface) sg (bb:Box3d) (kd:hmap<Box3d,KdTrees.Level0KdTree>) = 
      
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
      
      let transformBox (trafo:Trafo3d) (bb:Box3d) = bb.Transformed(trafo)
                
      let createSgSurfaces runtime signature (surfaces:plist<PRo3D.Surfaces.Surface>) =
        
        let sghs =
          surfaces
            |> PList.toList
            |> List.map (fun d -> 
             { 
               Configurations.Empty.mars() with 
                 patchHierarchies = d.opcPaths
                 preTransform = d.preTransform
                 useCompressedTextures = false })
            |> List.map (fun d -> createPlainSceneGraph runtime signature d true)
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

        let loadObject (s:PRo3D.Surfaces.Surface) : SgSurfaceObj =                        
            let obj = Loader.Assimp.load (s.importPath)            
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
                |> Sg.scale 1.0

            let pose = Pose.translate bb.Center // bb.Center V3d.Zero
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

    module SurfaceAttributes = 
        open System.Xml
            
        let get (name : string) (node : XmlNode)=
            node.SelectSingleNode(name).InnerText.Trim()

        let parseMap (index : int)(node : XmlNode) : ScalarLayer = 
            let definedRange = node |> get "ChannelsDefinedRange" |> Range1d.Parse
            { 
                label        = node |> get "Label" 
                actualRange  = node |> get "ChannelsActualRange"  |> Range1d.Parse
                definedRange = definedRange //node |> get "ChannelsDefinedRange" |> Range1d.Parse
                index        = index
                colorLegend  = (FalseColorsModel.initDefinedScalarsLegend definedRange)
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
                   
                         
 module SurfaceApp =
    open System.Diagnostics
    open Aardvark.Rendering.GL.Compiler

    open PRo3DCompability    
    open Aardvark.VRVis
    open Aardvark.Geometry
    open Surface
    
    type Action =
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | FlyToSurface              of Guid
        | MakeRelative              of Guid
        | RemoveSurface             of Guid*list<Index>
        | PickSurface               of SceneHit*string
        | OpenFolder                of Guid
        | RebuildKdTrees            of Guid
        | ToggleActiveFlag          of Guid
        | GroupsMessage             of Groups.Action  
        | PickObject                of V3d
        | PlaceSurface              of V3d
        | ScalarsColorLegendMessage of FalseColorLegendApp.Action
        | ColorCorrectionMessage    of ColorCorrectionProperties.Action

    let computeIndices (size:V2i)(invalids : int[]) : int[] =
      Aardvark.VRVis.IndexComputation.ComputeIndexArray(size,invalids)

    let intersectKdTrees' = KdTrees.intersectKdTrees computeIndices

    let doKdTreeIntersection (m : SurfaceModel) (r : FastRay3d) cache = 
        let mutable cache = cache
        let activeSgSurfaces = 
          m.surfaces.flat
            |> HMap.filter(fun _ s -> s.active)
            |> HMap.toList 
            |> List.map fst
            |> List.choose(fun id -> m.sgSurfaces |> HMap.tryFind id)

        let sgSurfaces = 
          activeSgSurfaces 
            |> List.filter( fun sg -> sgSurface.isSgOPC sg )
            |> List.map( fun sg -> sgSurface.toSgOPC sg )

        
        sgSurfaces          
          |> List.map (fun d -> d.kdTree, (m.surfaces.flat |> HMap.find d.surface) |> Leaf.toSurface)            
          |> List.sortByDescending(fun (_,surf) -> surf.priority.value)
          |> List.choose (
             fun (kd ,surf) ->
               if kd.IsEmpty then Log.error "no kdtree loaded for %s" surf.name; None
               else                        
                 //get bbs that are hit
                 let hitBoxes =
                   kd  
                     |> HMap.toList |> List.map fst
                     |> List.filter(
                         fun x -> 
                            let mutable t = 0.0
                            let r' = r.Ray.Transformed(surf.preTransform.Backward) //combine pre and current transform
                            x.Intersects(r', &t)
                     )
                 //intersect corresponding kdtrees
                 let closestHit =
                   hitBoxes
                     |> List.choose(
                         fun bb -> 
                             //Log.line "intersection: %s; pr: %f" surf.name surf.priority.value                                                             
                             let ray = r.Ray.Transformed(surf.preTransform.Backward) |> FastRay3d  //combine pre and current transform                     
                             let hit, c = 
                               kd |> intersectKdTrees' bb surf cache ray                     
                             cache <- c
                             hit
                             )                                
                     |> List.sortBy(fun (t,_)-> t)
                     |> List.tryHead                                               
             
                 closestHit
             )
          |> List.sortBy(fun (t,_) -> t)
          |> List.tryPick(fun d -> Some d), cache

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

    //let union (l : SurfaceModel) (r : SurfaceModel) : SurfaceModel =
    //  let l' = { l with 

    //  l

    let hmapsingle (k,v) = HMap.single k v

    let updateSurfaceTrafos (trafos: list<SurfaceTrafo>) (model:SurfaceModel) =
      let surfaces =        
        model.surfaces.flat 
          |> HMap.toList
          |> List.map(fun (_,v) -> 
            let s = (v |> Leaf.toSurface)
            s.name, s)
          |> HMap.ofList
      
      let surfaces' = 
        trafos 
          |> List.filter (fun x -> x.id |> surfaces.ContainsKey)
          |> List.map(fun x -> surfaces.Find x.id,x)
          |> List.map(fun (a,b) -> { a with preTransform = b.trafo })
            
      let flat' = 
        surfaces' 
          |> List.choose(fun x -> 
            let f = (fun _ -> x |> Leaf.Surfaces)
            Groups.updateLeaf' x.guid f model.surfaces 
              |> HMap.tryFind x.guid 
              |> Option.map(fun leaf -> (x.guid,leaf) |> hmapsingle))          
          |> List.fold HMap.union HMap.empty
                    
      //write union of surface models ... union flat and recalculate all others
      //this does only update surfaces ... not kdtrees or sgs !!!!             
      { model with surfaces = { model.surfaces with flat = flat' } }

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
        | RebuildKdTrees id ->                
            let surf = id |> SurfaceModel.getSurface model
            match surf with
              | Some (Leaf.Surfaces sf) ->                    
                let path = Surface.Files.getSurfaceFolder sf scenePath
                match path with
                   | Some p ->                            
                       let path = @".\20170530_TextureConverter_single\Vrvis.TextureConverter.exe"
                       if File.Exists(path) then //\Vrvis.TextureConverter.exe") then
                         let mutable converter = new Process()                             
                         Log.line "start processing"                        
                         converter.StartInfo.FileName <- "Vrvis.TextureConverter.exe"
                         converter.StartInfo.Arguments <- p + " -overwrite" //-lazy"
                         converter.StartInfo.UseShellExecute <- true                          
                         converter.StartInfo.WorkingDirectory <- @".\20170530_TextureConverter_single"                                    
                         converter.Start() |> ignore    
                         converter.WaitForExit()
                            //)
                       else
                         Log.line "texture converter not found"                             
                       model
                   | None -> model                         
              | _ -> model
        | ToggleActiveFlag id ->               
          let surf = id |> SurfaceModel.getSurface model
          match surf with
            | Some (Leaf.Surfaces s) ->
              { s with isActive = not s.isActive } |> SurfaceModel.updateSingleSurface' model
            | _ -> model
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
        | PlaceSurface p -> 
            match model.surfaces.singleSelectLeaf with
            | Some id -> 
              match model.surfaces.flat.TryFind(id) with 
              | Some s -> 
                 let trans = Trafo3d.Translation(p)
                 let s = s |> Leaf.toSurface
                 let f = (fun _ -> { s with preTransform = trans  } |> Leaf.Surfaces)
                 let g = Groups.updateLeaf s.guid f model.surfaces
                 
                 let sgs' = 
                     model.sgSurfaces
                     |> HMap.update id (fun x -> 
                       match x with 
                       | Some sg ->    //let pose = Pose.translate p // bb.Center
                         let pose = { sg.trafo.pose with position = sg.trafo.pose.position + p }

                         let trafo' = { 
                           TrafoController.initial with 
                             pose = pose
                             previewTrafo = trans
                             mode = TrafoMode.Local 
                         }
                         
                         if sgSurface.isSgOPC sg then
                           let sg' = sgSurface.toSgOPC sg 
                           let t = { sg' with trafo = trafo'; globalBB = (sg'.globalBB.Transformed trafo'.previewTrafo) } 
                           (SgSurfaces.SgSurfaceOPC t)
                         else
                           let sg' = sgSurface.toSgOBJ sg 
                           let t = { sg' with trafo = trafo'; globalBB = (sg'.globalBB.Transformed trafo'.previewTrafo) } 
                           (SgSurfaces.SgSurfaceOBJ t)
                       | None   -> failwith "surface not found")                             
                 { model with surfaces = g; sgSurfaces = sgs'} 
              | None -> model
            | None -> model
        | ScalarsColorLegendMessage msg ->
            match model.surfaces.singleSelectLeaf with
              | Some s -> 
                let surface = model.surfaces.flat |> HMap.find s |> Leaf.toSurface 
                match surface.selectedScalar with
                  | Some s -> 
                    let sc = { s with colorLegend = (FalseColorLegendApp.update s.colorLegend msg) }                        
                    let scs = surface.scalarLayers |> HMap.alter sc.index (Option.map(fun _ -> sc))
                    let s' = { surface with selectedScalar = Some sc; scalarLayers = scs }
                    model |> SurfaceModel.updateSingleSurface s'   
                  | None -> model
              | None -> model
        | ColorCorrectionMessage msg ->       
            let m = 
              match model.surfaces.singleSelectLeaf with
                | Some s -> 
                  let surface = model.surfaces.flat |> HMap.find s |> Leaf.toSurface
                  let s' = { surface with colorCorrection = (ColorCorrectionProperties.update surface.colorCorrection msg) }
                  model |> SurfaceModel.updateSingleSurface s'                        
                | None -> model
            m
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
            
             let headerAttributes =
                amap {
                  yield onClick singleSelect
                  let! selected = isSingleSelect model s
                  yield if selected then style "text-transform:uppercase" else style "text-transform:none"
                } |> AttributeMap.ofAMap

             let headerColor = sprintf "color: %s" (Html.ofC4b C4b.Gray)

             yield div [clazz "item"; style infoc] [
                 i [clazz "cube middle aligned icon"; onClick multiSelect;style bgc][] 
                 div [clazz "content"; style infoc] [                     
                     yield Incremental.div (AttributeMap.ofList [style infoc])(
                         alist {
                             yield div[clazz "header"; style headerColor][
                                Incremental.span headerAttributes ([Incremental.text s.name] |> AList.ofList)
                             ]
                             //if selected then
                             //    yield div[onClick singleSelect][ u[][Incremental.text desc]]
                             //else
                             //    yield div[onClick singleSelect][Incremental.text desc]
 
                             yield i [clazz "home icon"                                                
                                      onClick (fun _ -> FlyToSurface key) ][] |> UI.wrapToolTip "Fly to surface"                                                     

                             yield i [clazz "folder icon"                                                
                                      onClick (fun _ -> OpenFolder key) ][] |> UI.wrapToolTip "Open Folder"

                             yield i [
                               clazz "microchip icon"
                               onClick (fun _ -> RebuildKdTrees key)
                             ][] |> UI.wrapToolTip "process kdtrees"

                             yield Incremental.i (absRelIcons) (AList.empty)
                             yield Incremental.i visibleIcon AList.empty |> UI.wrapToolTip "Toggle Visible"
                             yield GuiEx.iconCheckBox s.isActive (ToggleActiveFlag key) |> UI.wrapToolTip "Toggle IsActive"
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
                                   
            let toggleIcon = group.visible |> Mod.map(fun toggle -> if toggle then "unhide icon" else "hide icon")                

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
                   // GuiEx.iconCheckBox group.visible (GroupsMessage(Groups.ToggleGroup path))
                ]
                 
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                    let! selected = group.expanded
                    if selected 
                    then yield clazz "icon outline open folder"
                    else yield clazz "icon outline folder"
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
        require GuiEx.semui (
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

   // let noSurfaceSelected = div[ style "font-style:italic"][ text "no surface selected" ]

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
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map SurfacePropertiesMessage 
        }                          
        
    let surfaceGroupProperties (model:MSurfaceModel) =
        adaptive {                                
            return (Groups.viewUI model.surfaces|> UI.map GroupsMessage)
        } 

    let viewColorCorrectionTools (model:MSurfaceModel) =
        adaptive {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let colorCorrection = match surf with | MSurfaces s -> s.colorCorrection | _ -> failwith "wrong type"
                    return ColorCorrectionProperties.view colorCorrection |> UI.map ColorCorrectionMessage
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map ColorCorrectionMessage 
        }                          

    let viewColorLegendTools (model:MSurfaceModel) =
        adaptive {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let scalar = 
                      match surf with 
                       | MSurfaces s -> s.selectedScalar 
                       | _ -> failwith "wrong type"

                    let! scalar = scalar
                    match scalar with
                        | Some s -> return FalseColorLegendApp.UI.viewDefinedScalarsLegendTools s.colorLegend |> UI.map ScalarsColorLegendMessage
                        | None -> return div[ style "font-style:italic"][ text "no scalar in properties selected" ] |> UI.map ScalarsColorLegendMessage 
                                  
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map ScalarsColorLegendMessage 
        }                          

    let showColorLegend (model:MSurfaceModel) =
        alist {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let scalar = match surf with | MSurfaces s -> s.selectedScalar | _ -> failwith "wrong type"
                    let! scalar = scalar
                    match scalar with
                        | Some s -> yield FalseColorLegendApp.Draw.createColorLegendScalars s.colorLegend
                        | None -> yield div[ style "font-style:italic"][ text "no scalar selected" ]
                                  
                | None -> yield div[ style "font-style:italic"][ text "no surface selected" ]
        }  


    let surfacesLeafButtonns (model:MSurfaceModel) = 
        let ts = model.surfaces.activeChild
        let sel = model.surfaces.singleSelectLeaf
        adaptive {  
            let! ts = ts
            let! sel = sel
            match sel with
                | Some x -> return (Groups.viewLeafButtons ts |> UI.map GroupsMessage)
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map GroupsMessage
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
                |> Mod.bind (fun x -> 
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
            yield GuiEx.accordion "Surfaces" "Cubes" true [ viewSurfacesGroups model ]
            yield GuiEx.accordion "Properties" "Content" false [
              Incremental.div AttributeMap.empty (
                alist { 
                  let! item = item2
                  yield item}
              )
            ]
            yield GuiEx.accordion "Actions" "Asterisk" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
                ]  
                
            yield GuiEx.accordion "Color Correction" "file image outline" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(viewColorCorrectionTools model))] 

            yield GuiEx.accordion "Scalars ColorLegend" "paint brush" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(viewColorLegendTools model))] 

            
        ]
    
    