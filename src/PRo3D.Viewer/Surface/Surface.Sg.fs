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
                    let kdMap = KdTrees.loadKdTrees h Trafo3d.Identity ViewerModality.XYZ Serialization.binarySerializer

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
    let mk (stype:SurfaceType) (maxTriangleSize : float) path =                 
    
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
    
            triangleSize = { Init.triangleSize with value = maxTriangleSize }
            surfaceType = stype
    
            colorCorrection = Init.initColorCorrection
            homePosition = None
            transformation = Init.Transformations
        }       
    
    let sgOfSurface (s:PRo3D.Surfaces.Surface) sg (bb:Box3d) (kd:hmap<Box3d,KdTrees.Level0KdTree>) = 
    
        let pose = Pose.translate V3d.Zero // bb.Center
        let trafo = { TrafoController.initial with pose = pose; previewTrafo = Pose.toTrafo pose; mode = TrafoMode.Local }
    
        let sgSurface = {
                surface    = s.guid
                sceneGraph = sg
                globalBB   = bb
                picking    = Picking.KdTree kd
                trafo      = trafo
                //transformation = Init.Transformations
            }
        sgSurface
    
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
    
    let viewHomePosition (model:MSurfaceModel) =
        let point = 
            aset{
                let! guid = model.surfaces.singleSelectLeaf
            
                match guid with
                    | Some i -> 
                        let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                        let! surf = leaf 
                        let x = match surf with | MSurfaces s -> s | _ -> failwith "wrong type"
                        let! hpos = x.homePosition
                        match hpos with
                            | Some p -> yield PRo3D.Sg.dots (AList.single p.Location) (Mod.constant 3.0) (Mod.constant C4b.Yellow)
                            | None -> yield Aardvark.UI.Extensions.Sg.empty
                    | None -> yield Aardvark.UI.Extensions.Sg.empty
            }|> Aardvark.UI.``F# Sg``.Sg.set
        Aardvark.UI.``F# Sg``.Sg.ofList [point]
        
            
                                  

