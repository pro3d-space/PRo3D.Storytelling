namespace Aardvark.Opc

open System
open System.Linq
open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.Base.Geometry
open Aardvark.Base.Coder

open Aardvark.Application
open Aardvark.Application.WinForms

[<AutoOpen>]
module Serialization =
    open MBrace.FsPickler
    open System.IO
    
    let binarySerializer = FsPickler.CreateBinarySerializer()

    let save path (d : 'a) =    
        let arr = binarySerializer.Pickle d     
        File.WriteAllBytes(path, arr);
        d

    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module SurfaceModel =
//    open MBrace.FsPickler
//    open MBrace.FsPickler.Combinators

//    let makeSurfaceModel a b =
//        {
//            surfaces = a
//            selectedSurface = b
//            sgSurfaces = hmap.Empty
//        }

//    let surfaceModelPickler : Pickler<SurfaceModel> =
//        Pickler.product makeSurfaceModel
//        ^+ Pickler.field (fun s -> s.surfaces)          Pickler.auto<list<Surface>>
//        ^. Pickler.field (fun s -> s.selectedSurface)   Pickler.auto<option<Surface>>

//    let initial = {
//        surfaces        = []
//        selectedSurface = None
//        sgSurfaces      = hmap.Empty
//    }

//    FsPickler.RegisterPicklerFactory (fun _ -> surfaceModelPickler)

[<AutoOpen>]
module KdTrees =
    open MBrace.FsPickler
    open MBrace.FsPickler.Combinators
    open Aardvark.VRVis

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
       
    FsPickler.RegisterPicklerFactory (fun _ -> level0KdTreePickler )

    let loadKdtree p =
        //Log.startTimed "loading tree"
        use b = new BinaryReadingCoder(System.IO.File.OpenRead(p))
        let mutable treeOida = Unchecked.defaultof<KdIntersectionTree>
        b.CodeT(&treeOida)
        //Log.stop()
        //  treeOida
        ConcreteKdIntersectionTree(treeOida, Trafo3d.Identity)
    

module Viewer = 
    open Aardvark.Base.Rendering.Effects
    open FShade
    open DomainModel

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
            [ for h in scene.patchHierarchies do
                let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
                yield PatchHierarchy.load p
            ] 
        
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
                (Serialization.loadAs<array<Box3d*Level0KdTree>> cacheFile) |> HMap.ofArray
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
                    let sg = Sg.patchLod signature runner h.baseDir 
                                    scene.lodDecider scene.useCompressedTextures                                 
                                    (PatchLod.toRoseTree h.tree)
                                    
                    sg
               ) 
            |> Sg.ofList
        

        let lodVisEnabled = Mod.init false
        
        let sg = g |> Sg.transform preTransform
                                
        sg, patchHierarchies, kdTrees

    let createSceneGraph (runtime : IRuntime) (sizes : IMod<V2i>) (mouse : IMouse) (keyboard : IKeyboard) (signature : IFramebufferSignature) (time : IMod<_>) (scene : OpcScene)  = 
    
        let runner = runtime.CreateLoadRunner 2

        let bb = scene.boundingBox
        let speed = Mod.init scene.speed

        let preTransform = Trafo3d.Translation(-bb.Center) * scene.preTransform

        let initialView = CameraView.lookAt (bb.Max - bb.Center)  V3d.Zero bb.Center.Normalized
        let view = initialView |> DefaultCameraController.controlWithSpeed speed mouse keyboard time
       
        let proj = sizes |> Mod.map (fun s -> Frustum.perspective 60.0 scene.near scene.far (float s.X / float s.Y))

        let fillMode = Mod.init FillMode.Fill
        let lodVisEnabled = Mod.init false
            
        let patchHierarchies =
            [ for h in scene.patchHierarchies do
                let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
                yield PatchHierarchy.load p
            ] 

   

        let g = 
            patchHierarchies 
            |> List.map (fun h -> 
                    Sg.patchLod signature runner h.baseDir 
                                scene.lodDecider scene.useCompressedTextures 
                                (PatchLod.toRoseTree h.tree)
                                
               ) 
            |> Sg.ofList

        keyboard.KeyDown(Keys.X).Values.Subscribe(fun _ -> 
            transact (fun () -> 
                match fillMode.Value with
                    | FillMode.Fill -> fillMode.Value <- FillMode.Line
                    | _ -> fillMode.Value <- FillMode.Fill
            )
        ) |> ignore

        keyboard.KeyDown(Keys.L).Values.Subscribe(fun _ -> 
            transact (fun () -> 
                lodVisEnabled.Value <- lodVisEnabled.Value |> not
            )
        ) |> ignore

        keyboard.KeyDown(Keys.G).Values.Subscribe(fun _ -> 
            for i in 0 .. 10 do
                System.GC.Collect()
            System.GC.WaitForFullGCComplete() |> ignore

            printf "fixme"
            //printfn "%A" ( ctrl.Runtime.Context.MemoryUsage ) 
        ) |> ignore

        keyboard.KeyDown(Keys.PageDown).Values.Subscribe(fun _ -> 
            transact (fun () -> 
                speed.Value <- speed.Value / 2.0
                printfn "speed: %A" speed.Value
            )
        ) |> ignore

        keyboard.KeyDown(Keys.PageUp).Values.Subscribe(fun _ -> 
            transact (fun () -> 
                speed.Value <- speed.Value * 2.0
                printfn "speed: %A" speed.Value
            )
        ) |> ignore
    
        let box = 
            Sg.wireBox' C4b.White  ( Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") )
             |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
             ]
        let sg =        
            g   
                //|> Sg.normalize
                |> Sg.andAlso (box)
                |> Sg.transform preTransform
                |> Sg.effect [
                            DefaultSurfaces.trafo |> toEffect
                            DefaultSurfaces.constantColor C4f.White |> toEffect
                            DefaultSurfaces.diffuseTexture |> toEffect
                            Shader.LoDColor |> toEffect
                            //DefaultSurfaces.simpleLighting |> toEffect
                        ]
                |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
                |> Sg.uniform "LodVisEnabled" lodVisEnabled
                |> Sg.fillMode fillMode
                |> Sg.cullMode (Mod.constant CullMode.None)
                |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
                |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)                
        sg

    let run (scene : OpcScene) =

        Aardvark.Base.Ag.initialize()
        Aardvark.Init()

        use app = new OpenGlApplication()
        let win = app.CreateSimpleRenderWindow()
        win.Text <- "Aardvark rocks OPCs \\o/"

        let sg = createSceneGraph win.Runtime win.Sizes win.Mouse win.Keyboard win.FramebufferSignature win.Time scene
    
        let cfg =  { BackendConfiguration.Default  with useDebugOutput = false }
        let task =
            RenderTask.ofList [
                app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.Gray)
                app.Runtime.CompileRender(win.FramebufferSignature, cfg, sg)  |> DefaultOverlays.withStatistics
            ]

        win.RenderTask <- task
        win.Run()
        0


    open Configurations.Network


    [<EntryPoint>]
    let main argv =
        //run ( Configurations.Network.testSurf () )
        match argv.ToLower() with
            | [| "perjen" |] -> run ( perjen () )
            | [| "mola" |] -> run ( mola () )
            | [| "mars" |] -> run ( mars () )
            | _ -> 
                printfn "wrong arguments: %A, synopsis: Aardvark.OpenViewer [perjen|mola|mars]" argv
                System.Environment.Exit 1 
                0


//    System.Environment.Exit 0

//    let sg = 
//        match hier with
//            | dir,Node(patch,_) ->
//                Patch.load dir patch.info
//                    |> Sg.ofIndexedGeometry
//                    //|> Sg.trafo (Mod.constant patch.info.Local2Global)
//                    |> Sg.effect [
//                        DefaultSurfaces.trafo |> toEffect
//                        DefaultSurfaces.constantColor C4f.White |> toEffect
//                        DefaultSurfaces.simpleLighting |> toEffect
//                    ]
//            | _ ->
//                failwith ""


//    let rec collect current  =
//        seq {
//            match current with
//                | Node(n, xs) ->
//                   for x in xs do yield! collect x
//                | Leaf(patch) ->
//                   let tex = patch.info.Textures |> List.head 
//                   let fullPath = Path.combine [patch.info.BaseDir; tex.fileName]
//                   let fullPath = System.IO.Path.ChangeExtension(fullPath,".dds")
//                   yield 
//                    Patch.load patch.info
//                     |> Sg.ofIndexedGeometry
//                     |> Sg.diffuseFileTexture' fullPath true
//                     |> Sg.trafo (Mod.constant patch.info.Local2Global)
//        }

//
//    let mutable loadedTextures = 0
//
//    let loadTextures (baseDir : string) (compressed : bool) (p : Patch) =
//        loadedTextures <- loadedTextures + 1
//        let t = p.info.Textures |> List.head
//        let sourcePath = Path.combine [baseDir; "Images"; t.fileName]
//        let extensions = [ ".texCache"; ".dds"; ".tif"; ".tiff"]
//
//        let rec tryFindTex exts path =
//            match exts with
//                | x::xs -> 
//                    let cuzrrent = System.IO.Path.ChangeExtension(path,x)
//                    if File.Exists cuzrrent then cuzrrent else tryFindTex xs path
//                | [] -> failwithf "texture not found: %s" path
//
//
//        let fullPath = tryFindTex extensions sourcePath
//        let cacheFileName = sprintf "%s.cache" fullPath
//        let cacheInfo = sprintf "%s.cacheInfo" fullPath
//        //if File.Exists cacheFileName then File.Delete cacheFileName
//
//        //CompressedImage.ensureCache runtime fullPath
//
//        let killTexture =  
//            if compressed then
//                CompressedImage.load runtime fullPath { wantMipMaps = true; wantSrgb = false; wantCompressed = false } true |> snd
//            else
//                let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
//                let t = FileTexture(fullPath,config) :> ITexture
//                let b = runtime.PrepareTexture(t)
//                
//                { new IDisposable with member x.Dispose() = runtime.DeleteTexture b }
//        killTexture.Dispose()
//
//
//    let testLoading (runtime : IRuntime) compressed = 
//        loadedTextures <- 0
//        use ctx = runtime.ContextLock
//        let sw = System.Diagnostics.Stopwatch()
//        sw.Start()
//        let textures = patchHierarchies |> List.map (fun (dir,h) -> QTree.map (loadTextures dir compressed) h)
//        sw.Stop()
//        printfn "compressed=%A; took: %A, time per tex: %Ams" compressed sw.MicroTime (float sw.Elapsed.TotalMilliseconds / float loadedTextures)
//        ()
//
//    //testLoading true
    //let g = patchHierarchies |> Seq.map (NaivePatchLoD.toSg preTransform) |> Sg.ofSeq
    //let g = patchHierarchies |> Seq.collect collect |> Sg.ofSeq