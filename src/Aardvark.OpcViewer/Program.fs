open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Opc
open FShade
open Aardvark.Base.Incremental.Load
open OpenTK.Graphics.OpenGL4
open Aardvark.Rendering.GL
open System.IO
//
module Shader =
    open FShade
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    
    let LoDColor  (v : Vertex) =
        fragment {
            if uniform?LodVisEnabled then
                let c : V4d = uniform?LoDColor
                return v.c * c
            else return v.c
        }

[<EntryPoint>]
let main argv = 
    //Test.test2()
    Aardvark.Init()

    



    use app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()
    win.Text <- "Aardvark rocks \\o/"



    let testLoaders extension (config : TextureParams) =
        let sw = System.Diagnostics.Stopwatch()
        use ctx = win.Runtime.ContextLock
        let cnt = 20
        let ddsFiles = System.IO.Directory.EnumerateFiles(@"D:\JR_Export\OPCs\Surface\Sol318id47191\OPC_001_001\Images\Image-0001", extension) |> Seq.toArray
        
        let out = Console.Out
        Console.SetOut System.IO.TextWriter.Null
        for fullPath in ddsFiles do
            let fT = FileTexture(fullPath, config) :> ITexture
            let tex = win.Runtime.PrepareTexture(fT)
            win.Runtime.DeleteTexture(tex)
        
        for i in 0 .. cnt - 1 do
            for fullPath in ddsFiles do
                let fT = FileTexture(fullPath, config) :> ITexture
                sw.Start()
                let tex = win.Runtime.PrepareTexture(fT)
                GL.Sync()
                sw.Stop()
                win.Runtime.DeleteTexture(tex)

        Console.SetOut out

        printfn "config (%s): %A -> %A" extension config (float sw.Elapsed.TotalMilliseconds / (float (ddsFiles.Length * cnt)))

//    testLoaders "*.tif" { wantMipMaps = true; wantSrgb = false; wantCompressed = true }
//    testLoaders "*.tif" { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
//    testLoaders "*.dds" { wantMipMaps = true; wantSrgb = false; wantCompressed = true }
//    testLoaders "*.dds" { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
//
//    System.Environment.Exit 0
//    
    
    
    let getLock () = win.Runtime.ContextLock

    PatchRendering.PatchLod.runner <- new Load.Runner(4,getLock)

    let bbOld = Box3d.Parse("[[-2490136.942562569, 2285877.560081598, -271419.972534080], [-2490134.569473011, 2285880.873850540, -271417.484781946]]")
    let bb = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") // mars

    let bb = Box3d.Parse("[[16034.950038118, 222765.173562629, 802.291122034], [16037.049953225, 222767.420825919, 802.333067577]]") // tunnel

    let up = bb.Center.Normalized

    let speed = Mod.init 5.0

    let initialView = CameraView.lookAt (bb.Max - bb.Center)  V3d.Zero up
    let view = initialView |> DefaultCameraController.controlWithSpeed speed win.Mouse win.Keyboard win.Time

    let quadGeometry =
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray = ([|0;1;2; 0;2;3|] :> Array),
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, [| V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO |] :> Array
                    DefaultSemantic.Colors, [| C4b.Red; C4b.Green; C4b.Blue; C4b.Yellow |] :> Array
                ]
        )
       
    let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 1.0 10000.0 (float s.X / float s.Y))
    let preTransform = Trafo3d.Translation(-bb.Center) * Trafo3d.FromOrthoNormalBasis(V3d.IOO, -V3d.OOI, V3d.OIO)

    let hier = PatchHierarchy.load @"C:\Aardwork\MarsStuff\47181\OPC_000_000\Patches\patchhierarchy.xml"
    let da = PatchHierarchy.load @"C:\Aardwork\MarsStuff\47181\OPC_000_000\Patches\patchhierarchy.xml"

    let dirs = System.IO.Directory.GetDirectories(@"C:\Aardwork\MarsStuff\47181")

    let surfaces = System.IO.Directory.GetDirectories(@"D:\JR_Export\OPCs\Surface","MSL_Mastcam_Sol_*") // mars
    let dirs = System.IO.Directory.GetDirectories(@"D:\perjen\2011_Bestandsaufnahme\OPC") // tunnel
//    let dirs = 
//        [
//            for surface in surfaces  do 
//                yield! System.IO.Directory.GetDirectories(surface)
//        ] 

    let patchHierarchies =
        [ for h in dirs do
            let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
            yield  PatchHierarchy.load p
        ] 

    let runtime = app.Runtime |> unbox<Aardvark.Rendering.GL.Runtime>

    let mutable loadedTextures = 0

    let loadTextures compressed (p : Patch) =
        loadedTextures <- loadedTextures + 1
        let t = p.info.Textures |> List.head
        let sourcePath = Path.combine [p.info.BaseDir; t.fileName]
        let extensions = [ ".texCache"; ".dds"; ".tif"; ".tiff"]

        let rec tryFindTex exts path =
            match exts with
                | x::xs -> 
                    let cuzrrent = System.IO.Path.ChangeExtension(path,x)
                    if File.Exists cuzrrent then cuzrrent else tryFindTex xs path
                | [] -> failwithf "texture not found: %s" path


        let fullPath = tryFindTex extensions sourcePath
        let cacheFileName = sprintf "%s.cache" fullPath
        let cacheInfo = sprintf "%s.cacheInfo" fullPath
        //if File.Exists cacheFileName then File.Delete cacheFileName

        //CompressedImage.ensureCache runtime fullPath

        let killTexture =  
            if compressed then
                CompressedImage.load runtime fullPath { wantMipMaps = true; wantSrgb = false; wantCompressed = false } true |> snd
            else
                let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
                let t = FileTexture(fullPath,config) :> ITexture
                let b = runtime.PrepareTexture(t)
                
                { new IDisposable with member x.Dispose() = runtime.DeleteTexture b }
        killTexture.Dispose()


    let testLoading compressed= 
        loadedTextures <- 0
        use ctx = runtime.Context.ResourceLock
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let textures = patchHierarchies |> List.map (QTree.map (loadTextures compressed))
        sw.Stop()
        printfn "compressed=%A; took: %A, time per tex: %Ams" compressed sw.MicroTime (float sw.Elapsed.TotalMilliseconds / float loadedTextures)
        ()

    //testLoading true
//    testLoading false
//    System.Environment.Exit 0

    let sg = 
        match hier with
            | Node(patch,_) ->
                Patch.load patch.info
                    |> Sg.ofIndexedGeometry
                    //|> Sg.trafo (Mod.constant patch.info.Local2Global)
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.White |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
            | _ ->
                failwith ""


    let rec collect current  =
        seq {
            match current with
                | Node(n, xs) ->
                   for x in xs do yield! collect x
                | Leaf(patch) ->
                   let tex = patch.info.Textures |> List.head 
                   let fullPath = Path.combine [patch.info.BaseDir; tex.fileName]
                   let fullPath = System.IO.Path.ChangeExtension(fullPath,".dds")
                   yield 
                    Patch.load patch.info
                     |> Sg.ofIndexedGeometry
                     |> Sg.diffuseFileTexture' fullPath true
                     |> Sg.trafo (Mod.constant patch.info.Local2Global)
        }

    let g = patchHierarchies |> List.map (fun t -> PatchRendering.PatchLod.PatchNode(win.FramebufferSignature, PatchRendering.PatchLod.toRoseTree t) :> ISg) |> Sg.ofList
    //let g = patchHierarchies |> Seq.map (NaivePatchLoD.toSg preTransform) |> Sg.ofSeq
    //let g = patchHierarchies |> Seq.collect collect |> Sg.ofSeq
        
    let fillMode = Mod.init FillMode.Fill

    let lodVisEnabled = Mod.init false

    win.Keyboard.KeyDown(Keys.X).Values.Subscribe(fun _ -> 
        transact (fun () -> 
            match fillMode.Value with
                | FillMode.Fill -> fillMode.Value <- FillMode.Line
                | _ -> fillMode.Value <- FillMode.Fill
        )
    ) |> ignore

    win.Keyboard.KeyDown(Keys.L).Values.Subscribe(fun _ -> 
        transact (fun () -> 
            lodVisEnabled.Value <- lodVisEnabled.Value |> not
        )
    ) |> ignore

    win.Keyboard.KeyDown(Keys.G).Values.Subscribe(fun _ -> 
        for i in 0 .. 10 do
            System.GC.Collect()
        System.GC.WaitForFullGCComplete() |> ignore


        printfn "%A" ( app.Runtime.Context.MemoryUsage ) 
    ) |> ignore

    win.Keyboard.KeyDown(Keys.PageDown).Values.Subscribe(fun _ -> 
        transact (fun () -> 
            speed.Value <- speed.Value / 2.0
            printfn "speed: %A" speed.Value
        )
    ) |> ignore

    win.Keyboard.KeyDown(Keys.PageUp).Values.Subscribe(fun _ -> 
        transact (fun () -> 
            speed.Value <- speed.Value * 2.0
            printfn "speed: %A" speed.Value
        )
    ) |> ignore
    

    let sg =
        g   
            //|> Sg.normalize
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

    
    let cfg =  { BackendConfiguration.Default  with useDebugOutput = true }
    let task =
        RenderTask.ofList [
            app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.Gray)
            app.Runtime.CompileRender(win.FramebufferSignature, cfg, sg) |> DefaultOverlays.withStatistics
        ]

    win.RenderTask <- task
    win.Run()
    0
