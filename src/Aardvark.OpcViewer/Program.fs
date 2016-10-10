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

module Shader =
    open FShade
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    
    let LoDColor  (v : Vertex) =
        fragment {
            if uniform?LodVisEnabled then
                let c : V4d = uniform?LoDColor
                let gamma = 1.0
                let grayscale = 0.2126 * v.c.X ** gamma + 0.7152 * v.c.Y ** gamma  + 0.0722 * v.c.Z ** gamma 
                return grayscale * c 
            else return v.c
        }

type FileName = string
type OpcViewer = { 
    patchHierarchies : seq<FileName> 
    boundingBox      : Box3d
    preTransform     : Trafo3d 
    near             : float
    far              : float
    speed            : float
    lodDecider       : PatchLod.LodDecider
}

let mars = 
    { 
        preTransform     = Trafo3d.Identity
        patchHierarchies = 
              System.IO.Directory.GetDirectories(@"D:\JR_Export\OPCs\Surface","MSL_Mastcam_Sol_*") 
              |> Seq.collect System.IO.Directory.GetDirectories
        boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
        near             = 0.1
        far              = 10000.0
        speed            = 5.0
        lodDecider       = DefaultMetrics.mars
    }

let mola = 
    { 
      preTransform     = Trafo3d.Identity
      patchHierarchies = System.IO.Directory.GetDirectories(@"D:\Mars_MGS_MOLA_DEM_mosaic_global_0.01") 
      boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
      near             = 100.0
      far              = 100000000.0
      speed            = 32768.0
      lodDecider       = DefaultMetrics.mars
    }

let perjen =
    { 
        preTransform     = Trafo3d.FromOrthoNormalBasis(V3d.IOO, -V3d.OOI, V3d.OIO)
        patchHierarchies = System.IO.Directory.GetDirectories(@"D:\perjen\2011_Bestandsaufnahme\OPC")
        boundingBox      = Box3d.Parse("[[16034.950038118, 222765.173562629, 802.291122034], [16037.049953225, 222767.420825919, 802.333067577]]")
        near             = 0.1
        far              = 10000.0
        speed            = 5.0
        lodDecider       = DefaultMetrics.tunnel
    }

[<EntryPoint>]
let main argv = 
    //Test.test2()
    Aardvark.Init()

    use app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()
    win.Text <- "Aardvark rocks OPCs \\o/"

    let runner = win.Runtime.CreateLoadRunner 2

    let scene = perjen

    let bb = scene.boundingBox
    let speed = Mod.init scene.speed

    let initialView = CameraView.lookAt (bb.Max - bb.Center)  V3d.Zero bb.Center.Normalized
    let view = initialView |> DefaultCameraController.controlWithSpeed speed win.Mouse win.Keyboard win.Time
       
    let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 scene.near scene.far (float s.X / float s.Y))
    let preTransform = Trafo3d.Translation(-bb.Center) * scene.preTransform

    let patchHierarchies =
        [ for h in scene.patchHierarchies do
            let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
            yield PatchHierarchy.load p
        ] 


    let g = patchHierarchies |> List.map (fun (d,h) -> Sg.patchLod win.FramebufferSignature runner d scene.lodDecider (PatchLod.toRoseTree h)) |> Sg.ofList
        
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

    
    let cfg =  { BackendConfiguration.Default  with useDebugOutput = false }
    let task =
        RenderTask.ofList [
            app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.Gray)
            app.Runtime.CompileRender(win.FramebufferSignature, cfg, sg) |> DefaultOverlays.withStatistics
        ]

    win.RenderTask <- task
    win.Run()
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