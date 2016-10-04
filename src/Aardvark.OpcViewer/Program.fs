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
//
module Shader =
    open FShade
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    
    let LoDColor  (v : Vertex) =
        fragment {
            let c : V4d = uniform?LoDColor
            return v.c * c
        }

[<EntryPoint>]
let main argv = 
    //Test.test2()
    Aardvark.Init()




    use app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()
    win.Text <- "Aardvark rocks \\o/"

    let runUploads (f : unit -> unit) =
        use r = win.Runtime.ContextLock
        f ()

    PatchRendering.PatchLod.runner <- new Load.Runner(1,runUploads)

    let bbOld = Box3d.Parse("[[-2490136.942562569, 2285877.560081598, -271419.972534080], [-2490134.569473011, 2285880.873850540, -271417.484781946]]")
    let bb = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]")

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
       
    let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))
    let preTransform = Trafo3d.Translation(-bb.Center)

    let hier = PatchHierarchy.load @"C:\Aardwork\MarsStuff\47181\OPC_000_000\Patches\patchhierarchy.xml"
    let da = PatchHierarchy.load @"C:\Aardwork\MarsStuff\47181\OPC_000_000\Patches\patchhierarchy.xml"

    let dirs = System.IO.Directory.GetDirectories(@"C:\Aardwork\MarsStuff\47181")

    let surfaces = System.IO.Directory.GetDirectories(@"F:\JR_Export\OPCs\Surface","MSL_Mastcam_Sol_*")
    let dirs = 
        [
            for surface in surfaces   do 
                yield! System.IO.Directory.GetDirectories(surface)
        ] 

    let patchHierarchies =
        [ for h in dirs do
            let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
            yield  PatchHierarchy.load p
        ] 

    let sg = 
        match hier with
            | Node(patch,_) ->
                patch.geometry.Value
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
                    patch.geometry.Value
                     |> Sg.ofIndexedGeometry
                     |> Sg.diffuseFileTexture' fullPath true
                     |> Sg.trafo (Mod.constant patch.info.Local2Global)
        }

    let g = patchHierarchies |> List.map (fun t -> PatchRendering.PatchLod.PatchNode(win.FramebufferSignature, PatchRendering.PatchLod.toRoseTree t) :> ISg) |> Sg.ofList
    //let g = patchHierarchies |> Seq.map (NaivePatchLoD.toSg preTransform) |> Sg.ofSeq
    //let g = patchHierarchies |> Seq.collect collect |> Sg.ofSeq
        
    let fillMode = Mod.init FillMode.Fill

    win.Keyboard.KeyDown(Keys.X).Values.Subscribe(fun _ -> 
        transact (fun () -> 
            match fillMode.Value with
                | FillMode.Fill -> fillMode.Value <- FillMode.Line
                | _ -> fillMode.Value <- FillMode.Fill
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
                        //DefaultSurfaces.diffuseTexture |> toEffect
                        Shader.LoDColor |> toEffect
                        //DefaultSurfaces.simpleLighting |> toEffect
                    ]
            |> Sg.uniform "LoDColor" (Mod.constant C4b.Gray)
            |> Sg.fillMode fillMode
            |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)

    
    let cfg =  BackendConfiguration.Default 
    let task =
        RenderTask.ofList [
            app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.Gray)
            app.Runtime.CompileRender(win.FramebufferSignature, cfg, sg) |> DefaultOverlays.withStatistics
        ]

    win.RenderTask <- task
    win.Run()
    0
