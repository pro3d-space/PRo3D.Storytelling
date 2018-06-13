namespace Aardvark.Opc.Viewer

open System
open System.Linq
open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Geometry
open Aardvark.Base.Coder

open Aardvark.Application
open Aardvark.Application.WinForms

open Aardvark.Base.Rendering.Effects
open FShade    

open Aardvark.SceneGraph.Opc
open Aardvark.VRVis.Opc

module Viewer = 
    open MBrace.FsPickler    
    
    let serializer = FsPickler.CreateBinarySerializer()

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
          scene.patchHierarchies |> Seq.map(fun x -> PatchHierarchy.load serializer.Pickle serializer.UnPickle (OpcPaths x)) |> Seq.toList
   
        let g = 
            patchHierarchies 
            |> List.map (fun h -> 
                    
                    Sg.patchLod signature runner h.opcPaths.Opc_DirAbsPath
                                scene.lodDecider scene.useCompressedTextures 
                                ViewerModality.XYZ
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

        use app = new OpenGlApplication(true)
        let win = app.CreateSimpleRenderWindow()
        win.Text <- "Aardvark rocks OPCs \\o/"

        let sg = createSceneGraph win.Runtime win.Sizes win.Mouse win.Keyboard win.FramebufferSignature win.Time scene
        //let sg = createSceneGraph2 win.Sizes win.Mouse win.Keyboard win.Time scene
    
        let cfg =  { BackendConfiguration.Default  with useDebugOutput = true }
        let task =
            RenderTask.ofList [
                app.Runtime.CompileClear(win.FramebufferSignature, (Mod.constant(C4b(30,30,30, 255).ToC4f())))
                app.Runtime.CompileRender(win.FramebufferSignature, cfg, sg) // |> DefaultOverlays.withStatistics
            ]

        win.RenderTask <- task
        win.Run()
        0


    open Configurations.HarriPc

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