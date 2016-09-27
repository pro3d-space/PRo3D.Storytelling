open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Opc

[<EntryPoint>]
let main argv = 
    Test.test2()
    Aardvark.Init()


    use app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()
    win.Text <- "Aardvark rocks \\o/"

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
       

    let initialView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))


    let hier = PatchHierarchy.load @"C:\Aardwork\MarsStuff\47181\OPC_000_000\Patches\patchhierarchy.xml"
    let da = PatchHierarchy.load @"C:\Aardwork\MarsStuff\47181\OPC_000_000\Patches\patchhierarchy.xml"

    let dirs = System.IO.Directory.GetDirectories(@"C:\Aardwork\MarsStuff\47181")

    let patchHierarchies =
        [ for h in dirs do
            let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
            yield  PatchHierarchy.load p
          for h in System.IO.Directory.GetDirectories(@"C:\Aardwork\MarsStuff\47191") do
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

    let g = patchHierarchies |> Seq.collect collect |> Sg.ofSeq
        
    let fillMode = Mod.init FillMode.Fill

    win.Keyboard.KeyDown(Keys.X).Values.Subscribe(fun _ -> 
        transact (fun () -> 
            match fillMode.Value with
                | FillMode.Fill -> fillMode.Value <- FillMode.Line
                | _ -> fillMode.Value <- FillMode.Fill
        )
    ) |> ignore

    let bb = Box3d.Parse("[[-2490136.942562569, 2285877.560081598, -271419.972534080], [-2490134.569473011, 2285880.873850540, -271417.484781946]]")

    let up = bb.Center.Normalized

    let initialView = CameraView.lookAt (bb.Max - bb.Center)  V3d.Zero up
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time


    let sg =
        g  
            //|> Sg.normalize
            |> Sg.transform (Trafo3d.Translation(-bb.Center))
            |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.White |> toEffect
                        DefaultSurfaces.diffuseTexture |> toEffect
                        //DefaultSurfaces.simpleLighting |> toEffect
                    ]
            |> Sg.fillMode fillMode
            |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)

    
    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)
            |> DefaultOverlays.withStatistics

    win.RenderTask <- task
    win.Run()
    0
