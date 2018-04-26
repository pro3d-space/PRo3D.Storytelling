namespace Dgm

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Rendering.GL

module DGM = 

    let inline isNan (v : V3f) = v.AnyNaN

    let slowSystemParseDouble (c : char[], start : int, endIndex : int) =
        let s = String(c,start,endIndex-start)
        let mutable r = 0.0
        if Double.TryParse(s,Globalization.NumberStyles.Float,Globalization.CultureInfo.InvariantCulture, &r) then r
        else failwithf "could not parse int double: %s" s

    let mutable doubleParse : char[] * int * int -> float = slowSystemParseDouble

    let createIndex (vi : Matrix<V3f>) =
        let dx = vi.Info.DX
        let dy = vi.Info.DY
        let dxy = dx + dy
        let mutable arr = Array.zeroCreate (int (vi.SX - 1L) * int (vi.SY - 1L) * 6)
        let mutable cnt = 0
        
        vi.SubMatrix(V2l.Zero,vi.Size-V2l.II).ForeachXYIndex(fun x y index -> 
            let i00 = index
            let i10 = index + dy
            let i01 = index + dx
            let i11 = index + dxy

            let p00 = vi.[i00] |> isNan
            let p01 = vi.[i10] |> isNan
            let p10 = vi.[i01] |> isNan
            let p11 = vi.[i11] |> isNan

            if p00 || p01 || p10 || p11 then ()
            else 
                arr.[cnt + 0] <- (int i00)
                arr.[cnt + 1] <- (int i10)
                arr.[cnt + 2] <- (int i11)
                arr.[cnt + 3] <- (int i00)
                arr.[cnt + 4] <- (int i11)
                arr.[cnt + 5] <- (int i01)
                cnt <- cnt + 6
        )
        Array.Resize(&arr, cnt)
        arr

    type DGM = 
        {
            ncols : int
            nrows : int
            corner : V2d
            cellSize : float
            noDataValue : float
        }

    let loadDgm (fileName : string) = 
        let extract (tag : string)  (s : String) =
            match s.SplitOnWhitespace() with
                | [|t;value|] when t = tag -> value
                | _ -> failwithf "could not parse tag: %s in file: %s" tag fileName

        let inline pint (s : string) = 
            let mutable r = 0
            if Int32.TryParse(s,&r) then r else failwithf "could not parse int from: %s in file: %s" s fileName

        let inline pfloat (s : String) =
            let mutable r = 0.0
            if Double.TryParse(s,Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture, &r) then r
            else failwithf "could not parse int double: %s in file: %s" s fileName

        let txt        = IO.File.OpenText(fileName) 
        let ncols      = txt.ReadLine() |> extract "NCOLS"     |> pint
        let nrows      = txt.ReadLine() |> extract "NROWS"     |> pint
        let xllcorner  = txt.ReadLine() |> extract "XLLCORNER" |> pfloat
        let yllcorner  = txt.ReadLine() |> extract "YLLCORNER" |> pfloat
        let cellSize   = txt.ReadLine() |> extract "CELLSIZE"  |> pfloat
        let noData     = txt.ReadLine() |> extract "NODATA_VALUE" |> pfloat
        let bufferSize = IO.FileInfo(fileName).Length |> int
        let buffer     = Array.zeroCreate bufferSize 
        let elementCnt = ncols * nrows
        let result     = Array.zeroCreate elementCnt
        let mutable cursor = 0
        while not txt.EndOfStream do
            let cnt = txt.ReadBlock(buffer,0,bufferSize)
            let mutable last = 0
            let mutable i = 0
            while i < cnt do
                let current = buffer.[i]
                if current = ' ' || current = '\n' || current = '\r' then
                    let cnt = i - last
                    if cnt > 0 then
                        let f = doubleParse(buffer,last,i)
                        result.[cursor] <- f
                        cursor <- cursor + 1
                    last <- i + 1
                    i <- i + 1
                elif i + 1 < cnt && buffer.[i+1] = '\r' || buffer.[i+1] = '\n' then
                   i <- i + 2
                   last <- i + 2
                else i <- i + 1
        if cursor <> elementCnt then failwithf "element count: %d does not match expected: %d in file: %s" cursor elementCnt fileName
        {
            ncols         = ncols
            nrows         = nrows
            corner        = V2d(xllcorner,yllcorner)
            cellSize      = cellSize
            noDataValue   = noData
        }, result

    let computeVertices (dgm : DGM) (arr : float[]) =
        Matrix<V3f>(V2i(dgm.ncols,dgm.nrows)).SetByCoord(fun (c : V2l) ->
            let height = arr.[int c.X + int c.Y * dgm.ncols]
            if height = dgm.noDataValue then V3f.NaN
            else V3f(float c.X * dgm.cellSize, (float dgm.nrows - float c.Y) * dgm.cellSize, height)
        )

    let normals (dgm : DGM) (o : Matrix<V3f>) =
        let m = o.SubMatrix(V2l.II,o.Size-V2l.II)
        o.Copy().SubMatrix(V2l.II,m.Size-V2l.II).SetByCoord(fun (l:V2l)-> 
            let v = m.[l]
            let W = m.[l.X-1L,l.Y]
            let O = m.[l.X+1L,l.Y]
            let N = m.[l.X,l.Y+1L]
            let S = m.[l.X,l.Y-1L]
            let n1 = (W-v).Cross(N-v)
            let n2 = (O-v).Cross(S-v)
            (n1+n2).Normalized
        )

    let createUVs (dgm : DGM) (o : Matrix<V3f>) =
        Matrix<V2f>(o.Size).SetByCoord(fun (l:V2l)-> 
            V2f(float l.X / float o.Size.X, 1.0 - float l.Y / float o.Size.Y)
        )


    let dgm2IndexGeometry (fileName : string) =

        let sw = System.Diagnostics.Stopwatch()
        Log.startTimed "Parsing"
        sw.Start()
        let dgm,heightfield = loadDgm fileName
        sw.Stop()
        Log.stop()

        Log.startTimed "Computing vertices"
        let vertices = computeVertices dgm heightfield
        Log.stop()

        Log.startTimed "Computing normals"
        let normals = normals dgm vertices
        Log.stop()

        Log.startTimed "Creating Mesh"
        let index = createIndex vertices
        Log.stop()

        Log.startTimed "Creating UVS"
        let uvs = createUVs dgm vertices
        Log.stop()

        printfn "ns/double: %f" (float sw.Elapsed.TotalMilliseconds * 1000000.0 / float heightfield.Length)

        let ig = 
            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,
                IndexArray = index,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Normals, normals.Data :> Array
                        DefaultSemantic.Positions, vertices.Data :> Array
                        DefaultSemantic.DiffuseColorCoordinates, uvs.Data :> Array
                    ]
            )
        ig,dgm

module WMS = 
    
    open System.Net
    open DGM

    let url = "https://gis.tirol.gv.at/arcgis/services/Service_Public/orthofoto/MapServer/WmsServer?SERVICE=WMS&Version=1.1.1&REQUEST=GetMap&LAYERS=Image_2013-2015_RGB&STYLES=&SRS=EPSG:31254&BBOX=49620,202960,103950,257300&WIDTH=4096&HEIGHT=4096&FORMAT=image/png"

    let mkRequest (resolution : V2i) (bb : Box2d) =
        sprintf "https://gis.tirol.gv.at/arcgis/services/Service_Public/orthofoto/MapServer/WmsServer?SERVICE=WMS&Version=1.1.1&REQUEST=GetMap&LAYERS=Image_2013-2015_RGB&STYLES=&SRS=EPSG:31254&BBOX=%f,%f,%f,%f&WIDTH=%d&HEIGHT=%d&FORMAT=image/png" bb.Min.X bb.Min.Y bb.Max.X bb.Max.Y resolution.X resolution.Y

    let downloadImage (prefix : string) (resolution : V2i) (bb : Box2d) =
        let request = mkRequest resolution bb
        let target = sprintf "%s_%A_.png" prefix bb
        if System.IO.File.Exists target then PixImage.Create(target).ToPixImage<byte>()
        else
            use wr = new WebClient()
            printfn "%s -> %s" request target
            wr.DownloadFile(request, target)
            System.Threading.Thread.Sleep(100)
            PixImage.Create(target).ToPixImage<byte>()

    let downloadDgm (path : string) (totalSize : V2i) (tileCount : int)  (dgm : DGM)=
        
        let tilePrefix = Path.combine [System.IO.Path.GetDirectoryName(path); "tile_"]
        let total = Box2d.FromMinAndSize(dgm.corner, V2d(float dgm.ncols* dgm.cellSize,float dgm.nrows* dgm.cellSize))
        let tileSizeWorld = total.Size /  V2d(tileCount,tileCount)

        let imageTile = totalSize / tileCount
        let target = PixImage<byte>(Col.Format.RGB,totalSize)

        let bbs = 
            [
                for y in 0 .. imageTile.Y .. totalSize.Y - 1 do
                    for x in 0 .. imageTile.X .. totalSize.X - 1 do
                        let minNormalized = V2d(x,y) / V2d(totalSize)
                        let bb = Box2d.FromMinAndSize(total.Min + minNormalized*total.Size,total.Size / V2d(tileCount,tileCount))
                        let img = downloadImage tilePrefix imageTile bb
                        printfn "[WMS] writing: %A" ((x,y), imageTile)
                        target.Set(V2i(x,target.Size.Y - y - imageTile.Y),img)
            ]

        let composeImagePath = Path.combine [System.IO.Path.GetDirectoryName(path); "composed.png"]
        target.SaveAsImage(composeImagePath)
        composeImagePath
        


module Viewer =

    [<EntryPoint>]
    let main ars =



        Ag.initialize()
        Aardvark.Init()

        let fastParse (c : char[], start : int, endIndex : int) =
            let mutable r = 0.0
            if FastDoubleParse.FastDoubleParse.TryParseDoubleFastStream(c,start,endIndex,&r) then r
            else failwithf "could not parse int double: %A" c 

        DGM.doubleParse <- fastParse

        let prefix =  @"C:\Aardwork\tirol"
        let IL = [ "IL_DGM10.asc"; "IBK_DGM10.asc"; "IM_DGM10.asc"; "LA_DGM10.asc" ]

        // https://gis.tirol.gv.at/arcgis/services/Service_Public/orthofoto/MapServer/WmsServer?
        // Image_2013-2015_RGB
        let sgs =
            [
                for m in IL do
                    let path = Path.combine [prefix;m]
                    let (ig,dgm) = path |> DGM.dgm2IndexGeometry 
                    Log.start "[WGM download for: %s]" m
                    let texturePath = WMS.downloadDgm path (V2i(16384,16384)) 16 dgm
                    let shift = V3d(dgm.corner.X, dgm.corner.Y,0.0)
                    let sg = 
                        ig
                         |> Sg.ofIndexedGeometry
                         |> Sg.diffuseFileTexture' texturePath true
                         |> Sg.transform (Trafo3d.Translation shift)
                    yield dgm,sg
            ]
            

        use app = new OpenGlApplication()
        let win = app.CreateSimpleRenderWindow(1)
        win.Text <- "Aardvark rocks \\o/"

        let corner = V3d((sgs |> List.head |> fst).corner, 0.0)

        let view = CameraView.LookAt(corner, corner + V3d.IIO, V3d.OOI)
        let perspective = 
            win.Sizes 
              |> Mod.map (fun s -> Frustum.perspective 60.0 20.0 800000.0 (float s.X / float s.Y))

        let speed = Mod.init 10.0
        let viewTrafo = DefaultCameraController.controlWithSpeed speed win.Mouse win.Keyboard win.Time view

        let quadSg =
            sgs |> List.map snd |> Sg.group

        let sg =
            quadSg 
                |> Sg.effect [
                    DefaultSurfaces.trafo                 |> toEffect
                    DefaultSurfaces.constantColor C4f.White |> toEffect
                    DefaultSurfaces.diffuseTexture |> toEffect
                    DefaultSurfaces.simpleLighting        |> toEffect
                  ]
               |> Sg.cullMode (Mod.constant CullMode.Clockwise)
               //|> Sg.fillMode (Mod.constant FillMode.Line)
               |> Sg.viewTrafo (viewTrafo   |> Mod.map CameraView.viewTrafo )
               |> Sg.projTrafo (perspective |> Mod.map Frustum.projTrafo    )

        //let bb = sg.LocalBoundingBox().GetValue() |> printfn "%A"
        let task = app.Runtime.CompileRender(win.FramebufferSignature, BackendConfiguration.ManagedOptimized, sg.RenderObjects())

        win.Keyboard.KeyDown(Keys.PageUp).Values.Subscribe(fun _ ->   
            transact (fun () -> speed.Value <- speed.Value * 2.0) 
        ) |> ignore
        win.Keyboard.KeyDown(Keys.PageDown).Values.Subscribe(fun _ ->   
            transact (fun () -> speed.Value <- speed.Value / 2.0) 
        ) |> ignore
        win.RenderTask <- task //|> DefaultOverlays.withStatistics
        win.Run()
        0





