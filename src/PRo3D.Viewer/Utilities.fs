namespace PRo3D

open System
open System.IO
open System.Windows.Forms

open MBrace.FsPickler
open MBrace.FsPickler.Json
open Aardvark.Base
open PRo3D
open Aardvark.Opc

module Serialization =
    open Aardvark.UI.Incremental
    
    let cache = Serialization.cache

    let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)
    let jsonSerializer = FsPickler.CreateJsonSerializer(indent=true)

    let save path (d : 'a) =    
        let arr =  binarySerializer.Pickle d
        File.WriteAllBytes(path, arr);
        d
    
    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)

    let saveJson path (d : 'a) =    
        let json =  jsonSerializer.PickleToString d
        writeToFile path json
        d

    let loadJsonAs<'a> path : 'a =
        let json = File.readAllText path
        jsonSerializer.UnPickleOfString json

    let fileExists filePath =
        match System.IO.File.Exists filePath with
            | true -> Some filePath
            | false -> None

module Dialogs =
    open Microsoft.WindowsAPICodePack.Dialogs    

    let saveDialog  (w : Form)  =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new SaveFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r
    
    let openDialog (w : Form) =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new OpenFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r

    let openDialog2 (w : Form)(isFolder : bool)(isMulti : bool) =
        let mutable r = []
        w.Invoke(Action(fun _ -> 
            let dialog = new CommonOpenFileDialog()
            dialog.Multiselect <- isMulti
          //  dialog.InitialDirectory <- "C:\\Users"
            dialog.IsFolderPicker <- isFolder
            if dialog.ShowDialog() = CommonFileDialogResult.Ok then
                r <- dialog.FileNames |> Seq.toList
            else r <- []
        )) |> ignore
        r
      
    let openFolders (w : Form) =
        openDialog2 w true true

    let openFolder (w : Form) =
        openDialog2 w true false

module Box3d =
    let extendBy (box:Box3d) (b:Box3d) =
        box.ExtendBy(b)
        box

    let ofSeq (bs:seq<Box3d>) =
        let box = 
            match bs |> Seq.tryHead with
                | Some b -> b
                | None -> failwith "box sequence must not be empty"
                    
        for b in bs do
            box.ExtendBy(b)
        box

module Mod =
    open Aardvark.Base.Incremental

    let bindOption (m : IMod<Option<'a>>) (defaultValue : 'b) (project : 'a -> IMod<'b>)  : IMod<'b> =
        m |> Mod.bind (function | None   -> Mod.constant defaultValue       
                                | Some v -> project v)

module Copy =
    let rec copyAll' (source : DirectoryInfo) (target : DirectoryInfo) skipExisting =
        
        // Check if the target directory exists, if not, create it.
        if not(Directory.Exists target.FullName) then
            Directory.CreateDirectory target.FullName |> ignore

        // Copy each file into it's new directory.
        for fi in source.GetFiles() do
             let sourceFile = fi.FullName
             let targetFile = Path.Combine(target.FullName, fi.Name)

             if ((sourceFile.ToLower() == targetFile.ToLower()) || (skipExisting && File.Exists(targetFile))) then
                Log.warn "Skipping %s, already exists" targetFile
             else
                Log.line "Copying to %s" targetFile
                fi.CopyTo(Path.Combine((target.ToString()), fi.Name), true) |> ignore      
                
        // Copy each subdirectory using recursion.
        let bla = source.GetDirectories()
        for srcSubDir in bla do
            let nextTgtSubDir = target.CreateSubdirectory(srcSubDir.Name)
            copyAll' srcSubDir nextTgtSubDir skipExisting

    let copyAll source target skipExisting=
        let s = DirectoryInfo(source)
        let t = DirectoryInfo(target)

        copyAll' s t skipExisting

module Sg =
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph
    open Aardvark.Base.Rendering
    open Aardvark.UI
    open Aardvark.Rendering.Text

    let computeInvariantScale (view : IMod<CameraView>) (near : IMod<float>) (p:IMod<V3d>) (size:IMod<float>) (hfov:IMod<float>) =
            adaptive {
                let! p = p
                let! v = view
                let! near = near
                let! size = size
                let! hfov = hfov
                let hfov_rad = Conversion.RadiansFromDegrees(hfov)
               
                let wz = Fun.Tan(hfov_rad / 2.0) * near * size
                let dist = V3d.Distance(p, v.Location)

                return ( wz / near ) * dist
            }

    let edgeLines (close : bool) (points : alist<V3d>) (trafo:IMod<Trafo3d>) =
        points
            |> AList.map(fun d -> trafo.GetValue().Backward.TransformPos d)
            |> AList.toMod 
            |> Mod.map (fun l ->
                let list = PList.toList l
                let head = list |> List.tryHead
                    
                match head with
                    | Some h -> if close then list @ [h] else list
                                    |> List.pairwise
                                    |> List.map (fun (a,b) -> new Line3d(a,b))
                                    |> List.toArray
                    | None -> [||])

    let linePass = RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main
                    
    let lines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>) (id : IMod<Guid>) = 
        let box = points |> AList.toMod |> Mod.map (fun d -> if d.Count > 2 then Box3d d else Box3d.Unit)

        let edges = edgeLines false points trafo

        //invisible cylinders for polyline picking
        let cylinders = 
            edges 
                |> Mod.map (fun e ->
                        [
                            for edge in e do                                    
                                let v = edge.P1 - edge.P0
                                let c = Cylinder3d(edge.P0,v.Normalized, v.Length, 0.25)
                                yield 
                                    Sg.ofList[]
                                        |> Sg.noEvents                                                                                          
                                        |> Sg.pickable (PickShape.Cylinder c)
                                        |> Sg.trafo trafo
                                        |> Sg.withEvents []
                                            //Sg.onMouseDownEvt (fun sceneHit -> Select (id |> Mod.force))]
                        ] |> Sg.ofList)
                |> Sg.dynamic
                                                            
        edges
            |> Sg.lines color
            |> Sg.noEvents
            |> Sg.shader {
                do! Aardvark.Opc.Shader.stableTrafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.thickLine    
            }                               
            |> Sg.trafo trafo
            |> Sg.uniform "LineWidth" width
            //|> Sg.pass linePass
            //|> Sg.depthTest (Mod.constant DepthTestMode.None)

    let computeScale (view : IMod<CameraView>)(p:IMod<V3d>)(size:float) =
        adaptive {
            let! p = p
            let! v = view
            let distV = p - v.Location
            let distF = V3d.Dot(v.Forward, distV)
            return distF * size / 800.0 //needs hfov at this point
        }    

    let mkDotISg color size trafo = 
        Sg.sphere 2 color size
            |> Sg.noEvents
            |> Sg.trafo(trafo) 
        
    let dots (points : alist<V3d>) (color : IMod<C4b>) (view : IMod<CameraView>) =
        aset {
            for p in points |> ASet.ofAList do
                let scale = computeScale view (Mod.constant p) 5.0                               
                yield mkDotISg color scale (Mod.constant (Trafo3d.Translation(p)))
        } 
        |> Sg.set
        |> Sg.effect [
            toEffect <| Aardvark.Opc.Shader.stableTrafo
            toEffect <| DefaultSurfaces.vertexColor
          //  toEffect <| Aardvark.Opc.Shader.stableLight
        ]

    let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) = 
        let right = forw.Cross up
        let rotTrafo = 
            new Trafo3d(
                new M44d(
                    right.X, up.X, forw.X, 0.0,
                    right.Y, up.Y, forw.Y, 0.0,
                    right.Z, up.Z, forw.Z, 0.0,
                    0.0,     0.0,  0.0,    1.0
                ),
                new M44d(
                    right.X, right.Y, right.Z, 0.0,
                    up.X,    up.Y,    up.Z,    0.0,
                    forw.X,  forw.Y,  forw.Z,  0.0,
                    0.0,     0.0,     0.0,     1.0
                )
        )
        rotTrafo * modelt

    let text (view : IMod<CameraView>) near hfov pos modelTrafo text  =
           
        let invScaleTrafo = 
            computeInvariantScale view near pos (Mod.constant 0.05) hfov |> Mod.map Trafo3d.Scale

        let billboardTrafo = 
            adaptive {
                let! v = view
                let! modelt = modelTrafo

                return screenAligned v.Forward v.Up modelt
            }
        let bounds : Border2d = { left = 0.1; right = 0.1; top = 0.1; bottom = 0.1 }

        Sg.textWithBackground (Font.create "Consolas" FontStyle.Regular) C4b.White C4b.Black bounds text
            |> Sg.noEvents
            |> Sg.shader {
                    do! Aardvark.Opc.Shader.stableTrafo
                }
            // |> Sg.transform (Trafo3d.FromOrthoNormalBasis(-V3d.IOO, V3d.OOI, V3d.OIO))
            |> Sg.trafo invScaleTrafo
            |> Sg.trafo billboardTrafo  

module UI =
    open Aardvark.UI
    open Aardvark.Base.Incremental

    let wrapToolTip (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "top center"
              attribute "data-variation" "mini" ] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipRight (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "right center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let dropDown'' (values : alist<'a>)(selected : IMod<Option<'a>>) (change : Option<'a> -> 'msg) (f : 'a ->string)  =

        let attributes (name : string) =
            AttributeMap.ofListCond [
                always (attribute "value" (name))
                onlyWhen (
                        selected 
                            |> Mod.map (
                                fun x -> 
                                    match x with
                                        | Some s -> name = f s
                                        | None   -> name = "-None-"
                            )) (attribute "selected" "selected")
            ]

        let ortisOnChange  = 
            let cb (i : int) =
                let currentState = values.Content |> Mod.force
                change (PList.tryAt (i-1) currentState)
                    
            onEvent "onchange" ["event.target.selectedIndex"] (fun x -> x |> List.head |> Int32.Parse |> cb)

        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
            (
                alist {
                    yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
                    yield! values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
                }
            )

module Console =    

    let print (x:'a) : 'a =
        printfn "%A" x
        x

module Lenses = 
    let get    (lens : Lens<'s,'a>) (s:'s) : 'a              = lens.Get(s)
    let set    (lens : Lens<'s,'a>) (v : 'a) (s:'s) : 's     = lens.Set(s,v)
    let set'   (lens : Lens<'s,'a>) (s:'s) (v : 'a)  : 's     = lens.Set(s,v)
    let update (lens : Lens<'s,'a>) (f : 'a->'a) (s:'s) : 's = lens.Update(s,f)