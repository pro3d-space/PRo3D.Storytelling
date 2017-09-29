namespace PRo3D

open System
open System.IO
open System.Windows.Forms

open MBrace.FsPickler

open Aardvark.Base

open PRo3D

module Serialization =
    
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
        Sg.sphere 5 color size
            |> Sg.noEvents
            |> Sg.trafo(trafo) 

    let dots (points : alist<V3d>) (color : IMod<C4b>) (view : IMod<CameraView>) =             
        aset {
            for p in points |> ASet.ofAList do
                let scale = computeScale view (Mod.constant p) 5.0
                //let scale = Mod.constant 0.6
                yield mkDotISg color scale (Mod.constant (Trafo3d.Translation(p)))
        } 
        |> Sg.set
        |> Sg.effect [
            toEffect <| Aardvark.Opc.Shader.stableTrafo
            toEffect <| DefaultSurfaces.vertexColor
            toEffect <| DefaultSurfaces.simpleLighting
        ]

module UI =
    open Aardvark.UI

    let wrapToolTip (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "left center"
              attribute "data-variation" "mini" ] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

module Console =    

    let print (x:'a) : 'a =
        printfn "%A" x
        x