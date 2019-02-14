namespace PRo3D

#nowarn "8989"

open System
open System.IO
open System.Windows.Forms

open MBrace.FsPickler
open MBrace.FsPickler.Json
open Aardvark.Base
open Aardvark.SceneGraph.Opc
open PRo3D

module Serialization =
  //  open Aardvark.UI.Incremental
    
    let registry = new CustomPicklerRegistry()    
    let cache = PicklerCache.FromCustomPicklerRegistry registry    

    let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)
    let jsonSerializer = FsPickler.CreateJsonSerializer(picklerResolver = cache) //  FsPickler.CreateJsonSerializer(indent=true) // 

    let save path (d : 'a) =    
        let arr =  binarySerializer.Pickle d
        File.WriteAllBytes(path, arr);
        d
    
    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr

    let writeLinesToFile path (contents : list<string>) =
        System.IO.File.WriteAllLines(path, contents)

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)

    let saveJson path (d : 'a) =    
        let json =  jsonSerializer.PickleToString d
        writeToFile path json
        d

    let loadJsonAs<'a> path : 'a =
        let json = System.IO.File.ReadAllText path //readAllText path
        let test2 : 'a = jsonSerializer.UnPickleOfString<'a> json
        test2
        
    let fileExists filePath =
        match System.IO.File.Exists filePath with
          | true  -> Some filePath
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

    let openDialog3 (w : Form) (path: string) =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new OpenFileDialog()
            dialog.InitialDirectory <- path
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r
      
    let openFolders (w : Form) =
        openDialog2 w true true

    let openFolder (w : Form) =
        openDialog2 w true false

    // aardium dialogs

    open Aardvark.UI
    
    //let onSaveFile (w : Form) =
    //    let cb xs =
    //        match xs with
    //            | x::[] when x <> null -> x |> Aardvark.Service.Pickler.json.UnPickleOfString |> Aardvark.Service.PathUtils.ofUnixStyle
    //            | _ -> failwithf "onSaveFile: %A" xs
    //    let mutable r = Unchecked.defaultof<_>
    //    w.Invoke(Action(fun _ -> r <- onEvent "onsave" [] cb )) |> ignore
    //    r

    let onChooseFiles (chosen : list<string> -> 'msg) =
        let cb xs =
            match xs with
                | [] -> chosen []
                | x::[] when x <> null -> x |> Aardvark.Service.Pickler.json.UnPickleOfString |> List.map Aardvark.Service.PathUtils.ofUnixStyle |> chosen
                | _ -> chosen []//failwithf "onChooseFiles: %A" xs
        onEvent "onchoose" [] cb   

    let onSaveFile (chosen : string -> 'msg) =
        let cb xs =
            match xs with
                | x::[] when x <> null -> x |> Aardvark.Service.Pickler.json.UnPickleOfString |> Aardvark.Service.PathUtils.ofUnixStyle |> chosen
                | _ -> chosen String.Empty //failwithf "onSaveFile: %A" xs
        onEvent "onsave" [] cb

    let onSaveFile1 (chosen : string -> 'msg) (path : Option<string>) =
      let cb xs =
        match path with
        | Some p-> p |> chosen
        | None ->
          match xs with
          | x::[] when x <> null -> 
            x |> Aardvark.Service.Pickler.json.UnPickleOfString |> Aardvark.Service.PathUtils.ofUnixStyle |> chosen
          | _ -> 
            String.Empty |> chosen
      onEvent "onsave" [] cb
          
module Double =
    let degreesFromRadians (d:float) =
        d.DegreesFromRadians()
        
    let radiansFromDegrees (d:float) =
        d.RadiansFromDegrees()
    
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
    open Aardvark.SceneGraph.SgPrimitives
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
                            
    let lines (points : alist<V3d>) (offset : IMod<float>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>) = 
        
        let edges = edgeLines false points trafo     
        edges
            |> Sg.lines color
            |> Sg.noEvents
            |> Sg.shader {
                 do! Shader.stableTrafo
                 do! DefaultSurfaces.vertexColor
                 do! Shader.thickLine    
             }                               
             |> Sg.trafo trafo
             |> Sg.uniform "LineWidth" width
             |> Sg.uniform "DepthOffset" offset
                        
    let scaledLines (points : alist<V3d>) (color : IMod<C4b>) (width : IMod<float>) (trafo : IMod<Trafo3d>) = 
        let edges = edgeLines false points trafo     
        let size =
            adaptive {
                let! line = edges
                return ((float (line.Length)) * 100.0)
            }
                                                            
        edges
            |> Sg.lines color
            |> Sg.noEvents
            |> Sg.uniform "WorldPos" (trafo |> Mod.map(fun (x : Trafo3d) -> x.Forward.C3.XYZ))
            |> Sg.uniform "Size" size
            |> Sg.shader {
               //  do! Shader.screenSpaceScale
                 do! Shader.stableTrafo
                 do! DefaultSurfaces.vertexColor
                 do! Shader.thickLine    
             }                               
             |> Sg.trafo trafo
             |> Sg.uniform "LineWidth" width
             |> Sg.uniform "DepthOffset" (Mod.constant 0.001)

    let cylinders positions = 
        positions |> Array.pairwise |> Array.map(fun (a,b) -> Line3d(a,b)) |> Array.map (fun x -> Cylinder3d(x, 0.02))    

    let pickable' (pick :IMod<Pickable>) (sg: ISg) =
        Sg.PickableApplicator (pick, Mod.constant sg)

    let getTriangles (pos : V3d[]) : array<Triangle3d> =
        let get (ti : int) =
            let i0 = 3 * ti
            Triangle3d(pos.[i0], pos.[i0 + 1], pos.[i0 + 2])

        Array.init (pos.Length / 3) get
         
    let mkDotISg color (size : IMod<float>) (pos : IMod<V3d>) = 
        Sg.sphere 2 color (Mod.constant 1.0)
            |> Sg.noEvents
            |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
            |> Sg.uniform "Size" size
            |> Sg.uniform "WorldPos" pos
        
    let dots (points : alist<V3d>) (size:IMod<float>) (color : IMod<C4b>) =
        aset {
            for p in points |> ASet.ofAList do
               // let scale = computeScale view (Mod.constant p) 5.0                               
                yield mkDotISg color (size) (Mod.constant p)
        } 
        |> Sg.set
        |> Sg.effect [
            toEffect <| Shader.screenSpaceScale
            toEffect <| Shader.stableTrafo
            toEffect <| DefaultSurfaces.vertexColor          
        ]

    ///probably move to a shader
    let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) =
        let right = up.Cross forw
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
    
    let text (view : IMod<CameraView>) near hfov pos modelTrafo text size =
         
      let invScaleTrafo = 
          computeInvariantScale view near pos size hfov |> Mod.map Trafo3d.Scale //(Mod.constant 0.05)
      
      let billboardTrafo = 
          adaptive {
              let! v = view
              let! modelt = modelTrafo
      
              return screenAligned v.Forward v.Up modelt
          }
      let bounds : Border2d = { left = 0.1; right = 0.1; top = 0.1; bottom = 0.1 }
      
      Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
          |> Sg.noEvents
          |> Sg.shader {
            do! Shader.stableTrafo
          }            
          |> Sg.trafo invScaleTrafo
          |> Sg.trafo billboardTrafo  

    
module UI =
    open Aardvark.UI
    open Aardvark.Base.Incremental

    let wrapToolTip (text:string) (dom:DomNode<'a>) : DomNode<'a> =
        //dom
        let attr = 
            [ attribute "title" text
              attribute "data-position" "top center"
              attribute "data-variation" "mini" ] 
                |> AttributeMap.ofList
                //|> AttributeMap.union dom.                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipRight (text:string) (dom:DomNode<'a>) : DomNode<'a> =
        //dom
        let attr = 
            [ attribute "title" text
              attribute "data-position" "right center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                //|> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipBottom (text:string) (dom:DomNode<'a>) : DomNode<'a> =
        //dom
        let attr = 
            [ attribute "title" text
              attribute "data-position" "bottom center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                //|> AttributeMap.union dom.Attributes                
                
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
    let set'   (lens : Lens<'s,'a>) (s:'s) (v : 'a)  : 's    = lens.Set(s,v)
    let update (lens : Lens<'s,'a>) (f : 'a->'a) (s:'s) : 's = lens.Update(s,f)

module Shader =
  
  open Aardvark.SceneGraph.Opc.Shader
  
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.Base.Rendering
  open Aardvark.SceneGraph
  open Aardvark.Rendering
  open Aardvark.UI
  open Aardvark.UI.Trafos
  open Aardvark.UI.Primitives
  open PRo3D
  open FShade


  type SuperVertex = 
        {
            [<Position>] pos :  V4d
            [<SourceVertexIndex>] i : int
        }

  let lines (t : Triangle<SuperVertex>) =
        line {
            yield t.P0
            yield t.P1
            restartStrip()
            
            yield t.P1
            yield t.P2
            restartStrip()

            yield t.P2
            yield t.P0
            restartStrip()
        }
  let private colormap =
        sampler2d {
            texture uniform?ColorMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
  }

  let falseColor (v : Vertex) =
      fragment {           
          if uniform?falseColors then
              let range : V2d = uniform?MinMax
              let norm = V2d((v.scalar - range.X)/ (range.Y - range.X), 0.5)

              let c = colormap.Sample(norm).XYZ

              return v.c * V4d(c.X, c.Y, c.Z, 1.0)
          else
              return v.c
      }

  [<ReflectedDefinition>]
  let hsv2rgb (h : float) (s : float) (v : float) =
    let h = Fun.Frac(h)
    let chr = v * s
    let x = chr * (1.0 - Fun.Abs(Fun.Frac(h * 3.0) * 2.0 - 1.0))
    let m = v - chr
    let t = (int)(h * 6.0)
    match t with
        | 0 -> V3d(chr + m, x + m, m)
        | 1 -> V3d(x + m, chr + m, m)
        | 2 -> V3d(m, chr + m, x + m)
        | 3 -> V3d(m, x + m, chr + m)
        | 4 -> V3d(x + m, m, chr + m)
        | 5 -> V3d(chr + m, m, x + m)
        | _ -> V3d(chr + m, x + m, m)

  [<ReflectedDefinition>]
  let mapFalseColors value : float =         
      let invert           = uniform?inverted
      let fcUpperBound     = uniform?upperBound
      let fcLowerBound     = uniform?lowerBound
      let fcInterval       = uniform?interval
      let fcUpperHueBound  = uniform?endC
      let fcLowerHueBound  = uniform?startC

      let low         = if (invert = false) then fcLowerBound else fcUpperBound
      let up          = if (invert = false) then fcUpperBound else fcLowerBound
      let interval    = if (invert = false) then fcInterval   else -1.0 * fcInterval        

      let rangeValue = up - low + interval
      let normInterv = (interval / rangeValue)

      //map range to 0..1 according to lower/upperbound
      let k = (value - low + interval) / rangeValue

      //discretize lookup
      let bucket = floor (k / normInterv)
      let k = ((float) bucket) * normInterv |> clamp 0.0 1.0

      let uH = fcUpperHueBound * 255.0
      let lH = fcLowerHueBound * 255.0
      //map values to hue range
      let fcHueUpperBound = if (uH < lH) then uH + 1.0 else uH
      let rangeHue = uH - lH // fcHueUpperBound - lH
      (k * rangeHue) + lH

  [<ReflectedDefinition>]
  let mapFalseColors2 value : float =
        let fcInterval     = uniform?interval
        let startColorHue     = uniform?startC
        let endColorHue       = uniform?endC
        let invertMapping  = uniform?inverted
        let fcUpperBound   = uniform?upperBound
        let fcLowerBound   = uniform?lowerBound

        let range = fcUpperBound - fcLowerBound
        let numOfRangeGaps = int(round (range / fcInterval)) //round
        let numOfStops = if (numOfRangeGaps + 2) > 100 then 100 else (numOfRangeGaps + 2)

        let startColor = startColorHue *255.0
        let endColor = endColorHue *255.0
        let hStepSize =
            if startColor < endColor then 
                (endColor - startColor) / ((float)(numOfStops-1))
            else 
                ((endColor + 1.0) - startColor) / ((float)(numOfStops-1))
                
        let pos = 
            match fcLowerBound < value with
                    | true -> (int ( round(value - fcLowerBound) / fcInterval )) //+1//round
                    | _ -> 0
        let pos1 = 
            match fcUpperBound < value with
                | true -> numOfRangeGaps
                | _ -> pos
                     
        let currColorH = 
            if invertMapping then
                (endColor - (hStepSize * ((float)(pos1))))
            else 
                (startColor + (hStepSize * ((float)(pos1))))
        currColorH
            
  let falseColorLegend2 (v : Vertex) =
      fragment {    

          if (uniform?falseColors) 
          then
              let hue = mapFalseColors2 v.scalar
              let c = hsv2rgb ((clamp 0.0 255.0 hue)/ 255.0 ) 1.0 1.0 // 
              return v.c * V4d(c.X, c.Y, c.Z, 1.0)
          else
              return v.c
      }
        
  [<ReflectedDefinition>]
  let myTrunc (value : float) =
    clamp 0.0 255.0 value
  
  let mapContrast (v : Vertex) =
    fragment { 
        if (uniform?useContrastS) then
            let c = uniform?contrastS
            let nc = V4d(v.c.X*255.0, v.c.Y*255.0,v.c.Z*255.0, 255.0)
            
            let factor = (259.0 * (c + 255.0)) / (255.0 * (259.0 - c))
            let red    = (myTrunc (factor * (nc.X   - 128.0) + 128.0)) / 255.0
            let green  = (myTrunc (factor * (nc.Y   - 128.0) + 128.0)) / 255.0
            let blue   = (myTrunc (factor * (nc.Z   - 128.0) + 128.0)) / 255.0
            return V4d(red, green, blue, 1.0)
        else
            return v.c
    }

  let mapBrightness (v : Vertex) = 
    fragment { 
        if (uniform?useBrightnS) then
            let b = uniform?brightnessS
            let nc = V4d(v.c.X*255.0, v.c.Y*255.0,v.c.Z*255.0, 255.0)        
            
            let red   = (myTrunc(nc.X + b)) / 255.0 
            let green = (myTrunc(nc.Y + b)) / 255.0 
            let blue  = (myTrunc(nc.Z + b)) / 255.0 
            return V4d(red, green, blue, 1.0)
        else
            return v.c
    }

  let mapGamma (v : Vertex) = 
    fragment { 
        if (uniform?useGammaS) then
            let g = uniform?gammaS
            let gammaCorrection = 1.0 / g
            return V4d(v.c.X**gammaCorrection, v.c.Y**gammaCorrection, v.c.Z**gammaCorrection, 1.0)
        else
            return v.c
    }

  let grayscale (v : Vertex) = 
    fragment { 
        if (uniform?useGrayS) then
            let value = v.c.X * 0.299 + v.c.Y * 0.587 + v.c.Z * 0.114
            return V4d(value, value, value, 1.0)
        else
            return v.c
    }

  let addColor (v : Vertex) = 
    fragment { 
        if (uniform?useColorS) then
            let col : V3d =  uniform?colorS
            let nc = V4d(v.c.X*255.0, v.c.Y*255.0, v.c.Z*255.0, 255.0)

            let red   = (myTrunc( nc.X * col.X)) / 255.0
            let green = (myTrunc( nc.Y * col.Y)) / 255.0
            let blue  = (myTrunc( nc.Z * col.Z)) / 255.0
            return V4d(red, green, blue, 1.0)
        else
            return v.c
    }

  let thickLine2 (line : Line<ThickLineVertex>) =
     triangle {
         let offset : float = uniform?depthOffset
         let t = uniform.LineWidth                                 
                      
         let sizeF = V3d(float uniform.ViewportSize.X, float uniform.ViewportSize.Y, 1.0)
         
         let pp0 = line.P0.pos
         let pp1 = line.P1.pos
         let middle = (pp0 + pp1) / 2.0
         let viewVec = ((middle.XYZ - uniform.CameraLocation).Normalized)
         let viewVec = V4d(viewVec.X, viewVec.Y, viewVec.Z, 1.0)

         let pp0 = pp0 + viewVec * offset
         let pp1 = pp1 + viewVec * offset
                                         
         //clipping
         let pp0 = if pp0.Z < 0.0 then (Lerp pp1 pp0 (pp1.Z / (pp1.Z - pp0.Z))) else pp0
         let pp1 = if pp1.Z < 0.0 then (Lerp pp0 pp1 (pp0.Z / (pp0.Z - pp1.Z))) else pp1            
                 
         let p0 = pp0.XYZ / pp0.W
         let p1 = pp1.XYZ / pp1.W
                    
         let fwp = (p1.XYZ - p0.XYZ) * sizeF
    
         let fw = V3d(fwp.XY * 2.0, 0.0) |> Vec.normalize
         let r = V3d(-fw.Y, fw.X, 0.0) / sizeF
         let d = fw / sizeF

        // let k = (d.Normalized |> Vec.cross r.Normalized).Normalized * offset      

         let p00 = p0.XYZ - r * t - d * t //+ k
         let p10 = p0.XYZ + r * t - d * t //+ k
         let p11 = p1.XYZ + r * t + d * t //+ k
         let p01 = p1.XYZ - r * t + d * t //+ k
    
         let rel = t / (Vec.length fwp)
    
         yield { line.P0 with pos = V4d(p00, 1.0); lc = V2d(-1.0, -rel); w = rel }
         yield { line.P0 with pos = V4d(p10, 1.0); lc = V2d( 1.0, -rel); w = rel }
         yield { line.P1 with pos = V4d(p01, 1.0); lc = V2d(-1.0, 1.0 + rel); w = rel }
         yield { line.P1 with pos = V4d(p11, 1.0); lc = V2d( 1.0, 1.0 + rel); w = rel }
     }

module PRo3DNumeric = 
    open Aardvark.Base.Incremental
    open Aardvark.UI

    let inline (=>) a b = Attributes.attribute a b

    type Action = 
        | SetValue of float
        | SetMin of float
        | SetMax of float
        | SetStep of float
        | SetFormat of string

    let update (model : NumericInput) (action : Action) =
        match action with
        | SetValue v -> { model with value = v }
        | SetMin v ->   { model with min = v }
        | SetMax v ->   { model with max = v }
        | SetStep v ->  { model with step = v }
        | SetFormat s -> { model with format = s }

    let formatNumber (format : string) (value : float) =
        String.Format(Globalization.CultureInfo.InvariantCulture, format, value)

    let numericField<'msg> ( f : Action -> seq<'msg> ) ( atts : AttributeMap<'msg> ) ( model : MNumericInput ) inputType =         

        let tryParseAndClamp min max fallback s =
            let parsed = 0.0
            match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                | (true,v) -> clamp min max v
                | _ ->  printfn "validation failed: %s" s
                        fallback

        let onWheel' (f : Aardvark.Base.V2d -> seq<'msg>) =
            let serverClick (args : list<string>) : Aardvark.Base.V2d = 
                let delta = List.head args |> Pickler.unpickleOfJson
                delta  / Aardvark.Base.V2d(-100.0,-100.0) // up is down in mouse wheel events

            onEvent' "onwheel" ["{ X: event.deltaX.toFixed(10), Y: event.deltaY.toFixed(10)  }"] (serverClick >> f)

        let attributes = 
            amap {                
                yield style "text-align:right; color : black"                

                let! min = model.min
                let! max = model.max
                let! value = model.value
                match inputType with
                    | Slider ->   
                        yield "type" => "range"
                        yield onChange' (tryParseAndClamp min max value >> SetValue >> f)   // continous updates for slider
                    | InputBox -> 
                        yield "type" => "number"
                        yield onChange' (tryParseAndClamp min max value >> SetValue >> f)  // batch updates for input box (to let user type)

                let! step = model.step
                yield onWheel' (fun d -> value + d.Y * step |> clamp min max |> SetValue |> f)

                yield "step" => sprintf "%f" step
                yield "min"  => sprintf "%f" min
                yield "max"  => sprintf "%f" max

                let! format = model.format
                yield "value" => formatNumber format value
            } 

        Incremental.input (AttributeMap.ofAMap attributes |> AttributeMap.union atts)

    let numericField' = numericField (Seq.singleton) AttributeMap.empty

    let view' (inputTypes : list<NumericInputType>) (model : MNumericInput) : DomNode<Action> =
        inputTypes 
            |> List.map (numericField' model) 
            |> List.intersperse (text " ") 
            |> div []

    let view (model : MNumericInput) =
        view' [InputBox] model

  