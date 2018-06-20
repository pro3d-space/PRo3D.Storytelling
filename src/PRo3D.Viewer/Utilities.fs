namespace PRo3D

#nowarn "8989"

open System
open System.IO
open System.Windows.Forms

open MBrace.FsPickler
open MBrace.FsPickler.Json
open Aardvark.Base
open PRo3D
open Aardvark.SceneGraph.Opc

module Serialization =
    open Aardvark.UI.Incremental
    
    let registry = new CustomPicklerRegistry()    
    let cache = PicklerCache.FromCustomPicklerRegistry registry    

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
             |> Sg.uniform "DepthOffset" (Mod.constant 0.2)
            
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

    let mkDotISg color size (pos : IMod<V3d>) = 
        Sg.sphere 2 color (Mod.constant 1.0)
            |> Sg.noEvents
            |> Sg.trafo(pos |> Mod.map Trafo3d.Translation)
            |> Sg.uniform "Size" size
            |> Sg.uniform "WorldPos" pos
        
    let dots (points : alist<V3d>) (color : IMod<C4b>) =
        aset {
            for p in points |> ASet.ofAList do
               // let scale = computeScale view (Mod.constant p) 5.0                               
                yield mkDotISg color (Mod.constant 5.0) (Mod.constant p)
        } 
        |> Sg.set
        |> Sg.effect [
            toEffect <| Shader.screenSpaceScale
            toEffect <| Shader.stableTrafo
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

    let wrapToolTipBottom (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "bottom center"
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

module Shader =
  open FShade
  open Aardvark.SceneGraph.Opc.Shader

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
    let s = clamp 0.0 1.0 s
    let v = clamp 0.0 1.0 v

    let h = h % 1.0
    let h = if h < 0.0 then h + 1.0 else h
    let hi = floor ( h * 6.0 ) |> int
    let f = h * 6.0 - float hi
    let p = v * (1.0 - s)
    let q = v * (1.0 - s * f)
    let t = v * (1.0 - s * ( 1.0 - f ))
    match hi with
        | 1 -> V3d(q,v,p)
        | 2 -> V3d(p,v,t)
        | 3 -> V3d(p,q,v)
        | 4 -> V3d(t,p,v)
        | 5 -> V3d(v,p,q)
        | _ -> V3d(v,t,p)

  
  let falseColorLegend (v : Vertex) =
      fragment {           
          if uniform?falseColors then
              let sC : V3d = uniform?startC
              let eC : V3d = uniform?endC
              let interval : float = uniform?interval
              let lowerBound : float = uniform?lowerBound
              let upperBound : float = uniform?upperBound
              let stepS : float = uniform?stepS
              let numOfRG : float = uniform?numOfRG
              let inverted : bool = uniform?inverted

              let pos = if lowerBound < v.scalar then
                         (round ( (v.scalar - lowerBound) / interval ))
                        else 0.0
                        
              let pos1 = if upperBound < v.scalar then
                            numOfRG
                         else pos
                     
              let p = 
                  if uniform?inverted then
                    eC.X - (stepS * pos1)
                  else 
                    sC.X + (stepS * pos1)
              
              let norm = V2d(p, 0.5)
              let c = hsv2rgb p 0.0 0.0 //colormap.Sample(norm).XYZ

              return v.c * V4d(c.X, c.Y, c.Z, 1.0)
              //return v.c
          else
              return v.c
      }

  [<ReflectedDefinition>]
  let mapFalseColors value : float =         
      let invert = uniform?fcInvert
      let fcUpperBound = uniform?falseColorUpperBound
      let fcLowerBound = uniform?falseColorLowerBound
      let fcInterval = uniform?interval
      let fcUpperHueBound = uniform?fcHueUpperBound
      let fcLowerHueBound = uniform?fcHueLowerBound

      let low         = if (invert = false) then fcLowerBound else fcUpperBound
      let up          = if (invert = false) then fcUpperBound else fcLowerBound
      let interval    = if (invert = false) then fcInterval   else -1.0 * fcInterval        

      let  rangeValue = up - low + interval
      let  normInterv = (interval / rangeValue)

      //map range to 0..1 according to lower/upperbound
      let k = (value - low + interval) / rangeValue

      //discretize lookup
      let bucket = floor (k / normInterv)
      let k = ((float) bucket) * normInterv |> clamp 0.0 1.0

      //map values to hue range
      let fcHueUpperBound = if (fcUpperHueBound < fcLowerHueBound) then fcUpperHueBound + 1.0 else fcUpperHueBound
      let rangeHue = fcHueUpperBound - fcLowerHueBound
      (k * rangeHue) + fcLowerHueBound

  let falseColor2 (v : Vertex) =
      fragment {    

          if (uniform?useFalseColors) 
          then
              let mapping = mapFalseColors v.scalar
              let mappingV2d = V2d(mapping, 0.5)
              let c = colormap.Sample(mappingV2d).XYZ
              return v.c *  V4d(c.X, c.Y, c.Z, 1.0)
          else
              return v.c
      }

  //let stableTrafoBigKoords (v : Vertex) =
  //      vertex {
  //          let vp = uniform.ModelViewTrafo * v.pos
  //          let wp = uniform.ModelTrafo * v.pos
  //          return { 
  //              v with
  //                  pos  = uniform.ProjTrafo * vp
  //                  wp   = wp
  //                  n    = transformNormal v.n
  //                  ldir = V3d.Zero - vp.XYZ |> Vec.normalize
  //          } 
  //      }

  [<ReflectedDefinition>]
  let myTrunc (value : float) =
    if value < 0.0 then
        0.0
    elif value > 255.0 then
        255.0
    else
        value

  [<ReflectedDefinition>]
  let mapContrast (vColor : V4d) =
    let c = uniform?contrastS
    let nc = V4d(vColor.X*255.0, vColor.Y*255.0,vColor.Z*255.0, 255.0)

    let factor = (259.0 * (c + 255.0)) / (255.0 * (259.0 - c))
    let red = (myTrunc (factor * (nc.X   - 128.0) + 128.0)) / 255.0
    let green = (myTrunc (factor * (nc.Y   - 128.0) + 128.0)) / 255.0
    let blue = (myTrunc (factor * (nc.Z   - 128.0) + 128.0)) / 255.0
    V4d(red, green, blue, 1.0)

  [<ReflectedDefinition>]
  let mapBrightness (vColor : V4d) =
    let b = uniform?brightnessS
    let nc = V4d(vColor.X*255.0, vColor.Y*255.0,vColor.Z*255.0, 255.0)
    
    let red = (myTrunc(nc.X + b)) / 255.0 
    let green = (myTrunc(nc.Y + b)) / 255.0 
    let blue  = (myTrunc(nc.Z  + b)) / 255.0 
    V4d(red, green, blue, 1.0)

  [<ReflectedDefinition>]
  let mapGamma (vColor : V4d) =
    let g = uniform?gammaS
    let gammaCorrection = 1.0 / g
    V4d(vColor.X**gammaCorrection, vColor.Y**gammaCorrection, vColor.Z**gammaCorrection, 1.0)

  [<ReflectedDefinition>]
  let grayscale (vColor : V4d) =
    let value = vColor.X * 0.299 + vColor.Y * 0.587 + vColor.Z * 0.114
    V4d(value, value, value, 1.0)

  [<ReflectedDefinition>]
  let addColor (vColor : V4d) =
    let col : V3d =  uniform?colorS
    let nc = V4d(vColor.X*255.0, vColor.Y*255.0,vColor.Z*255.0, 255.0)

    let red = (myTrunc( nc.X * col.X))/255.0
    let green = (myTrunc( nc.Y * col.Y ))/255.0
    let blue = (myTrunc( nc.Z * col.Z )) / 255.0
    V4d(red, green, blue, 1.0)



  let imageCorrection (v : Vertex) =
      fragment {
        let contrast = 
          if (uniform?useContrastS) 
          then
              (mapContrast v.c) 
          else
              v.c

        let brightn = 
            if (uniform?useBrightnS) 
              then
                 (mapBrightness contrast) // contrast * 
              else
                  contrast

        let gamma = 
            if (uniform?useGammaS) 
              then
                  (mapGamma brightn) 
              else
                  brightn

        let grey = 
            if (uniform?useGrayS) 
              then
                  (grayscale gamma) 
              else
                  gamma

        if (uniform?useColorS) 
          then
              return addColor grey
          else
              return grey


      }
