namespace PRo3D.Surfaces

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
open Aardvark.SceneGraph

open PRo3D

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =
    open Aardvark.SceneGraph.SgFSharp.Sg

    let getSurface model guid =
        model.surfaces |> PList.toList |> List.tryFind (fun a -> a.guid = guid)

    let updateSingleSurface model (s': PRo3D.Surfaces.Surface)  =
        let surfaces' = 
            model.surfaces 
                |> PList.toList 
                |> List.updateIf (fun x -> x.guid = s'.guid) (fun x -> s') 
                |> PList.ofList

        // update sgsurfaces as well !!!
        let sgSurfaces' = 
            model.sgSurfaces 
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)   

        { model with surfaces = surfaces'; sgSurfaces = sgSurfaces' }

    let updateSingleSurface' model (s':PRo3D.Surfaces.Surface)  =
        let surfaces' = 
            model.surfaces 
                |> PList.toList 
                |> List.updateIf (fun x -> x.guid = s'.guid) (fun x -> s') 
                |> PList.ofList

        // update sgsurfaces as well !!!
        let sgSurfaces' = 
            model.sgSurfaces 
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)   

        // update sggroup as well !!!
        let group' = 
            model.sgGrouped
              |> PList.map(fun surfs ->
                surfs
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)
                )

        { model with surfaces = surfaces'; sgSurfaces = sgSurfaces'; sgGrouped = group' }

    let updateSurfaces model (surfaces' : plist<PRo3D.Surfaces.Surface>) =
        let sgSurfaces' =
            surfaces' 
                |> PList.toList
                |> List.map (
                    fun x -> 
                        let update = 
                            (fun (d : Option<SgSurface>) ->
                                match d with 
                                    | Some k -> Some { k with surface = x }
                                    | None -> None)

                        HMap.alter x.guid update model.sgSurfaces ) 
                |> List.fold (fun a b -> HMap.union a b) HMap.empty

        { model with surfaces = surfaces'; sgSurfaces = sgSurfaces' }

    let groupSurfaces (sgSurfaces : hmap<Guid, SgSurface>) =
        sgSurfaces
            |> HMap.toList
            |> List.groupBy(fun (g,x) -> x.surface.priority.value)
            |> List.map( fun (p,k) -> (p, k |> HMap.ofList))
            |> List.sortBy fst
            |> List.map snd
            |> PList.ofList

    let updateGrouping' (model:SurfaceModel) (s':PRo3D.Surfaces.Surface) =
        let group' = 
            model.sgGrouped
              |> PList.map(fun surfs ->
                surfs
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)
                )
        { model with sgGrouped = group' }

    module SurfaceAttributes = 
        open System.Xml
            
        let get (name : string) (node : XmlNode)=
            node.SelectSingleNode(name).InnerText.Trim()

        let parseMap (index : int)(node : XmlNode) : ScalarLayer =        
            { 
                label        = node |> get "Label" 
                actualRange  = node |> get "ChannelsActualRange"  |> Range1d.Parse
                definedRange = node |> get "ChannelsDefinedRange" |> Range1d.Parse
                index        = index
            }

        let parseTexture (index : int)(node : XmlNode) : TextureLayer =        
            { 
                label = node |> get "Label" 
                index = index
            }

        let parseLayer (index : int)(node : XmlNode) : AttributeLayer = 
            let typ = node |> get "Type"
            match typ with
                | "Texture" -> TextureLayer (parseTexture index node)
                | "Map"     -> ScalarLayer (parseMap index node)
                | _         -> failwith "type not supported"

        let layers (doc : XmlDocument) =
            let nodes = doc.SelectNodes "/Aardvark/SurfaceAttributes/AttributeLayers/AttributeLayer"
            nodes
             |> Seq.cast<XmlNode>
             |> Seq.mapi parseLayer

        let read (path:string) =
            let doc = new XmlDocument() in doc.Load path
            doc |> layers
    
    module Files = 

        /// checks if "path" is a valid opc folder
        let isOPCPath (path : string) = 
            let imagePath = Path.combine [path; "images"]
            let patchPath = Path.combine [path; "patches"]
            let dirsExist = (System.IO.Directory.Exists imagePath) && (System.IO.Directory.Exists patchPath)
            let storageExists = System.IO.File.Exists(patchPath + "\\patchhierarchy.xml")
            if dirsExist && storageExists then
                true
            else false

        /// checks if "path" is a valid surface folder
        let isSurfaceFolder (path : string) =
            let mutable isValid = false
            let subfolders = System.IO.Directory.GetDirectories(path)
            for s in subfolders do
                isValid <- (isOPCPath(s) || isValid)
            isValid
                    
        /// returns all valid OPCs in "path"
        let getOPCPaths path = 
            if path = String.Empty then failwith "encountered empty opcpath"

            if isOPCPath path then
                [path.Replace(@"\", @"\\\\")]
            elif System.IO.Directory.Exists path then
                System.IO.Directory.GetDirectories(path)
                    |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                    |> Seq.choose (fun a -> if isOPCPath a then Some a else None)
                    |> Seq.toList 
            else []

        /// returns all valid OPCs in "path"
        let getOPCNames path = 
            if path = String.Empty then failwith "encountered empty opcpath"

            if isOPCPath path then
                [path.Replace(@"\", @"\\\\")]
            elif System.IO.Directory.Exists path then
                System.IO.Directory.GetDirectories(path)
                    |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                    |> Seq.choose (fun a -> if isOPCPath a then Some a else None)
                    |> Seq.map (fun a -> System.IO.Path.GetFileName a; )
                    |> Seq.toList 
            else []

        let expandNamesToPaths (path : string) (names : List<string>) =
            names |> List.map(fun a -> 
                let p = (Path.combine [path; a]).Replace(@"\", @"\\\\")
                //Log.warn "%s" p
                p)

        /// returns all valid surface folders in "path"   
        let discover path = 
            if System.IO.Directory.Exists path then
                System.IO.Directory.EnumerateDirectories path 
                    // |> Seq.map (fun a -> a.Replace(@"\", @"\\\\")) 
                    |> Seq.filter (fun a -> isSurfaceFolder a)
                    |> Seq.toList 
            else []

        let getSurfaceFolder (surface:PRo3D.Surfaces.Surface) (scenePath:option<string>) =
            if surface.relativePaths 
            then
                match scenePath with
                    | Some sp -> 
                        let x = Path.Combine((Path.GetDirectoryName sp),"Surfaces")
                        let relPath = Path.Combine(x, surface.name)
                        if Directory.Exists relPath then Some relPath else None
                    | None -> None
            else
                let p = surface.importPath
                if Directory.Exists p then Some p else None

        let expandLazyKdTreePaths (scenePath : option<string>) (surfaces : hmap<Guid, SgSurface>)=
            match scenePath with
                | Some sp -> 
                    surfaces |> HMap.map (fun _ s -> 
                        let path = Path.Combine((Path.GetDirectoryName sp),"Surfaces")
                        let kd =
                            s.kdTree |> HMap.map (fun _ k -> 
                                match k with 
                                    | Level0KdTree.LazyKdTree lk when s.surface.relativePaths -> 
                                        let kdTreeSub =
                                            lk.kdtreePath.Split(Path.DirectorySeparatorChar) 
                                                |> List.ofArray |> List.rev |> List.take 5 |> List.rev |> Path.combine  
                                        let triangleSub =
                                            lk.objectSetPath.Split(Path.DirectorySeparatorChar) 
                                                |> List.ofArray |> List.rev |> List.take 5 |> List.rev |> Path.combine 
                                        let trianglePath' = Path.Combine(path, triangleSub)
                                        let kdtreePath' = Path.Combine(path, kdTreeSub)
                                        LazyKdTree { lk with kdtreePath = kdtreePath'; objectSetPath = trianglePath' }
                                    | Level0KdTree.LazyKdTree lk -> LazyKdTree lk
                                    | Level0KdTree.InCoreKdTree ik -> InCoreKdTree ik
                            )
                        { s with kdTree = kd }
                    )
                | None -> surfaces

        let makeSurfaceRelative guid (surfaceModel : SurfaceModel) (scenePath : option<string>) =
            match scenePath with
                | Some sp ->
                    let surf = getSurface surfaceModel guid
                    match surf with
                        | Some s ->
                            // create target dir
                            let targetSurfaceDir = Path.Combine((Path.GetDirectoryName sp),"Surfaces")
                            if not (Directory.Exists targetSurfaceDir) then
                                Directory.CreateDirectory(targetSurfaceDir) |> ignore

                            // copy data
                            let targetDir = Path.Combine(targetSurfaceDir, s.name)
                            Copy.copyAll s.importPath targetDir true

                            // update opc paths and scene
                            let s' = { s with opcPaths = expandNamesToPaths targetDir s.opcNames; relativePaths = true }
                            updateSingleSurface surfaceModel s'
                    
                        | None ->
                            Log.error "surface %s not found" (guid.ToString())
                            surfaceModel
                | None -> 
                    Log.error "can't make surface relative. not valid scene path has not been saved yet."
                    surfaceModel

    let assertInvalidBB (bb:Box3d) = 
        if(bb.IsInvalid) then 
            failwith (sprintf "invalid bounding box %s" (bb.ToString()))
        else bb
   
    /// creates a surface from opc folder path
    let mk path = 
        
        let names = Files.getOPCNames path                
        {
            guid            = Guid.NewGuid()
            name            = IO.Path.GetFileName path
            importPath      = path
            opcNames        = names
            opcPaths        = names |> Files.expandNamesToPaths path
            fillMode        = FillMode.Fill
            cullMode        = CullMode.None
            isVisible       = true
            relativePaths   = false
            quality         = Init.quality
            priority        = Init.priority
            trafo           = Trafo3d.Identity

            scalarLayers    = PList.empty
            selectedScalar  = None

            textureLayers   = PList.empty
            selectedTexture = None            
        }
    
    let combineLeafBBs (hierarchies : list<PatchHierarchy>) =
         hierarchies
            |> List.map(fun d -> d.tree |> QTree.getLeaves)                  
            |> Seq.map (fun p -> p |> Seq.map(fun d -> assertInvalidBB d.info.GlobalBoundingBox)) 
            |> Seq.concat
            |> Box3d.ofSeq

    let combineRootBBs (hierarchies : list<PatchHierarchy>) =
         hierarchies
            |> List.map(fun d -> d.tree |> QTree.getRoot)                  
            |> Seq.map (fun p -> assertInvalidBB p.info.GlobalBoundingBox)
            |> Box3d.ofSeq

    let sgOfSurface (s:PRo3D.Surfaces.Surface) sg (bb:Box3d) (kd:hmap<Box3d,Level0KdTree>) = {
        surface = s       
        sceneGraph = sg
        globalBB = bb
        kdTree = kd
    }

    let transformBox (trafo:Trafo3d) (bb:Box3d) =
        bb.Transformed(trafo)
      
    let createSgSurfaces runtime signature surfaces =
        surfaces          
            |> PList.toList
            |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths; preTransform = d.trafo })
            |> List.map (fun d -> Viewer.createPlainSceneGraph runtime signature d true)
            |> List.zip (surfaces |> PList.toList)
            |> List.map (
                fun (surf, (sg, hierarchies, kdtree)) -> 
                    let bb = hierarchies |> combineLeafBBs |> transformBox surf.trafo
                    sgOfSurface surf sg bb kdtree)
            |> List.map (fun d -> (d.surface.guid, d))
            |> HMap.ofList
                  
 module SurfaceApp =
    open System.Diagnostics
    open Aardvark.Rendering.GL.Compiler

    open PRo3DCompability    
    open Aardvark.VRVis
    
    type Action =
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | SelectSurface             of Guid
        | FlyToSurface              of Guid
        | MakeRelative              of Guid
        | RemoveSurface             of Guid
        | PickSurface               of SceneHit*string
        | OpenFolder                of Guid

    type Model = SurfaceModel

    let triangleIsNan (t:Triangle3d) =
        t.P0.AnyNaN || t.P1.AnyNaN || t.P2.AnyNaN

    let getInvalidIndices (positions : V3d[]) =
        positions |> List.ofArray |> List.mapi (fun i x -> if x.AnyNaN then Some i else None) |> List.choose id

    let loadTriangles (kd : LazyKdTree) = 
        
        let positions = kd.objectSetPath |> Aara.fromFile<V3f>

        let data = 
            positions.Data |> Array.map (fun x ->  x.ToV3d() |> kd.affine.Forward.TransformPos)

        let invalidIndices = getInvalidIndices data
        let index = PRo3DCSharp.ComputeIndexArray(positions.Size.XY.ToV2i(), invalidIndices)                
                
        let triangles =             
            index 
                |> Seq.map(fun x -> data.[x])
                |> Seq.chunkBySize 3                               
                |> Seq.map(fun x -> Triangle3d(x))
                |> Seq.filter(fun x -> triangleIsNan x |> not) |> Seq.toArray
                |> TriangleSet

        triangles

    let loadObjectSet (cache : hmap<string, ConcreteKdIntersectionTree>) (lvl0Tree : Level0KdTree)=       
    
        match lvl0Tree with
            | InCoreKdTree kd -> 
                kd.kdTree, cache
            | LazyKdTree kd -> 
            
                let kdTree, cache =
                    match kd.kdTree with
                        | Some k -> k, cache
                        | None -> 
                            let tree = cache |> HMap.tryFind (kd.boundingBox.ToString())
                            match tree with
                                | Some t -> 
                                    //Log.line "cache hit %A" kd.boundingBox
                                    t, cache
                                | None ->                                     
                                    Log.line "cache miss %A- loading kdtree" kd.boundingBox

                                    let mutable tree = KdTrees.loadKdtree kd.kdtreePath                                    
                                    tree.KdIntersectionTree.ObjectSet <- (kd |> loadTriangles)

                                    let key = tree.KdIntersectionTree.BoundingBox3d.ToString()
                                                                        
                                    tree, (HMap.add key tree cache)
                kdTree, cache

    let intersectKdTrees (kdTreeMap: hmap<Box3d, Level0KdTree>) bb (surface : PRo3D.Surfaces.Surface) (cache : hmap<string, ConcreteKdIntersectionTree>) (ray : FastRay3d) = 

        let kdtree, c = kdTreeMap |> HMap.find bb |> loadObjectSet cache        

        let kdi = kdtree.KdIntersectionTree 
        let mutable hit = ObjectRayHit.MaxRange
                
        let objFilter a b = true
        let hitFilter a b c d = false
        if kdi.Intersect(ray, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then 
            let point = ray.Ray.GetPointOnRay(hit.RayHit.T)            
            Some (hit.RayHit.T, surface),c
        else            
            None,c

    let doKdTreeIntersection (m : Model) (t : Trafo3d) (r : FastRay3d) cache = 
        let mutable cache = cache

        //let stuffs = 
        //    m.sgGrouped |> PList.rev |> PList.map(fun x -> x |> HMap.map(fun _ b -> Log.warn "%A" b.surface.priority.value)) |> ignore

        let kdSurfPairs =
            m.sgSurfaces
            |> HMap.toSeq
            |> Seq.map (fun (_,d) -> d.kdTree, d.surface) 
            |> Seq.sortByDescending(fun (_,s) -> s.priority)

        let mutable lastPriority = Int32.MaxValue
        let mutable hit : option<float*Surface> = None

        for (kd ,surf) in kdSurfPairs do 
            Log.error "stuff %A" surf.priority.value
            let priorityChanged = (int surf.priority.value) = lastPriority

            let h =
                if kd.IsEmpty then 
                    Log.error "no kdtree loaded for %s" surf.name
                    None
                else
                    None

            hit <- h
            lastPriority <- (int surf.priority.value)            

        m.sgSurfaces
            |> HMap.toSeq
            |> Seq.map (fun (_,d) -> d.kdTree, d.surface) 
            |> Seq.map (
                fun (kd ,surf) ->
                    if kd.IsEmpty then 
                        Log.error "no kdtree loaded for %s" surf.name
                        None
                    else                        
                        //get bbs that are hit
                        let hitBoxes =
                            kd  |> HMap.toSeq 
                                |> Seq.map fst
                                |> Seq.filter(
                                    fun x -> 
                                       let mutable t = 0.0
                                       let r' = r.Ray.Transformed(surf.trafo.Backward)
                                       x.Intersects(r', &t)
                                )                                                
                        //intersect corresponding kdtrees
                        let closestHit =
                            hitBoxes
                                |> Seq.map(
                                    fun bb -> 
                                        Log.line "intersection: %s; pr: %f" surf.name surf.priority.value                                        

                                        let hit, c = 
                                            r.Ray.Transformed(surf.trafo.Backward)
                                            |> FastRay3d 
                                            |> intersectKdTrees kd bb surf cache

                                        cache <- c
                                        hit
                                        )
                                |> Seq.choose(fun x -> x)
                                |> Seq.sortBy(fun (t,_)-> t)
                                |> Seq.tryHead                                               

                        closestHit
                )
            |> Seq.choose(fun d -> d)
            |> Seq.sortByDescending(fun (_,surf) -> surf.priority.value)
            |> Seq.tryHead, cache
    
    let updateGrouping (model:SurfaceModel) =
        { model with sgGrouped = (Surface.groupSurfaces model.sgSurfaces) }

    let update (model:SurfaceModel) (action:Action) (scenePath : option<string>) = 
        match action with
            | SurfacePropertiesMessage msg ->
                let m =
                    match model.selectedSurface with 
                        | Some s -> 
                            let s' = SurfaceProperties.update s msg 
                            let model = Surface.updateSingleSurface' model s'
                            { model with selectedSurface = Some s' }
                        | None -> model

                match msg with
                  | SurfaceProperties.SetPriority _ -> m |> updateGrouping 
                  | _ -> m
                
            | SelectSurface id -> 
                let surf = model.surfaces |> Seq.tryFind(fun x -> x.guid = id)
                let surf =
                    match surf, model.selectedSurface with
                        | Some a, Some b -> if a.guid = b.guid then None else Some a
                        | Some a, None   -> Some a
                        | None, _        -> None

                { model with selectedSurface = surf }
            | FlyToSurface id -> model
            | MakeRelative id ->                
                let model = Surface.Files.makeSurfaceRelative id model scenePath
                let sgSurfaces = model.sgSurfaces |> Surface.Files.expandLazyKdTreePaths scenePath
                { model with sgSurfaces = sgSurfaces}
            | RemoveSurface id ->                
                let toRemove = model.surfaces |> PList.toList |> List.tryFindIndex (fun d -> d.guid = id)
                match toRemove with
                    | Some index ->
                        let s' = model.surfaces |> PList.removeAt index

                        let sg' = model.sgSurfaces |> HMap.remove id
                        
                        { model with surfaces = s'; sgSurfaces = sg' }
                    | None -> model             
            | OpenFolder id ->
                let surf = id |> Surface.getSurface model
                match surf with
                    | Some s -> 
                        let path = Surface.Files.getSurfaceFolder s scenePath
                        match path with
                            | Some p -> 
                                Process.Start("explorer.exe", p) |> ignore
                                model
                            | None -> model
                    | None -> model
            | _ -> model

    let absRelIcons (m : MSurface)=
        adaptive {
            let! absRelIcons = 
                m.relativePaths 
                    |> Mod.map(fun x ->                                 
                        let icon = if x then "suitcase icon" else "cloud download icon"                                
                        AttributeMap.ofList [
                            clazz icon;                                                    
                            onClick (fun x -> MakeRelative (m.guid |> Mod.force))
                        ]         
                    )
            return absRelIcons
        }

    let viewSurfaceHeader (m:MSurface) absRelIcons =         
        [
            Incremental.text m.name; text " "

            i [clazz "home icon"                                                
               onClick (fun _ -> FlyToSurface (m.guid |> Mod.force))
            ][] |> UI.wrapToolTip "FlyTo"                                                                                

            Incremental.i (absRelIcons)(AList.empty) |> UI.wrapToolTip "portable"                                        

            i [clazz "folder icon"                                                
               onClick (fun _ -> OpenFolder (m.guid |> Mod.force))
            ][] |> UI.wrapToolTip "open folder"                                                                                

            i [clazz "Remove icon red";                                             
                onClick (fun _ -> RemoveSurface (m.guid |> Mod.force))
            ][] |> UI.wrapToolTip "Remove"                                         
        ]    

    let viewSurfaceList (m:MSurfaceModel) = 
        let listAttr = AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : visible;max-height: 30%"]
        Incremental.div listAttr (
            alist {                        
                for (_,s) in m.sgSurfaces |> AMap.toASet |> AList.ofASet do

                    let! selected = m.selectedSurface
                    let! color =
                        match selected with
                          | Some sel -> 
                            sel.guid |> Mod.map (fun g -> if g = (s.surface.guid |> Mod.force) then C4b.Blue else C4b.White)
                          | None -> Mod.constant C4b.White
                                                                                    
                    let! absRelIcons = absRelIcons s.surface
                    
                    let bgc = color |> Html.ofC4b |> sprintf "color: %s"

                    yield div [clazz "item"] [
                        i [
                            clazz "large cube middle aligned icon"; 
                            style bgc
                            onClick (fun _ -> SelectSurface (s.surface.guid |> Mod.force))
                        ][]
                        div [clazz "content"] [
                            div [clazz "header"] (
                                viewSurfaceHeader s.surface absRelIcons
                            )
                            div [clazz "description"] [
                                Incremental.text (
                                    s.globalBB 
                                        |> Mod.map (fun d -> sprintf "Size: %.3f m" (d.Size.Length)))

                                Incremental.text (
                                    s.surface.relativePaths
                                        |> Mod.map (fun d -> sprintf " | local: %A" (d)))
                            ]                                    
                        ]
                    ]
            }
        )

    let viewSurfaceProperties (model:MSurfaceModel) =
        adaptive {
            let! bla = model.selectedSurface
            let opt = 
                match bla with
                    | Some s -> SurfaceProperties.view s
                    | None   -> div[][]

            return (opt |> UI.map SurfacePropertiesMessage)
        }                          

    let surfaceUI (model:MSurfaceModel) =
        div[][                            
            yield GuiEx.accordion "Surfaces" "Cubes" true [ viewSurfaceList model ]
            yield GuiEx.accordion "Properties" "Content" true [
                        Incremental.div AttributeMap.empty (AList.ofModSingle (viewSurfaceProperties model))
            ]
        ]


