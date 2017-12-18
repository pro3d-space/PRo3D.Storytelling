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
open PRo3D.Groups

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =
    open Aardvark.SceneGraph.SgFSharp.Sg
    open Aardvark.UI.Trafos    
    
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
                    let surf = SurfaceModel.getSurface surfaceModel guid
                    match surf with
                        | Some s ->
                            // create target dir
                            let targetSurfaceDir = Path.Combine((Path.GetDirectoryName sp),"Surfaces")
                            if not (Directory.Exists targetSurfaceDir) then
                                Directory.CreateDirectory(targetSurfaceDir) |> ignore

                            // copy data
                            match s with
                                |Child.Surfaces sf -> 
                                    let targetDir = Path.Combine(targetSurfaceDir, sf.name)
                                    Copy.copyAll sf.importPath targetDir true

                                    // update opc paths and scene
                                    let s' = { sf with opcPaths = expandNamesToPaths targetDir sf.opcNames; relativePaths = true }
                                    surfaceModel |> SurfaceModel.updateSingleSurface s'
                                | _ ->
                                    Log.error "surface %s not found" (guid.ToString())
                                    surfaceModel
                        | None ->
                            Log.error "surface %s not found" (guid.ToString())
                            surfaceModel
                | None -> 
                    Log.error "can't make surface relative. not valid scene path has not been saved yet."
                    surfaceModel

    type SgHelper = {
        surf        : PRo3D.Surfaces.Surface
        bb          : Box3d
        sg          : ISg        
        kdtree      : hmap<Box3d,Level0KdTree>
    }

    let assertInvalidBB (bb:Box3d) = 
        if(bb.IsInvalid) then 
            failwith (sprintf "invalid bounding box %s" (bb.ToString()))
        else bb
   
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
            preTransform    = Trafo3d.Identity            
            scalarLayers    = PList.empty
            selectedScalar  = None
            textureLayers   = PList.empty
            selectedTexture = None            
        }       

    let sgOfSurface (s:PRo3D.Surfaces.Surface) sg (bb:Box3d) (kd:hmap<Box3d,Level0KdTree>) = 
    
        let pose = Pose.translate V3d.Zero // bb.Center
        let trafo = { TrafoController.initial with pose = pose; previewTrafo = Pose.toTrafo pose; mode = TrafoMode.Local }

        {
            surface    = s       
            sceneGraph = sg
            globalBB   = bb
            kdTree     = kd
            trafo      = trafo
        }

    let transformBox (trafo:Trafo3d) (bb:Box3d) =
        bb.Transformed(trafo)
              
    let createSgSurfaces runtime signature surfaces =        
        let sghs =
            surfaces
                |> PList.toList
                |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths; preTransform = d.preTransform })
                |> List.map (fun d -> Viewer.createPlainSceneGraph runtime signature d true)
                |> List.zip (surfaces |> PList.toList)
                |> List.map(fun (surf, (sg, hierarchies, kdtree)) -> 
                    let bb = hierarchies |> combineLeafBBs |> transformBox surf.preTransform
                    { surf = surf; bb = bb; sg = sg; kdtree = kdtree })

        let sgSurfaces =
            sghs 
              |> List.map (fun d -> sgOfSurface d.surf d.sg d.bb d.kdtree)
              |> List.map (fun d -> (d.surface.guid, d))
              |> HMap.ofList       

        sgSurfaces
                  
 module SurfaceApp =
    open System.Diagnostics
    open Aardvark.Rendering.GL.Compiler

    open PRo3DCompability    
    open Aardvark.VRVis
    
    type Action =
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | FlyToSurface              of Guid
        | MakeRelative              of Guid
        | RemoveSurface             of Guid*list<Index>
        | PickSurface               of SceneHit*string
        | OpenFolder                of Guid
        | GroupsMessage             of Groups.Action    

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
       // let hitFilter a b c d = false
        if kdi.Intersect(ray, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then
            //let point = ray.Ray.GetPointOnRay(hit.RayHit.T)
            Some (hit.RayHit.T, surface),c
        else            
            None,c

    let doKdTreeIntersection (m : SurfaceModel) (t : Trafo3d) (r : FastRay3d) cache = 
        let mutable cache = cache

        m.sgSurfaces
            |> HMap.toSeq
            |> Seq.map (fun (_,d) -> d.kdTree, d.surface) 
            |> Seq.sortByDescending(fun (_,surf) -> surf.priority.value)
            |> Seq.choose (
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
                                       let r' = r.Ray.Transformed(surf.preTransform.Backward) //combine pre and current transform
                                       x.Intersects(r', &t)
                                )                                                
                        //intersect corresponding kdtrees
                        let closestHit =
                            hitBoxes
                                |> Seq.map(
                                    fun bb -> 
                                        Log.line "intersection: %s; pr: %f" surf.name surf.priority.value                                        

                                        let hit, c = 
                                            r.Ray.Transformed(surf.preTransform.Backward) //combine pre and current transform
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
            |> Seq.tryPick(fun d -> Some d), cache
        
    let update (model:SurfaceModel) (action:Action) (scenePath : option<string>) = 
        match action with
            | SurfacePropertiesMessage msg ->                
                let m = 
                  match model.surfaces.lastSelectedChild with
                    | Some s -> 
                      match s with 
                        | Child.Surfaces sf ->
                            let s' = SurfaceProperties.update sf msg 
                            let model = model |> SurfaceModel.updateSingleSurface s'
                            { model with surfaces = { model.surfaces with lastSelectedChild = Some (Child.Surfaces s') }}
                        | _ -> model
                    | None -> model
                match msg with
                  | SurfaceProperties.SetPriority _ -> m |> SurfaceModel.triggerSgGrouping 
                  | _ -> m            
            | MakeRelative id ->                
                let model = Surface.Files.makeSurfaceRelative id model scenePath
                let sgSurfaces = model.sgSurfaces |> Surface.Files.expandLazyKdTreePaths scenePath
                { model with sgSurfaces = sgSurfaces}
            | RemoveSurface (id,path) -> //model
                let groups = Groups.removeChild2 model.surfaces id path
                let sg' = model.sgSurfaces |> HMap.remove id
                { model with surfaces = groups; sgSurfaces = sg'}                    
            | OpenFolder id ->
                let surf = id |> SurfaceModel.getSurface model
                match surf with
                    | Some s -> 
                        match s with 
                            | Child.Surfaces sf ->
                                let path = Surface.Files.getSurfaceFolder sf scenePath
                                match path with
                                    | Some p -> 
                                        Process.Start("explorer.exe", p) |> ignore
                                        model
                                    | None -> model      
                            | _ -> failwith "can only contain surfaces"
                    | None -> model
            | GroupsMessage msg -> { model with surfaces = Groups.update model.surfaces msg}
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
    
    let mkColor (model : MGroupsModel) (s : MSurface) =
            let id = s.guid |> Mod.force

            let color =  
                model.selectedChilds
                    |> ASet.map(fun x -> x.id = id)
                    |> ASet.contains true
                    |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White)            
            color

    let viewSurfacesInGroups (path:list<Index>) (model:MGroupsModel)(select : MSurface*list<Index> -> 'outer)(lift : Groups.Action -> 'outer) (surfaces: alist<IMod<MChild>>) : alist<DomNode<'outer>> = 
            alist {
                let surfaces = surfaces
                                |> AList.map (fun x -> Mod.force x)
                                |> AList.choose( fun x -> match x with
                                                            | MSurfaces s -> Some s
                                                            | _-> None )
                for s in surfaces do

                    let select = fun _ -> select (s,path)
                    let! c = mkColor model s
                    let bgc = sprintf "color: %s" (Html.ofC4b c)
                    let infoc = sprintf "color: %s" (Html.ofC4b C4b.White)
                    let! key = s.guid
                                                                                    
                    let! absRelIcons = absRelIcons s

                    yield div [clazz "item"; style bgc] [
                        i [clazz "large cube middle aligned icon"; onClick select;style bgc][] 
                        div [clazz "content"; style bgc] [
                            let desc = s.name
                            yield Incremental.div (AttributeMap.ofList [clazz "header"; style infoc])(
                                alist {
                                    yield Incremental.text desc

                                    yield i [clazz "home icon"                                                
                                             onClick (fun _ -> FlyToSurface key) ][] |> UI.wrapToolTip "FlyTo"                                                                                

                                    yield Incremental.i (absRelIcons)(AList.empty) |> UI.wrapToolTip "portable"                                        

                                    yield i [clazz "folder icon"                                                
                                             onClick (fun _ -> OpenFolder key) ][] |> UI.wrapToolTip "open folder"                                                                                

                                    yield i [clazz "Remove icon red"
                                             //onClick (fun _ -> lift <| RemoveSurface (key,path))] [] |> UI.wrapToolTip "Remove" 
                                             onClick (fun _ -> RemoveSurface (key,path)) ][] |> UI.wrapToolTip "Remove"        
                                    } 
                            ) 
                            yield div [clazz "description"; style infoc] [
                                Incremental.text (
                                    s.relativePaths
                                        |> Mod.map (fun d -> sprintf " | local: %A" (d)))
                            ]                                    
                        ]
                    ]
            }
           
    let rec viewTree path (group : MGroup) (model : MGroupsModel) : alist<DomNode<Action>> =

        alist {

            let! s = model.activeGroup
            let color = sprintf "color: %s" (Html.ofC4b C4b.White)                
            let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subGroups    

            let activeIcon =
                adaptive {
                    let active = s.id
                    let! group  =  group.key

                    return if (active = group) then "circle icon" else "circle thin icon"
                }

            let setActive = Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)
            let activeAttributes = 
                Groups.mkGroupAttributeMap activeIcon (GroupsMessage setActive)
                                   
            let toggleIcon =
                adaptive {
                    let! toggle = group.visible
                    return if toggle then "unhide icon" else "hide icon"
                }

            let toggleAttributes = Groups.mkGroupAttributeMap toggleIcon (GroupsMessage(Groups.ToggleGroup path))
               
            let desc =
                div [style color] [       
                    Incremental.text group.name
                    Incremental.i activeAttributes AList.empty |> UI.wrapToolTip "Set active"
                        
                    i [clazz "plus icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.AddGroup path))] [] 
                    |> UI.wrapToolTip "Add Group"   
                        
                    i [clazz "remove circle icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.ClearGroup path))] [] 
                    |> UI.wrapToolTip "Clear Group"
                        
                    i [clazz "remove icon red"
                       onMouseClick (fun _ -> Action.GroupsMessage(Groups.RemoveGroup path))] [] 
                    |> UI.wrapToolTip "Remove Group"

                    Incremental.i toggleAttributes AList.empty |> UI.wrapToolTip "Toggle Group"
                ]
                 
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                    let! selected = group.expanded
                    if selected 
                    then yield clazz "icon large outline open folder"
                    else yield clazz "icon large outline folder"
                    yield style "overflow-y : visible"
                } |> AttributeMap.ofAMap
            
            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = group.expanded
                    if isExpanded then yield style "visible"
                    else yield style "hidden"
                }         


            let select = fun (a:MSurface,path:list<Index>) -> Action.GroupsMessage(Groups.SelectChilds (path, a.guid |> Mod.force, a.name |> Mod.force))  // Select (path, a.key |> Mod.force)
            let lift   = fun (a:Groups.Action) -> (GroupsMessage a)

            yield div [ clazz "item"] [
                    Incremental.i itemAttributes AList.empty
                    div [ clazz "content" ] [                         
                        div [ clazz "description noselect"] [desc]
                        Incremental.div (AttributeMap.ofAMap childrenAttribs) (                          
                            alist { 
                                let! isExpanded = group.expanded
                                if isExpanded then yield! children

                                if isExpanded then 
                                    yield! viewSurfacesInGroups path model select lift group.childList
                            }
                        )  
                                
                    ]
            ]
                
        }


    let viewSurfacesGroups (model:MSurfaceModel) = 
        require Html.semui (
            TreeView.view [] (viewTree [] model.surfaces.rootGroup model.surfaces)
        )    
        

    //let viewSurfaceProperties (model:MSurfaceModel) =
    //    adaptive {
    //        let! bla = model.surfaces.lastSelectedChild
    //        let opt = 
    //            match bla with
    //                | Some s -> SurfaceProperties.view s
    //                | None   -> div[][]

    //        return (opt |> UI.map SurfacePropertiesMessage)
    //    }    

    let lscToDom lsc isSome = 
        adaptive {
            match lsc with
                | Some b ->   
                    let! b' = b
                    match b' with 
                        | MSurfaces bm -> return isSome bm
                        | _ -> return div[][]                                                             
                | None   -> return div[][] 
        }

    let viewSurfaceProperties (model:MSurfaceModel) =
        adaptive {
            let! bla = model.surfaces.lastSelectedChild
            let! opt = lscToDom bla (fun x -> SurfaceProperties.view x)

            return (opt |> UI.map SurfacePropertiesMessage)
        }                          
        
    let surfaceGroupProperties (model:MSurfaceModel) =
        adaptive {                                
            return (Groups.viewUI model.surfaces|> UI.map GroupsMessage)
        } 
    
    
    let surfaceUI (model:MSurfaceModel) =
        let item2 = 
            model.surfaces.lastSelectedItem 
                |> Mod.map (fun x -> 
                    match x with 
                        | SelectedItem.Group -> surfaceGroupProperties model
                        | _ -> viewSurfaceProperties model 
                )
        div[][                            
            yield GuiEx.accordion "Surfaces" "Cubes" true [ viewSurfacesGroups model ]
            yield GuiEx.accordion "Properties" "Content" true [
                        Incremental.div AttributeMap.empty (alist{ let! item = item2
                                                                   let! i = item
                                                                   yield i})
            ]
        ]
    
    