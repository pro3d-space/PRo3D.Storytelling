namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
open Aardvark.VRVis
open Aardvark.SceneGraph

open PRo3D

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =

    module Utils =
    
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


    let qualityInit = 
        {
            value = 0.0
            min =  -2.0
            max = 5.0
            step = 0.1
            format = "{0:0.0}"
        }

    let mk path = 
        
        let names = Utils.getOPCNames path                
        {
            guid = Guid.NewGuid()
            name = IO.Path.GetFileName path
            importPath = path
            opcNames = names
            opcPaths = names |> Utils.expandNamesToPaths path
            fillMode = FillMode.Fill
            cullMode = CullMode.None
            isVisible = true
            relativePaths = false
            quality = qualityInit
        }

    let checkBB (bb:Box3d) = 
        if(bb.IsInvalid) then 
            failwith (sprintf "invalid bounding box %s" (bb.ToString()))
        else bb

    let computeBB (hierarchies : list<PatchHierarchy>) =
         hierarchies
            |> List.map(fun d -> d.tree |> QTree.getLeaves)                  
            |> Seq.map (fun p -> p |> Seq.map(fun d -> checkBB d.info.GlobalBoundingBox)) 
            |> Seq.concat
            |> Box3d.ofSeq

    let computeBBfromRoots (hierarchies : list<PatchHierarchy>) =
         hierarchies
            |> List.map(fun d -> d.tree |> QTree.getRoot)                  
            |> Seq.map (fun p -> checkBB p.info.GlobalBoundingBox)
            |> Box3d.ofSeq

    let toSg (s:Surface) sg bb (kd:hmap<Box3d,Level0KdTree>) = {
        surface = s       
        sceneGraph = sg
        globalBB = bb
        kdTree = kd
    }
      
    let createSgSurfaces runtime signature surfaces =
        surfaces
            //|> List.map (fun (d : Surface) -> { d with guid = Guid.NewGuid() })
            |> List.map (fun d -> { Configurations.Empty.mars() with patchHierarchies = d.opcPaths })
            |> List.map (fun d -> Viewer.createPlainSceneGraph runtime signature d true)
            |> List.zip surfaces
            |> List.map (fun (surf, (sg, hierarchies, kdtree)) -> toSg surf sg (hierarchies |> computeBB) kdtree)
            |> List.map (fun d -> (d.surface.guid, d))
            |> HMap.ofList

    let getSurface model guid =
        model.surfaces |> List.tryFind (fun a -> a.guid = guid)
    
    let updateSurface model (s':Surface)  =
        let surfaces' = model.surfaces |> List.updateIf (fun x -> x.guid = s'.guid) (fun x -> s')     

        // update sgsurfaces as well !!!
        let sgSurfaces' = 
            model.sgSurfaces 
                |> HMap.alter s'.guid 
                        (fun x -> match x with 
                                    | Some k -> Some { k with surface = s' }
                                    | None -> None)   

        { model with surfaces = surfaces'; sgSurfaces = sgSurfaces' }

    let updateSurfaces model (surfaces' : list<Surface>) =
        let sgSurfaces =
            surfaces' 
                |> List.map (
                    fun x -> 
                        let update = 
                            (fun (d : Option<SgSurface>) ->
                                match d with 
                                    | Some k -> Some { k with surface = x }
                                    | None -> None)

                        HMap.alter x.guid update model.sgSurfaces ) 
                |> List.fold (fun a b -> HMap.union a b) HMap.empty

        { model with surfaces = surfaces'; sgSurfaces = sgSurfaces }
    
    //let checkPath p =
    //    if File.Exists
    //        Some p

    open System.IO
    let getSurfaceFolder (surface:Surface) (scenePath:option<string>) =        
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
                        let s' = { s with opcPaths = Utils.expandNamesToPaths targetDir s.opcNames; relativePaths = true }
                        updateSurface surfaceModel s'
                    
                    | None ->
                        Log.error "surface %s not found" (guid.ToString())
                        surfaceModel
            | None -> 
                Log.error "can't make surface relative. not valid scene path has not been saved yet."
                surfaceModel

 module SurfaceApp =
    open System.Diagnostics
    open Aardvark.Rendering.GL.Compiler

    open PRo3DCompability
    
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

    let loadObjectSet (lvl0Tree : Level0KdTree) =       
    
        match lvl0Tree with
            | InCoreKdTree kd -> 
                kd.kdTree
            | LazyKdTree kd -> 
            
                let positions = kd.kdtreePath |> Aara.fromFile<V3f>

                let data = 
                    positions.Data |> Array.map (fun x ->  x.ToV3d() |> kd.affine.Forward.TransformPos)

                let invalidIndices = getInvalidIndices data              
                let index = PRo3DCSharp.ComputeIndexArray(positions.Size.XY.ToV2i(), invalidIndices)                
                
                let triangles = 
                    index 
                        |> Seq.map(fun x -> data.[x])
                        |> Seq.chunkBySize 3                               
                        |> Seq.map(fun x -> Triangle3d(x))
                        |> Seq.filter(fun x -> triangleIsNan x |> not) 
                        |> TriangleSet
                        
                let kdTree =
                    match kd.kdTree with
                        | Some k -> k
                        | None -> KdTrees.loadKdtree kd.kdtreePath
                            
                let mutable kdTree' = kdTree               
                kdTree'.KdIntersectionTree.ObjectSet <- triangles
                kdTree'  

    let intersectHMap (kdTreeMap: hmap<Box3d, Level0KdTree>) bb (ray : FastRay3d) name= 

        let kdtree = kdTreeMap |> HMap.find bb |> loadObjectSet        

        let kdi = kdtree.KdIntersectionTree 
        let mutable hit = ObjectRayHit.MaxRange
                
        let objFilter a b = true
        let hitFilter a b c d = false
        if kdi.Intersect(ray, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then 
            let point = ray.Ray.GetPointOnRay(hit.RayHit.T)            
            Some (hit.RayHit.T, name)
        else            
            None

    let doKdTreeIntersection (m : Model) (t : Trafo3d) (r : FastRay3d) = 
        m.sgSurfaces
            |> HMap.map (fun _ d -> d.kdTree, d.surface.name) 
            |> HMap.map (
                fun _ (kd ,n) ->                     
                    if kd.IsEmpty 
                    then 
                        Log.error "no kdtree loaded for %s" n
                        None
                    else
                        
                        //get bbs that are hit
                        let boundingBoxes =
                            kd  |> HMap.toSeq 
                                |> Seq.map fst
                                |> Seq.filter(
                                    fun x -> 
                                       let mutable t = 0.0
                                       x.Intersects(r.Ray, &t)
                                )
                        
                        //intersect corresponding kdtrees
                        let intersection =
                            boundingBoxes
                                |> Seq.map(
                                    fun bb -> intersectHMap kd bb r n)
                                |> Seq.choose(fun x -> x)
                                |> Seq.sortByDescending(fun (t,n)-> t)
                                |> Seq.tryHead
                        
                        intersection
                )
            |> Seq.filter(fun (_,d) -> d.IsSome)
            |> Seq.map(fun (_,d) -> d.Value)
            |> Seq.sortByDescending(fun (t,n)-> t)
            |> Seq.tryHead

    let update (model:SurfaceModel) (action:Action) (scenePath : option<string>) = 
        match action with
            | SurfacePropertiesMessage msg ->
                match model.selectedSurface with 
                    | Some s -> 
                        let s' = SurfaceProperties.update s msg                            
                        
                        let model = Surface.updateSurface model s'
                        { model with selectedSurface = Some s' }
                    | None -> model
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
                Surface.makeSurfaceRelative id model scenePath
            | RemoveSurface id ->                
                let toRemove = model.surfaces |> List.tryFindIndex (fun d -> d.guid = id)
                match toRemove with
                    | Some index ->
                        let s' = model.surfaces |> PList.ofList |> PList.removeAt index |> PList.toList

                        let sg' = model.sgSurfaces |> HMap.remove id
                        
                        { model with surfaces = s'; sgSurfaces = sg' }
                    | None -> model             
            | OpenFolder id ->
                let surf = id |> Surface.getSurface model
                match surf with
                    | Some s -> 
                        let path = Surface.getSurfaceFolder s scenePath
                        match path with
                            | Some p -> 
                                Process.Start("explorer.exe", p) |> ignore
                                model
                            | None -> model
                    | None -> model
            | _ -> model

    let viewSurfaceList (m:MSurfaceModel) = 
            Incremental.div 
                (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto;max-height: 30%"]) (
                    alist {                        
                        for (_,s) in m.sgSurfaces |> AMap.toASet |> AList.ofASet do

                            let! selected = m.selectedSurface
                            let! color =
                                match selected with
                                    | Some sel -> 
                                        sel.guid |> Mod.map (fun g -> if g = (s.surface.guid |> Mod.force) then C4b.Blue else C4b.White)
                                    | None -> Mod.constant C4b.White
                            
                            let bgc = sprintf "color: %s" (Html.ofC4b color)
                            let! absRelIcons = 
                                s.surface.relativePaths 
                                    |> Mod.map(
                                        fun x -> 
                                            if x 
                                            then 
                                                AttributeMap.ofList [ clazz "suitcase icon";
                                                                      attribute "data-content" "???"
                                                                      attribute "data-position" "left center"
                                                                      attribute "data-variation" "mini"
                                                                    ]
                                            else
                                                AttributeMap.ofList [
                                                    clazz "cloud download icon";
                                                    attribute "title" "Relative";
                                                    attribute "data-position" "left center"
                                                    attribute "data-variation" "mini"
                                                    onClick (fun x -> MakeRelative (s.surface.guid |> Mod.force))
                                                ] 
                                                
                                    )                            
                                
                            yield div [clazz "item"] [
                                i [
                                    clazz "large cube middle aligned icon"; 
                                    style bgc
                                    onClick (fun x -> SelectSurface (s.surface.guid |> Mod.force))
                                ][]
                                div [clazz "content"] [
                                    div [clazz "header"] [
                                        Incremental.text s.surface.name
                                        text " "
                                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                                            i [
                                                clazz "home icon"
                                                attribute "title" "FlyTo"
                                                attribute "data-position" "left center"
                                                attribute "data-variation" "mini"
                                                onClick (fun x -> FlyToSurface (s.surface.guid |> Mod.force))
                                            ][]  
                                        )
                                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                                            Incremental.i (absRelIcons)(AList.empty)
                                        )
                                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                                            i [
                                                clazz "folder icon"
                                                attribute "title" "Open Folder"
                                                attribute "data-position" "left center"
                                                attribute "data-variation" "mini"
                                                onClick (fun x -> OpenFolder (s.surface.guid |> Mod.force))
                                            ][]
                                        )
                                        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                                            i [
                                                clazz "Remove icon red"; 
                                                attribute "title" "Remove"
                                                attribute "data-position" "left center"
                                                attribute "data-variation" "mini"
                                                onClick (fun x -> RemoveSurface (s.surface.guid |> Mod.force))
                                            ][]  
                                        )
                                    ]
                                    div [clazz "description"] [
                                        Incremental.text (
                                            s.globalBB 
                                                |> Mod.map (fun d -> sprintf "Size: %.3f m" (d.Size.Length)))

                                        Incremental.text (
                                            s.surface.relativePaths
                                                |> Mod.map (fun d -> sprintf " | local: %A" (d)))
                                    ]

                                    //<i class="huge icons">
                                    //    <i class="puzzle icon"></i>
                                    //    <i class="corner add icon"></i>
                                    //</i>
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
            //button [onClick (fun _ -> QuickLoad1)][text "Surface"]
            //button [onClick (fun _ -> QuickLoad2)][text "Scene"]
          
            GuiEx.accordion "Surfaces" "Cubes" true [ viewSurfaceList model ]
            GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (viewSurfaceProperties model))
            ]
        ]


