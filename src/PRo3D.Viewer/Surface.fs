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
                    |> Seq.map (fun a -> 
                                    let bla = System.IO.Path.GetFileName a; 
                                    bla )
                    |> Seq.toList 
            else []

        let expandNamesToPaths (path : string) (names : List<string>) =
            names |> List.map(fun a -> (Path.combine [path; a]).Replace(@"\", @"\\\\"))

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

    let toSg (s:Surface) sg bb kd = {
        surface = s
        guid = Guid.NewGuid()
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
            |> HSet.ofList

 module SurfaceApp =
    
    type Action =
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | SelectSurface             of Guid
        | FlyToSurface              of Guid
        | PickSurface               of SceneHit*string

    type Model = SurfaceModel

    let doKdTreeIntersection (m : Model) (t : Trafo3d) (r : FastRay3d) = 
        m.sgSurfaces
            |> HSet.map (fun d -> d.kdTree, d.surface.name) 
            |> HSet.map (
                fun (kd,n) ->            
                    Log.warn "do intersection for %s" n
                    let kdi = kd |> KdIntersectionTree
                    let mutable hit = ObjectRayHit.MaxRange
                    let objFilter  a b = true
                    let hitFilter a b c d = false
                    if kdi.Intersect(r, Func<_,_,_>(objFilter), null, 0.0, Double.MaxValue, &hit) then 
                        Log.warn "hit for %s" n
                        Some (hit.RayHit.T, n)
                    else
                        Log.error "no hit for %s" n
                        None //Double.MaxValue, n
                )
            |> HSet.toSeq
            |> Seq.filter(fun d -> d.IsSome)
            |> Seq.map(fun d -> d.Value)
            |> Seq.sortByDescending(fun (t,n)-> t)
            |> Seq.tryHead

    let update (model:SurfaceModel) (action:Action) = 
        match action with
            | SurfacePropertiesMessage msg ->
                match model.selectedSurface with 
                    | Some s -> 
                        let s' = SurfaceProperties.update s msg                            

                        let surfaces' = 
                            model.surfaces |> List.updateIf (fun x -> x.guid = s'.guid) (fun x -> s')

                        let sgSurfaces' =
                            model.sgSurfaces
                                |> HSet.toList 
                                |> List.updateIf (fun x -> x.surface.guid = s'.guid) (fun x -> { x with surface = s'})
                                |> HSet.ofList

                        {
                            surfaces = surfaces'
                            selectedSurface = Some s'
                            sgSurfaces = sgSurfaces'
                        }                        
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
            | _ -> model

    let viewSurfaceList (m:MSurfaceModel) = 
            Incremental.div 
                (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto;max-height: 30%"]) (
                    alist {
                        for s in (m.sgSurfaces |> AList.ofASet) do

                            let! selected = m.selectedSurface
                            let! color =
                                match selected with
                                    | Some sel -> 
                                        sel.guid |> Mod.map (fun g -> if g = (s.surface.guid |> Mod.force) then C4b.Blue else C4b.White)
                                    | None -> Mod.constant C4b.White
                            
                            let bgc = sprintf "color: %s" (Html.ofC4b color)

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
                                        i [
                                            clazz "home icon"; 
                                            onClick (fun x -> FlyToSurface (s.surface.guid |> Mod.force))
                                        ][]     
                                    ]
                                    div [clazz "description"] [
                                        Incremental.text (
                                            s.globalBB 
                                                |> Mod.map (fun d -> sprintf "Size: %.3f m" (d.Size.Length)))
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
            //button [onClick (fun _ -> QuickLoad1)][text "Surface"]
            //button [onClick (fun _ -> QuickLoad2)][text "Scene"]
            //button [onClick (fun _ -> DeleteLast)][text "Delete Last"]
            GuiEx.accordion "Surfaces" "Cubes" true [ viewSurfaceList model ]
            GuiEx.accordion "Properties" "Content" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (viewSurfaceProperties model))
            ]
        ]


