namespace PRo3D

open System
open System.IO
open Aardvark.Base
open Aardvark.Opc    

open PRo3D.Surfaces
open PRo3D.Viewer
open PRo3D.Surfaces.Surface
open PRo3D.Groups

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scene =
    open Aardvark.Base.MultimethodTest
    
    
    let surfaceModelLens = Model.Lens.scene |. Scene.Lens.surfacesModel
    let _flatSurfaces = Scene.Lens.surfacesModel |. SurfaceModel.Lens.surfaces |. GroupsModel.Lens.flat

    let expandRelativePaths (m:Scene) =
        // Surfaces        
        
        //match m.scenePath with
        //    | Some p ->         
        //        let p' = Path.Combine((Path.GetDirectoryName p),"Surfaces")
        //        let surfaces = 
        //            m.surfacesModel.surfaces                           
        //            |> PList.map(
        //                fun x -> 
        //                    if x.relativePaths then
        //                        //let p'' = Path.Combine(p', x.name)
        //                        { x with opcPaths = Files.expandNamesToPaths p'' x.opcNames }
        //                    else x
        //                )
        //        { m with surfacesModel = Surface.updateSurfaces m.surfacesModel surfaces}
        //    | None -> m     
        
        match m.scenePath with
            | Some p ->         
                let p' = Path.Combine((Path.GetDirectoryName p),"Surfaces")
                let newFlats = 
                    (Child.childrenToSurfaces m.surfacesModel.surfaces.flat)   
                        |> HSet.map(
                            fun x -> 
                                if x.relativePaths then
                                    let p'' = Path.Combine(p', x.name)
                                    { x with opcPaths = Files.expandNamesToPaths p'' x.opcNames }
                                else x
                            )
                let flatList = newFlats|> HSet.toList
                let childList = flatList |> List.map(fun x -> Child.Surfaces x )
                let newGroupsModel = Groups.updateChildList childList m.surfacesModel.surfaces
                let m' = { m with surfacesModel = { m.surfacesModel with surfaces = newGroupsModel }}      
                { m' with surfacesModel = SurfaceModel.updateSurfaces m'.surfacesModel (flatList |> PList.ofList)}
            | None -> m        

    let readLine (filePath:string) =
            use sr = new System.IO.StreamReader (filePath)
            sr.ReadLine ()       

    
    
    let addLegacyTrafos surfaces = 
        surfaces 
            |> PList.map(fun s -> s,Path.ChangeExtension(s.importPath, ".trafo"))
            |> PList.map(fun(s,p) ->                     
                    match (Serialization.fileExists p) with
                        | Some path->
                            let t = readLine path
                            { s with preTransform = Trafo3d.Parse(t) }
                        | None ->
                            s
                        )

    let getOPCxPath (surfacePath : string) = 
        let name = Path.GetFileName surfacePath
        Path.ChangeExtension(Path.Combine(surfacePath, name), ".opcx")

    let addSurfaceAttributes surfaces = 
        surfaces 
            |> PList.map(fun s -> s, s.importPath |> getOPCxPath)
            |> PList.map(fun(s,p) -> 
                    match (Serialization.fileExists p) with
                        | Some path->        
                            let layers = Surface.SurfaceAttributes.read path
                            let textures = layers |> SurfaceProperties.getTextures

                            { s with 
                                scalarLayers  = layers |> SurfaceProperties.getScalars
                                textureLayers = textures
                                selectedTexture = textures |> PList.tryHead
                            }
                        | None ->
                            s
                        )

    /// appends surfaces to existing surfaces
    let import' (runtime : IRuntime) (signature: IFramebufferSignature)(surfaces : plist<Surface>) (m : Model) =
            
        //handle semantic surfaces
        let surfaces = 
            surfaces 
                |> addLegacyTrafos
                |> addSurfaceAttributes                

        let existingSurfaces = Child.childrenToSurfaces m.scene.surfacesModel.surfaces.flat |> HSet.toList |> PList.ofList         
        let allSurfaces = surfaces |> PList.append' existingSurfaces
        let sChildren = surfaces |> PList.map (fun x -> Child.Surfaces x) 
        let newGroupsModel = Groups.AddChildren m.scene.surfacesModel.surfaces.activeGroup.path sChildren m.scene.surfacesModel.surfaces
        let m = //{ m with scene = { m.scene with surfacesModel = { m.scene.surfacesModel with surfaces = newGroupsModel }}}
            (surfaceModelLens |. SurfaceModel.Lens.surfaces).Set(m, newGroupsModel)
        
        //handle sg surfaces
        let sgSurfaces = 
            allSurfaces
                |> Surface.createSgSurfaces runtime signature
                |> HMap.union m.scene.surfacesModel.sgSurfaces
                |> Files.expandLazyKdTreePaths m.scene.scenePath
                                       
        let m = (surfaceModelLens |. SurfaceModel.Lens.sgSurfaces).Set(m, sgSurfaces)

        let grouped = m.scene.surfacesModel |> SurfaceModel.triggerSgGrouping
        surfaceModelLens.Set(m, grouped)        

    /// loads surfaces given in scene - creates their sg pendants
    let load (runtime : IRuntime) (signature: IFramebufferSignature)(s : Scene) (m : Model) =
        
        let s = s |> expandRelativePaths

        let flat = Groups.flatten s.surfacesModel.surfaces.rootGroup
        let s = _flatSurfaces.Set(s, flat)
        let m = { m with scene = s }

        let surfs = s.surfacesModel.surfaces.flat |> Child.childrenToSurfaces |> HSet.toList |> PList.ofList

        let sgSurfaces = Surface.createSgSurfaces runtime signature surfs |> Files.expandLazyKdTreePaths m.scene.scenePath

        let m = (surfaceModelLens |. SurfaceModel.Lens.sgSurfaces).Set(m, sgSurfaces)

        let grouped = m.scene.surfacesModel |> SurfaceModel.triggerSgGrouping
        
        surfaceModelLens.Set(m, grouped)

    let loadScene path m runtime signature =
        let s = { (Serialization.loadAs<Scene> path) with scenePath = Some path }
        let m = { m with scene = s }

        load runtime signature s m
            
    let loadLastScene runtime signature m =
        match Serialization.fileExists "./recent" with
            | Some path  ->
              try 
                let r = Serialization.loadAs<Recent> path
                let m = { m with recent = r}
                match r.recentScenes |> HMap.toList |> List.sortByDescending (fun (_,v) -> v.writeDate) |> List.tryHead with
                    | Some (_,scenePath) ->
                        try
                            loadScene scenePath.path m runtime signature        
                        with
                            | e ->
                                Log.warn "Error parsing last scene: %s. loading empty" scenePath.path
                                Log.error "%A" e.Message
                                m
                    | None -> 
                        Log.warn("No recent scene found")
                        m
                //m
                with e ->
                    Aardvark.Base.Log.warn "%A. deleting recent." e
                    System.IO.File.Delete path
                    m
            | _ -> 
                Log.warn("recent scene file does not exist (yet)")
                m

