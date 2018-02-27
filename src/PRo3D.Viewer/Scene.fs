namespace PRo3D

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Opc    

open PRo3D.Surfaces
open PRo3D.Viewer
open PRo3D.Surfaces.Surface
open PRo3D.Groups

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let withScene (s:Scene) (m:Model) =
        { m with scene = s}

    let withScene' (m:Model) (s:Scene) =
        { m with scene = s}

    let private stashAndSaveRecent' (s : SceneHandle) (model : Model) =
        let scenePaths = 
            model.recent.recentScenes                 
                |> HMap.update s.path 
                    (
                        fun v -> 
                            match v with
                                | Some x -> { x with writeDate = s.writeDate }
                                | None -> s
                    )

        let recent = { model.recent with recentScenes = scenePaths }

        Serialization.save "./recent" recent |> ignore

        { model with recent = recent }

    let stashAndSaveRecent (path : string) (model : Model) =
        let handle = {
            name = Path.GetFileName(path)
            path = path
            writeDate = DateTime.Now
        }
        stashAndSaveRecent' handle model

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scene =
    open Aardvark.Base.MultimethodTest       
    //open Aardvark.Base.Incremental.Computation Expression Builders
    
    let _surfaceModelLens = Model.Lens.scene |. Scene.Lens.surfacesModel
    let _flatSurfaces = Scene.Lens.surfacesModel |. SurfaceModel.Lens.surfaces |. GroupsModel.Lens.flat

    let expandRelativePaths (m:Scene) =
                        
        match m.scenePath with
            | Some p ->         
                let p' = Path.Combine((Path.GetDirectoryName p),"Surfaces")
                let flat' = 
                    (m.surfacesModel.surfaces.flat |> Leaf.toSurfaces)
                        |> HMap.map(fun _ x ->                            
                                if x.relativePaths then
                                    let p'' = Path.Combine(p', x.name)
                                    { x with opcPaths = Files.expandNamesToPaths p'' x.opcNames }
                                else x
                            )
                        |> HMap.map(fun _ x -> Leaf.Surfaces x)

                let sm = { m.surfacesModel.surfaces with flat = flat' }                
                
                { m with surfacesModel = { m.surfacesModel with surfaces = sm }}      
            | None -> m        

    let private readLine (filePath:string) =
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

        let existingSurfaces = 
            m.scene.surfacesModel.surfaces.flat
              |> Leaf.toSurfaces
              |> HMap.toList 
              |> List.map snd 
              |> PList.ofList         

        let allSurfaces = existingSurfaces |> PList.append' surfaces //????
        let sChildren = surfaces |> PList.map Leaf.Surfaces

        let m = 
            m.scene.surfacesModel.surfaces 
              |> Groups.addLeaves m.scene.surfacesModel.surfaces.activeGroup.path sChildren 
              |> Lenses.set' (_surfaceModelLens |. SurfaceModel.Lens.surfaces) m                        
        
        let surfaceMap = 
            (m.scene.surfacesModel.surfaces.flat |> Leaf.toSurfaces)

        //handle sg surfaces
        let m = 
            allSurfaces
                |> Surface.createSgSurfaces runtime signature
                |> HMap.union m.scene.surfacesModel.sgSurfaces
                |> Files.expandLazyKdTreePaths m.scene.scenePath surfaceMap
                |> Lenses.set' (_surfaceModelLens |. SurfaceModel.Lens.sgSurfaces) m                                               
                    
        m.scene.surfacesModel 
          |> SurfaceModel.triggerSgGrouping 
          |> Lenses.set' _surfaceModelLens m
             
    let prepareSurfaceModel (runtime : IRuntime) (signature: IFramebufferSignature) (scenePath : option<string>) (model : SurfaceModel) : SurfaceModel =
        let surfaces = model.surfaces.flat |> Leaf.toSurfaces 

        let surfacesList =
            surfaces
              |> HMap.toList 
              |> List.map snd 
              |> PList.ofList

        let sgSurfaces = 
            Surface.createSgSurfaces runtime signature surfacesList |> Files.expandLazyKdTreePaths scenePath surfaces        

        model 
          |> SurfaceModel.withSgSurfaces sgSurfaces 
          |> SurfaceModel.triggerSgGrouping    
          
    let setFrustum (m:Model) =
       let near = m.scene.config.nearPlane.value
       let far = m.scene.config.farPlane.value
       Frustum.perspective 60.0 near far 1.0
           

    let loadScene path m runtime signature =
        try
            let s = { (Serialization.loadAs<Scene> path) with scenePath = Some path }
        
            let m = 
                m |> Model.withScene (expandRelativePaths <| s)

            let m = { m with frustum = setFrustum m }

            let sModel = 
                m.scene.surfacesModel 
                  |> prepareSurfaceModel runtime signature s.scenePath

            _surfaceModelLens.Set(m, sModel)        
        with _ ->
            Log.error "Could not load selected scenefile %A. It is either outdated or not a valid scene" path
            //Log.error "error %A" e
            m

    
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

