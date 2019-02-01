namespace PRo3D.Surfaces

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Opc

open Aardvark.VRVis.Opc

open Aardvark.UI.Operators
open Aardvark.UI.Trafos  

open PRo3D
open PRo3D.Groups



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Surface =
    open Aardvark.SceneGraph.SgFSharp.Sg                          
    module ObjectFiles =        

        let loadObject (s:PRo3D.Surfaces.Surface) : SgSurface =                        
            let obj = Loader.Assimp.load (s.importPath)            
            let meshes = 
                [
                    for mesh in obj.meshes do
                        yield mesh.geometry.Sg |> Sg.noEvents
                ] |> Sg.ofList

            let bb = obj.bounds
            let sg = 
                obj
                |> Sg.adapter
                |> Sg.requirePicking
                |> Sg.noEvents
                |> Sg.scale 1.0

            let pose = Pose.translate bb.Center // bb.Center V3d.Zero
            let trafo = { TrafoController.initial with pose = pose; previewTrafo = Pose.toTrafo pose; mode = TrafoMode.Local }
            {
                surface     = s.guid    
                trafo       = trafo
                globalBB    = bb
                sceneGraph  = sg
                picking     = Picking.PickMesh meshes
            }
                 
        let createSgObjects runtime signature surfaces =        
            let sghs =
                surfaces
                    |> PList.toList
                    |> List.map (fun d -> (loadObject d))

            let sgObjects =
                sghs 
                  |> List.map (fun d -> (d.surface, d))
                  |> HMap.ofList       

            sgObjects

    module SurfaceAttributes = 
        open System.Xml
            
        let get (name : string) (node : XmlNode)=
            node.SelectSingleNode(name).InnerText.Trim()

        let parseMap (index : int)(node : XmlNode) : ScalarLayer = 
            let definedRange = node |> get "ChannelsDefinedRange" |> Range1d.Parse
            { 
                label        = node |> get "Label" 
                actualRange  = node |> get "ChannelsActualRange"  |> Range1d.Parse
                definedRange = definedRange //node |> get "ChannelsDefinedRange" |> Range1d.Parse
                index        = index
                colorLegend  = (FalseColorsModel.initDefinedScalarsLegend definedRange)
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
                   
                         
 module SurfaceApp =
    open System.Diagnostics
    open Aardvark.Rendering.GL.Compiler

    open PRo3DCompability    
    open Aardvark.VRVis
    open Aardvark.Geometry
    open Surface
    
    type Action =
        | SurfacePropertiesMessage  of SurfaceProperties.Action
        | FlyToSurface              of Guid
        | MakeRelative              of Guid
        | RemoveSurface             of Guid*list<Index>
        | PickSurface               of SceneHit*string
        | OpenFolder                of Guid
        | RebuildKdTrees            of Guid
        | ToggleActiveFlag          of Guid
        | GroupsMessage             of Groups.Action  
        //| PickObject                of V3d
        | PlaceSurface              of V3d
        | ScalarsColorLegendMessage of FalseColorLegendApp.Action
        | ColorCorrectionMessage    of ColorCorrectionProperties.Action
        | SetHomePosition           //of Guid*CameraView
           
    let doKdTreeIntersection (m : SurfaceModel) (r : FastRay3d) cache : option<float * PRo3D.Surfaces.Surface> * hmap<_,_> = 
        let mutable cache = cache
        let activeSgSurfaces = 
          m.surfaces.flat
            |> HMap.filter(fun _ s -> s.active)
            |> HMap.toList 
            |> List.map fst
            |> List.choose(fun id -> m.sgSurfaces |> HMap.tryFind id)

        let hits = 
          activeSgSurfaces 
            |> List.map (fun d -> d.picking, (m.surfaces.flat |> HMap.find d.surface) |> Leaf.toSurface)                      
            |> List.choose (
               fun (p ,surf) ->
                 match p with
                    | Picking.KdTree kd ->
                      if kd.IsEmpty then Log.error "no kdtree loaded for %s" surf.name; None
                      else                        
                        //get bbs that are hit
                        let hitBoxes =
                          kd  
                            |> HMap.toList |> List.map fst
                            |> List.filter(
                                fun x -> 
                                   let mutable t = 0.0
                                   let r' = r.Ray //combine pre and current transform
                                   x.Intersects(r', &t)
                            )
                        //intersect corresponding kdtrees
                        let closestHit =
                          hitBoxes
                            |> List.choose(
                                fun bb -> 
                                    //Log.line "intersection: %s; pr: %f" surf.name surf.priority.value                                                             
                                    let ray = r.Ray.Transformed(surf.preTransform.Backward) |> FastRay3d  //combine pre and current transform                     
                                    let hit, c = 
                                      kd |> DebugKdTreesX.intersectKdTrees bb surf cache ray
                                    cache <- c
                                    hit
                                    )
                            |> List.sortBy(fun (t,_)-> t)
                            |> List.tryHead
                      
                        closestHit
                    | Picking.PickMesh pm -> Log.error "no kdtree loaded for %s" surf.name; None
               )
            |> List.groupBy(fun (_,surf) -> surf.priority.value) |> List.sortByDescending fst
            |> List.tryHead
            |> Option.bind(fun (_,b) -> b |> List.sortBy fst |> List.tryHead)
            //|> List.sortBy(fun (t,_) -> t)
            //|> List.tryPick(fun d -> Some d)

        hits, cache

    let updateTrafo (trafo:SurfaceTrafo) (surfaces:hmap<string,PRo3D.Surfaces.Surface>) (model:SurfaceModel) =                                
      match surfaces.TryFind(trafo.id) with
        | Some s -> 
          let f = (fun _ -> { s with preTransform = trafo.trafo } |> Leaf.Surfaces)
          let g = Groups.updateLeaf s.guid f model.surfaces
          Log.line "MeasurementImporter: matched and updated %s" s.name
          { model with surfaces = g} 
        | None -> model
                 
    let updateTrafos (trafos:plist<SurfaceTrafo>) (model:SurfaceModel) =
      let surfaces = 
        model.surfaces.flat 
          |> HMap.toList 
          |> List.map(fun (_,v) -> 
             let surf = v |> Leaf.toSurface
             (surf.name, surf))
          |> HMap.ofList
      
      let rec update (p : list<SurfaceTrafo>) (model:SurfaceModel)  =
          match p with
            | x::rest -> 
              match rest with
                | [] -> updateTrafo x surfaces model
                | _ ->  update rest (updateTrafo x surfaces model) 
            | _ -> model
      update (trafos |> PList.toList) model

    //let union (l : SurfaceModel) (r : SurfaceModel) : SurfaceModel =
    //  let l' = { l with 

    //  l

    let hmapsingle (k,v) = HMap.single k v

    let updateSurfaceTrafos (trafos: list<SurfaceTrafo>) (model:SurfaceModel) =
      let surfaces =        
        model.surfaces.flat 
          |> HMap.toList
          |> List.map(fun (_,v) -> 
            let s = (v |> Leaf.toSurface)
            s.name, s)
          |> HMap.ofList
      
      let surfaces' = 
        trafos 
          |> List.filter (fun x -> x.id |> surfaces.ContainsKey)
          |> List.map(fun x -> surfaces.Find x.id,x)
          |> List.map(fun (a,b) -> { a with preTransform = b.trafo })
            
      let flat' = 
        surfaces' 
          |> List.choose(fun x -> 
            let f = (fun _ -> x |> Leaf.Surfaces)
            Groups.updateLeaf' x.guid f model.surfaces 
              |> HMap.tryFind x.guid 
              |> Option.map(fun leaf -> (x.guid,leaf) |> hmapsingle))          
          |> List.fold HMap.union HMap.empty
                    
      //write union of surface models ... union flat and recalculate all others
      //this does only update surfaces ... not kdtrees or sgs !!!!             
      { model with surfaces = { model.surfaces with flat = flat' } }

    let update (model:SurfaceModel) (action:Action) (scenePath : option<string>) (view:CameraView) = 
      match action with
        | SurfacePropertiesMessage msg ->                
            let m = 
              match model.surfaces.singleSelectLeaf with
                | Some s -> 
                  let surface = model.surfaces.flat |> HMap.find s |> Leaf.toSurface 
                  let s' = 
                      match msg with
                        | SurfaceProperties.Action.SetHomePosition -> {surface with homePosition = Some view}
                        | _ -> SurfaceProperties.update surface msg
                  model |> SurfaceModel.updateSingleSurface s'                        
                | None -> model
            match msg with
              | SurfaceProperties.SetPriority _ -> m |> SurfaceModel.triggerSgGrouping 
              | _ -> m            
        | MakeRelative id ->                
            let model = Files.makeSurfaceRelative id model scenePath
            let sgSurfaces = model.sgSurfaces |> Files.expandLazyKdTreePaths scenePath (model.surfaces.flat |> Leaf.toSurfaces)
            { model with sgSurfaces = sgSurfaces}
        | RemoveSurface (id,path) -> //model
            let groups = Groups.removeLeaf model.surfaces id path true
            let sg' = model.sgSurfaces |> HMap.remove id
            { model with surfaces = groups; sgSurfaces = sg'} |> SurfaceModel.triggerSgGrouping              
        | OpenFolder id ->
            let surf = id |> SurfaceModel.getSurface model
            match surf with
              | Some s -> 
                match s with 
                  | Leaf.Surfaces sf ->
                    let path = Files.getSurfaceFolder sf scenePath
                    match path with
                      | Some p -> 
                          Process.Start("explorer.exe", p) |> ignore
                          model
                      | None -> model
                  | _ -> failwith "can only contain surfaces"
              | None -> model
        | RebuildKdTrees id ->                
            let surf = id |> SurfaceModel.getSurface model
            match surf with
              | Some (Leaf.Surfaces sf) ->                    
                let path = Files.getSurfaceFolder sf scenePath
                match path with
                   | Some p ->                            
                       let path = @".\20170530_TextureConverter_single\Vrvis.TextureConverter.exe"
                       if File.Exists(path) then //\Vrvis.TextureConverter.exe") then
                         let mutable converter = new Process()                             
                         Log.line "start processing"                        
                         converter.StartInfo.FileName <- "Vrvis.TextureConverter.exe"
                         converter.StartInfo.Arguments <- p + " -overwrite" //-lazy"
                         converter.StartInfo.UseShellExecute <- true                          
                         converter.StartInfo.WorkingDirectory <- @".\20170530_TextureConverter_single"                                    
                         converter.Start() |> ignore    
                         converter.WaitForExit()
                            //)
                       else
                         Log.line "texture converter not found"                             
                       model
                   | None -> model                         
              | _ -> model
        | ToggleActiveFlag id ->               
          let surf = id |> SurfaceModel.getSurface model
          match surf with
            | Some (Leaf.Surfaces s) ->
              { s with isActive = not s.isActive } |> SurfaceModel.updateSingleSurface' model
            | _ -> model
        | GroupsMessage msg -> 
            let groups = Groups.update model.surfaces msg

            match msg with
              | Groups.Action.RemoveGroup _ | Groups.Action.RemoveChild _ ->
                let sgs = 
                    model.sgSurfaces 
                        |> HMap.filter(fun k _ -> groups.flat |> HMap.containsKey k)

                { model with surfaces = groups; sgSurfaces = sgs } |> SurfaceModel.triggerSgGrouping                  
              | _ -> { model with surfaces = groups }
        //| PickObject p -> model   
        | PlaceSurface p -> 
            match model.surfaces.singleSelectLeaf with
            | Some id -> 
              match model.surfaces.flat.TryFind(id) with 
              | Some s -> 
                 let trans = Trafo3d.Translation(p)
                 let s = s |> Leaf.toSurface
                 let f = (fun _ -> { s with preTransform = trans  } |> Leaf.Surfaces)
                 let g = Groups.updateLeaf s.guid f model.surfaces
                 
                 let sgs' = 
                     model.sgSurfaces
                     |> HMap.update id (fun x -> 
                       match x with 
                       | Some sg ->    //let pose = Pose.translate p // bb.Center
                         let pose = { sg.trafo.pose with position = sg.trafo.pose.position + p }

                         let trafo' = { 
                           TrafoController.initial with 
                             pose = pose
                             previewTrafo = trans
                             mode = TrafoMode.Local 
                         }
                         { sg with trafo = trafo'; globalBB = (sg.globalBB.Transformed trafo'.previewTrafo) }
                       | None   -> failwith "surface not found")                             
                 { model with surfaces = g; sgSurfaces = sgs'} 
              | None -> model
            | None -> model
        | ScalarsColorLegendMessage msg ->
            match model.surfaces.singleSelectLeaf with
              | Some s -> 
                let surface = model.surfaces.flat |> HMap.find s |> Leaf.toSurface 
                match surface.selectedScalar with
                  | Some s -> 
                    let sc = { s with colorLegend = (FalseColorLegendApp.update s.colorLegend msg) }                        
                    let scs = surface.scalarLayers |> HMap.alter sc.index (Option.map(fun _ -> sc))
                    let s' = { surface with selectedScalar = Some sc; scalarLayers = scs }
                    model |> SurfaceModel.updateSingleSurface s'   
                  | None -> model
              | None -> model
        | ColorCorrectionMessage msg ->       
            let m = 
              match model.surfaces.singleSelectLeaf with
                | Some s -> 
                  let surface = model.surfaces.flat |> HMap.find s |> Leaf.toSurface
                  let s' = { surface with colorCorrection = (ColorCorrectionProperties.update surface.colorCorrection msg) }
                  model |> SurfaceModel.updateSingleSurface s'                        
                | None -> model
            m
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

    let isSelected (model : MGroupsModel) (s : MSurface) =
        let id = s.guid |> Mod.force
            
        model.selectedLeaves
          |> ASet.map(fun x -> x.id = id)
          |> ASet.contains true
    
    let mkColor (model : MGroupsModel) (s : MSurface) =
        isSelected model s 
          |> Mod.bind (fun x -> if x then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White)

    let lastSelected (model : MGroupsModel) (s : MSurface) =
            let id = s.guid |> Mod.force
            model.singleSelectLeaf 
                |> Mod.map( fun x -> match x with 
                                        | Some xx -> xx = id
                                        | _ -> false )

    let isSingleSelect (model : MGroupsModel) (s : MSurface) =
            model.singleSelectLeaf |> Mod.map( fun x -> 
                match x with 
                  | Some selected -> selected = (s.guid |> Mod.force)
                  | None -> false )

    let viewSurfacesInGroups (path:list<Index>) (model:MGroupsModel) (singleSelect : MSurface*list<Index> -> 'outer) (multiSelect : MSurface*list<Index> -> 'outer) (lift : Groups.Action -> 'outer) (surfaceIds: alist<Guid>) : alist<DomNode<'outer>> = 
       alist {

         let surfaces = 
            surfaceIds 
               |> AList.chooseM (fun x -> model.flat |> AMap.tryFind x)

         let surfaces = 
            surfaces
               |> AList.map Mod.force
               |> AList.choose(function | MSurfaces s -> Some s | _-> None )

         for s in surfaces do

             let singleSelect = fun _ -> singleSelect(s,path)
             let multiSelect = fun _ -> multiSelect(s,path)
             //let! selected = s |> isSingleSelect model

             let! c = mkColor model s
             let infoc = sprintf "color: %s" (Html.ofC4b C4b.White)

             let! key = s.guid                                                                             
             let! absRelIcons = absRelIcons s

             let visibleIcon = 
                amap {
                  yield onMouseClick (fun _ -> lift <| Groups.ToggleChildVisibility (key,path))
                  let! visible = s.isVisible
                  if visible then 
                    yield clazz "unhide icon" 
                  else 
                    yield clazz "hide icon"
                } |> AttributeMap.ofAMap
            
             let headerColor = 
               (isSingleSelect model s) 
                 |> Mod.map(fun x -> 
                   (if x then C4b.VRVisGreen else C4b.Gray) 
                     |> Html.ofC4b 
                     |> sprintf "color: %s"
                 ) 

            // let headerColor = sprintf "color: %s" (Html.ofC4b C4b.Gray)
             let headerAttributes =
                amap {
                  yield onClick singleSelect
                  //let! selected = isSingleSelect model s
                  
                  //yield if selected then style "text-transform:uppercase" else style "text-transform:none"
                  //yield if selected then style bgc else style bgc
                } |> AttributeMap.ofAMap

             let headerText = Mod.map2 (fun a b -> sprintf "%.0f|%s" a b) (s.priority.value) s.name

             let bgc = sprintf "color: %s" (Html.ofC4b c)
             yield div [clazz "item"; style infoc] [
                 i [clazz "cube middle aligned icon"; onClick multiSelect;style bgc][] 
                 div [clazz "content"; style infoc] [                     
                     yield Incremental.div (AttributeMap.ofList [style infoc])(
                         alist {
                             let! hc = headerColor
                             yield div[clazz "header"; style hc][
                                Incremental.span headerAttributes ([Incremental.text headerText] |> AList.ofList)
                             ]
                             //if selected then
                             //    yield div[onClick singleSelect][ u[][Incremental.text desc]]
                             //else
                             //    yield div[onClick singleSelect][Incremental.text desc]
 
                             yield i [clazz "home icon"                                                
                                      onClick (fun _ -> FlyToSurface key) ][] |> UI.wrapToolTip "Fly to surface"                                                     

                             yield i [clazz "folder icon"                                                
                                      onClick (fun _ -> OpenFolder key) ][] |> UI.wrapToolTip "Open Folder"

                             //yield i [
                             //  clazz "microchip icon"
                             //  onClick (fun _ -> RebuildKdTrees key)
                             //][] |> UI.wrapToolTip "process kdtrees"

                             yield Incremental.i (absRelIcons) (AList.empty)
                             yield Incremental.i visibleIcon AList.empty |> UI.wrapToolTip "Toggle Visible"
                             yield GuiEx.iconCheckBox s.isActive (ToggleActiveFlag key) |> UI.wrapToolTip "Toggle IsActive"
                         } 
                     )                                     
                 ]
             ]
       }
           
    let rec viewTree path (group : MNode) (model : MGroupsModel) : alist<DomNode<Action>> =

        alist {

            let! s = model.activeGroup
            let color = sprintf "color: %s" (Html.ofC4b C4b.White)                
            let children = AList.collecti (fun i v -> viewTree (i::path) v model) group.subNodes    

            let activeIcon =
                adaptive {                    
                    let! group  =  group.key
                    return if (s.id = group) then "circle icon" else "circle thin icon"
                }

            let setActive = Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)
            let activeAttributes = 
                Groups.mkGroupAttributeMap activeIcon (GroupsMessage setActive)
                                   
            let toggleIcon = Mod.constant "unhide icon" //group.visible |> Mod.map(fun toggle -> if toggle then "unhide icon" else "hide icon")                

            let toggleAttributes = Groups.mkGroupAttributeMap toggleIcon (GroupsMessage(Groups.ToggleGroup path))
               
            let desc =
                div [style color] [       
                    Incremental.text group.name
                    Incremental.i activeAttributes AList.empty |> UI.wrapToolTip "Set active"
                        
                    i [clazz "plus icon"
                       onMouseClick (fun _ -> 
                         GroupsMessage(Groups.AddGroup path))] []
                    |> UI.wrapToolTip "Add Group"                                          

                    Incremental.i toggleAttributes AList.empty |> UI.wrapToolTip "Toggle Group"
                   // GuiEx.iconCheckBox group.visible (GroupsMessage(Groups.ToggleGroup path))
                ]
                 
            let itemAttributes =
                amap {
                    yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                    let! selected = group.expanded
                    if selected 
                    then yield clazz "icon outline open folder"
                    else yield clazz "icon outline folder"
                    yield style "overflow-y : visible"
                } |> AttributeMap.ofAMap
            
            let childrenAttribs =
                amap {
                    yield clazz "list"
                    let! isExpanded = group.expanded
                    if isExpanded then yield style "visible"
                    else yield style "hidden"
                }         

            let singleSelect = 
                    fun (s:MSurface,path:list<Index>) -> 
                        Action.GroupsMessage(Groups.SingleSelectLeaf (path, s.guid |> Mod.force, ""))

            let multiSelect = 
                fun (s:MSurface,path:list<Index>) -> 
                    Action.GroupsMessage(Groups.AddLeafToSelection (path, s.guid |> Mod.force, ""))

            let lift = fun (a:Groups.Action) -> (GroupsMessage a)

            yield div [ clazz "item"] [
                    Incremental.i itemAttributes AList.empty
                    div [ clazz "content" ] [                         
                        div [ clazz "description noselect"] [desc]
                        Incremental.div (AttributeMap.ofAMap childrenAttribs) (                          
                            alist { 
                                let! isExpanded = group.expanded
                                if isExpanded then yield! children
                                
                                if isExpanded then 
                                    yield! viewSurfacesInGroups path model singleSelect multiSelect lift group.leaves
                            }
                        )  
                                
                    ]
            ]
                
        }


    let viewSurfacesGroups (model:MSurfaceModel) = 
        require GuiEx.semui (
            Incremental.div 
              (AttributeMap.ofList [clazz "ui celled list"]) 
              (viewTree [] model.surfaces.rootGroup model.surfaces)            
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

   // let noSurfaceSelected = div[ style "font-style:italic"][ text "no surface selected" ]

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
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let x = match surf with | MSurfaces s -> s | _ -> failwith "wrong type"
                                       
                    return SurfaceProperties.view x |> UI.map SurfacePropertiesMessage
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map SurfacePropertiesMessage 
        }                          
        
    let surfaceGroupProperties (model:MSurfaceModel) =
        adaptive {                                
            return (Groups.viewUI model.surfaces|> UI.map GroupsMessage)
        } 

    let viewColorCorrectionTools (model:MSurfaceModel) =
        adaptive {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let colorCorrection = match surf with | MSurfaces s -> s.colorCorrection | _ -> failwith "wrong type"
                    return ColorCorrectionProperties.view colorCorrection |> UI.map ColorCorrectionMessage
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map ColorCorrectionMessage 
        }                          

    let viewColorLegendTools (model:MSurfaceModel) =
        adaptive {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let scalar = 
                      match surf with 
                       | MSurfaces s -> s.selectedScalar 
                       | _ -> failwith "wrong type"

                    let! scalar = scalar
                    match scalar with
                        | Some s -> return FalseColorLegendApp.UI.viewDefinedScalarsLegendTools s.colorLegend |> UI.map ScalarsColorLegendMessage
                        | None -> return div[ style "font-style:italic"][ text "no scalar in properties selected" ] |> UI.map ScalarsColorLegendMessage 
                                  
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map ScalarsColorLegendMessage 
        }                          

    let showColorLegend (model:MSurfaceModel) =
        alist {
            let! guid = model.surfaces.singleSelectLeaf
            
            match guid with
                | Some i -> 

                    let leaf = model.surfaces.flat |> AMap.find i |> Mod.bind(id)
                    let! surf = leaf 
                    let scalar = match surf with | MSurfaces s -> s.selectedScalar | _ -> failwith "wrong type"
                    let! scalar = scalar
                    match scalar with
                        | Some s -> yield FalseColorLegendApp.Draw.createColorLegendScalars s.colorLegend
                        | None -> yield div[ style "font-style:italic"][ text "no scalar selected" ]
                                  
                | None -> yield div[ style "font-style:italic"][ text "no surface selected" ]
        }  

    let surfacesLeafButtonns (model:MSurfaceModel) = 
        let ts = model.surfaces.activeChild
        let sel = model.surfaces.singleSelectLeaf
        adaptive {  
            let! ts = ts
            let! sel = sel
            match sel with
                | Some x -> return (Groups.viewLeafButtons ts |> UI.map GroupsMessage)
                | None -> return div[ style "font-style:italic"][ text "no surface selected" ] |> UI.map GroupsMessage
        } 

    let surfacesGroupButtons (model:MSurfaceModel) = 
        let ts = model.surfaces.activeGroup
        adaptive {  
            let! ts = ts
            return (Groups.viewGroupButtons ts |> UI.map GroupsMessage)
        } 
    
    let surfaceUI (model:MSurfaceModel) =
        let item2 = 
            model.surfaces.lastSelectedItem 
                |> Mod.bind (fun x -> 
                    match x with 
                        | SelectedItem.Group -> surfaceGroupProperties model
                        | _ -> viewSurfaceProperties model
                )
        let buttons = 
            model.surfaces.lastSelectedItem 
                |> Mod.bind (fun x -> 
                    match x with 
                        | SelectedItem.Group -> surfacesGroupButtons model
                        | _ -> surfacesLeafButtonns model
                )
        div[][                            
            yield GuiEx.accordion "Surfaces" "Cubes" true [ viewSurfacesGroups model ]
            yield GuiEx.accordion "Properties" "Content" false [
              Incremental.div AttributeMap.empty (
                alist { 
                  let! item = item2
                  yield item}
              )
            ]
            yield GuiEx.accordion "Actions" "Asterisk" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
                ]  
                
            yield GuiEx.accordion "Color Correction" "file image outline" false [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(viewColorCorrectionTools model))] 

            yield GuiEx.accordion "Scalars ColorLegend" "paint brush" true [
                    Incremental.div AttributeMap.empty (AList.ofModSingle(viewColorLegendTools model))] 

            
        ]
    
    