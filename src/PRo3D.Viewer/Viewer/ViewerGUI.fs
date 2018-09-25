namespace PRo3D.Viewer

open Aardvark.Service

open System
open System.Windows.Forms
open System.Diagnostics

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Operators
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text

open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives.Sg
open Aardvark.VRVis

open MBrace.FsPickler
open System.IO

open PRo3D
open PRo3D.Groups
open PRo3D.ReferenceSystem

module Gui = 

  let pitchAndBearing (r:MReferenceSystem) (view:IMod<CameraView>) =
          adaptive {
            let! up    = r.up.value
            let! north = r.northO//r.north.value   
            let! v     = view

            return (DipAndStrike.pitch up v.Forward, DipAndStrike.bearing up north v.Forward)
          }

  let textOverlays (m : MReferenceSystem) (cv : IMod<CameraView>) = 
    div [js "oncontextmenu" "event.preventDefault();"] [ 
      let planet = m.planet |> Mod.map(fun x -> sprintf "%s" (x.ToString()))  
    
      
      let pnb = pitchAndBearing m cv
    
      let pitch    = pnb |> Mod.map(fun (p,_) -> sprintf "%s deg" (p.ToString("0.00")))
      let bearing  = pnb |> Mod.map(fun (_,b) -> sprintf "%s deg" (b.ToString("0.00")))
      
      let position = cv |> Mod.map(fun x -> x.Location.ToString("0.00"))
    
      let spericalc = 
        Mod.map2 (fun (a : CameraView) b -> 
          CooTransformation.getLatLonAlt(a.Location) b ) cv m.planet
      
      let lon = spericalc |> Mod.map(fun x -> sprintf "%s deg" ((x.longitude).ToString("0.00")))
      let lat = spericalc |> Mod.map(fun x -> sprintf "%s deg" ((x.latitude).ToString("0.00")))
      let alt = spericalc |> Mod.map(fun x -> sprintf "%s m" ((x.altitude).ToString("0.00")))
      
      let style' = "color: white; font-family:Consolas;"
    
      yield div [clazz "ui"; style "position: absolute; top: 15px; left: 15px; float:left" ] [
          //arrowOverlay
          yield table [] [
              tr[][
                  td[style style'][Incremental.text planet]
              ]
              tr[][
                  td[style style'][text "Bearing: "]
                  td[style style'][Incremental.text bearing]
              ]
              tr[][
                  td[style style'][text "Pitch: "]
                  td[style style'][Incremental.text pitch]
              ]
              tr[][
                  td[style style'][text "Position: "]
                  td[style style'][Incremental.text position]
              ]
              tr[][
                  td[style style'][text "Longitude: "]
                  td[style style'][Incremental.text lon]
              ]
              tr[][
                  td[style style'][text "Latitude: "]
                  td[style style'][Incremental.text lat]
              ]
              tr[][
                  td[style style'][text "Altitude: "]
                  td[style style'][Incremental.text alt]
              ]
          ]
      ]                              
    ]

  module TopMenu =
    
    let private importSurface (m:MModel) =
      [
         text "Import Surface"
         i [clazz "dropdown icon"][] 
         div [ clazz "menu"] [
             div [ clazz "ui inverted item"; 
               Dialogs.onChooseFiles ImportSurface;
               clientEvent "onclick" ("parent.aardvark.processEvent('__ID__', 'onchoose', parent.aardvark.dialog.showOpenDialog({properties: ['openDirectory', 'multiSelections']}));") ][
               text "OPC"
             ]
             div [ clazz "ui inverted item"; 
               Dialogs.onChooseFiles ImportObject;
               clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ][
               text "OBJ"
             ]
         ]
      ]

    let private scene (m:MModel) = 
      let save = 
        match (m.scene.scenePath|> Mod.force) with
            | Some p ->
                 div [ clazz "ui inverted item"; onMouseClick (fun _ -> SaveScene p)][
                  text "Save"
                ]
            | None ->
                div [ clazz "ui inverted item"; Dialogs.onSaveFile1 SaveScene (m.scene.scenePath |> Mod.force);
                  clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Scene', defaultPath :'C:\Users\laura\VRVis\PRo3D_25062018\src\test.xml'}]}));")][
                  text "Save"
                ]
      [
        text "Scene" 
        i [clazz "dropdown icon"][]
      
        div [ clazz "menu"] [
            save
            //div [ clazz "ui inverted item"; Dialogs.onSaveFile1 SaveScene (m.scene.scenePath |> Mod.force);
            //      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Scene', defaultPath :'C:\Users\laura\VRVis\PRo3D_25062018\src\test.xml'}]}));")][
            //      text "Save"
            //]
            div [ clazz "ui inverted item"; Dialogs.onSaveFile SaveAs;
                  clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({properties: []}));") ][
                  text "Save as..."
            ]
            div [ clazz "ui inverted item"; Dialogs.onChooseFiles OpenScene;
                  clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));")][
                  text "Open"
            ]
            div [ clazz "ui inverted item"; onMouseClick (fun _ -> NewScene)][
                  text "New"
            ]
            div[ clazz "ui inverted item" ] [
                onBoot """$('#__ID__').popup({inline:true,hoverable:true, position   : 'right center'});""" (
                    text "Recent"
                )
        
                div [clazz "ui flowing popup bottom left transition hidden"] [
                    Incremental.div (AttributeMap.ofList [clazz "ui link list"]) (
                        aset {
                            for (_,s) in (m.recent.recentScenes |> AMap.toASet) do
                                 yield a [clazz "item inverted";
                                          onClick (fun _ -> LoadScene (s.path |> Mod.force))] [
                                    span [style "color:black"] [Incremental.text s.name]
                                 ]
                        } |> AList.ofASet
                    )
                ]
            ]
         ]
      ]

    let menu (m : MModel) = 
      div [clazz "menu-bar"] [
        // menu
        div [ clazz "ui top menu"; style "z-index: 10000; padding:0px; margin:0px"] [
          onBoot "$('#__ID__').dropdown('on', 'hover');" (
            div [ clazz "ui dropdown item"; style "padding:0px 5px"] [
              i [clazz "large sidebar icon"; style "margin:0px 2px"][] 
              
              div [ clazz "ui menu"] [
                  div [ clazz "ui dropdown item"; style "width: 150px"] [] //spacer as workaround for taboverdraw
                  div [ clazz "ui dropdown item"] (scene m)
              
                  div [ clazz "ui dropdown item"] (importSurface m)

                  div [ clazz "ui dropdown item"] [
                      text "Import"
                      i [clazz "dropdown icon"][] 
                      div [ clazz "menu"] [
                          div [ clazz "ui item";
                              Dialogs.onChooseFiles ImportAnnotationGroups;
                              clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ][
                              text "Annotations Groups"
                          ]
                          div [ clazz "ui item";
                              Dialogs.onChooseFiles ImportSurfaceTrafo;
                              clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ] [
                              text "Surface Trafos"
                          ]
                      ]
                  ]
              
                  DrawingApp.UI.annotationMenu  |> UI.map DrawingMessage;
                  
                  div[ clazz "ui item" ] [
                      onBoot """$('#__ID__').popup({inline:true,hoverable:true, position   : 'right center'});""" (
                          text "Closed Pages"
                      )
              
                      div [clazz "ui flowing popup bottom left transition hidden"] [
                          Incremental.div (AttributeMap.ofList [clazz "ui link list"]) (
                              aset {
                                  let! closedP = m.scene.closedPages
                                  for cp in closedP do
                                       yield a [clazz "item inverted";
                                                onClick (fun _ -> AddPage cp)] [
                                          span [style "color:black"] [Incremental.text (cp.id |> Mod.constant)]
                                       ]
                              } |> AList.ofASet
                          )
                      ]
                  ]
              ] 
            ]
          )
        ]
      ]

  module Annotations =
    
    let viewAnnotationProperties (model :MModel) =
            let view = (fun leaf ->
                match leaf with
                  | MAnnotations ann -> AnnotationProperties.view ann
                  | _ -> div[style "font-style:italic"][ text "no annotation selected" ])
            
            model.drawing.annotations |> Groups.viewSelected view AnnotationMessage
               

    let viewDipAndStrike (model:MModel) = 
        let view = (fun leaf ->
            match leaf with
              | MAnnotations ann -> DipAndStrike.viewUI ann
              | _ -> div[style "font-style:italic"][ text "no annotation selected" ])

        model.drawing.annotations |> Groups.viewSelected view DnSProperties    
        
    let viewDnSColorLegendUI (model:MModel) = 
      model.drawing.colorLegend 
        |> FalseColorLegendApp.viewDnSLegendProperties DnSColorLegendMessage 
        |> Mod.constant


    let annotationLeafButtonns' (model:MModel) = 
        let ts = model.drawing.annotations.activeChild
        let sel = model.drawing.annotations.singleSelectLeaf
        adaptive {  
            let! ts = ts
            let! sel = sel
            match sel with
                | Some x -> return (Groups.viewLeafButtons ts |> UI.map AnnotationGroupsMessageViewer)
                | None -> return div[style "font-style:italic"][ text "no annotation group selected" ]
        }      
        
    let annotationLeafButtonns (model:MModel) =           
      Mod.map2(fun ts sel -> 
        match sel with
         | Some _ -> (Groups.viewLeafButtons ts |> UI.map AnnotationGroupsMessageViewer)
         | None -> div[style "font-style:italic"][ text "no annotation group selected" ]
        ) model.drawing.annotations.activeChild model.drawing.annotations.singleSelectLeaf
        
    let annotationGroupProperties (model:MModel) =                            
      Groups.viewUI model.drawing.annotations 
        |> UI.map AnnotationGroupsMessageViewer 
        |> Mod.constant
    
    let annotationGroupButtons (model:MModel) = 
      model.drawing.annotations.activeGroup 
        |> Mod.map (fun x -> Groups.viewGroupButtons x |> UI.map AnnotationGroupsMessageViewer)            
        
    let annotationUI (m : MModel) = 
      let prop = 
          m.drawing.annotations.lastSelectedItem
              |> Mod.bind (fun x -> 
                  match x with 
                    | SelectedItem.Group -> annotationGroupProperties m
                    | _ -> viewAnnotationProperties m 
              )
      let buttons = 
          m.drawing.annotations.lastSelectedItem
              |> Mod.bind (fun x -> 
                  match x with 
                    | SelectedItem.Group -> annotationGroupButtons m
                    | _ -> annotationLeafButtonns m 
              )
      
      div [][                
          GuiEx.accordion "Annotations" "Write" true [
              Groups.viewSelectionButtons |> UI.map AnnotationGroupsMessageViewer
              DrawingApp.UI.viewAnnotationGroups m.drawing |> UI.map DrawingMessage
          ]
          GuiEx.accordion "Properties" "Content" true [
              Incremental.div AttributeMap.empty (AList.ofModSingle prop)                                        
          ]
         
          GuiEx.accordion "Actions" "Asterisk" true [
              Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
          ]     
          
          GuiEx.accordion "Dip&Strike" "Calculator" false [
              Incremental.div AttributeMap.empty (AList.ofModSingle(viewDipAndStrike m))] 
         
          GuiEx.accordion "Dip&Strike ColorLegend" "paint brush" false [
              Incremental.div AttributeMap.empty (AList.ofModSingle(viewDnSColorLegendUI m))] 
          ]    

  module Bookmarks =
    let bookmarkGroupProperties (model:MModel) =                                       
          Groups.viewUI model.scene.bookmarksModel.bookmarks 
            |> UI.map BookmarkUIMessage 
            |> Mod.constant
            
        
                        
    let viewBookmarkProperties (model:MModel) = 
        let view = (fun leaf ->
            match leaf with
              | MBookmarks bm -> Bookmarks.UI.view bm
              | _ -> div[style "font-style:italic"][ text "no bookmark selected" ])

        model.scene.bookmarksModel.bookmarks |> Groups.viewSelected view BookmarkUIMessage

    let bookmarksLeafButtonns (model:MModel) = 
        let ts = model.scene.bookmarksModel.bookmarks.activeChild
        let sel = model.scene.bookmarksModel.bookmarks.singleSelectLeaf
        adaptive {  
            let! ts = ts
            let! sel = sel
            match sel with
                | Some _ -> return (Groups.viewLeafButtons ts |> UI.map BookmarkUIMessage)
                | None -> return div[style "font-style:italic"][ text "no bookmark selected" ]            
        } 
    
    let bookmarksGroupButtons (model:MModel) = 
        let ts = model.scene.bookmarksModel.bookmarks.activeGroup
        adaptive {  
            let! ts = ts
            return (Groups.viewGroupButtons ts |> UI.map BookmarkUIMessage)
        } 
    
    let bookmarkUI (m:MModel) = 
        let item2 = 
            m.scene.bookmarksModel.bookmarks.lastSelectedItem 
                |> Mod.map (fun x -> 
                    match x with 
                      | SelectedItem.Group -> bookmarkGroupProperties m
                      | _ -> viewBookmarkProperties m 
                )
        let buttons = 
            m.scene.bookmarksModel.bookmarks.lastSelectedItem 
                |> Mod.bind (fun x -> 
                    match x with 
                      | SelectedItem.Group -> bookmarksGroupButtons m
                      | _ -> bookmarksLeafButtonns m 
                )
        div [][
            br []
            Bookmarks.UI.viewGUI |> UI.map BookmarkMessage
    
            GuiEx.accordion "Bookmarks" "Write" true [
                //Groups.viewSelectionButtons |> UI.map BookmarkUIMessage
                Bookmarks.viewBookmarksGroups m.scene.bookmarksModel |> UI.map BookmarkMessage 
            ]                
            GuiEx.accordion "Properties" "Content" true [
                Incremental.div AttributeMap.empty ( item2 |> Mod.bind id |> AList.ofModSingle)
            ] 
            GuiEx.accordion "Actions" "Asterisk" true [
                Incremental.div AttributeMap.empty (AList.ofModSingle (buttons))
            ]
        ]
    