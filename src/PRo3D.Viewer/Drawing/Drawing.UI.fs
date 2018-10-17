namespace PRo3D.DrawingApp

open System
open System.IO

open System.Windows.Forms
open System.Text
open System.Net.WebSockets
open System.Threading
open System.Collections.Concurrent    

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI

open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open Aardvark.Rendering.Text
open Aardvark.VRVis
open Aardvark.VRVis.Opc

open Aardvark.UI
open Aardvark.UI.Primitives    
open PRo3D
open PRo3D.Drawing
open PRo3D.Groups
open MBrace.FsPickler.Json   

module UI =
      open Aardvark.Base.Incremental
      //open Aardvark.UI.TreeViewApp

      let viewAnnotationToolsHorizontal (model:MDrawingModel) =
          Html.Layout.horizontal [
              Html.Layout.boxH [ i [clazz "large Write icon"][] ]
              Html.Layout.boxH [ Html.SemUi.dropDown model.geometry SetGeometry ]
              Html.Layout.boxH [ Html.SemUi.dropDown model.projection SetProjection ]
              Html.Layout.boxH [ ColorPicker.view model.color |> UI.map ChangeColor; div[][] ]
              Html.Layout.boxH [ Numeric.view' [InputBox] model.thickness |> UI.map ChangeThickness ]
            //  Html.Layout.boxH [ Html.SemUi.dropDown model.semantic SetSemantic ]                
          ]
                    
      let mkColor (model : MGroupsModel) (a : MAnnotation) =
        let color =  
            model.selectedLeaves
                |> ASet.map(fun x -> x.id = a.key)
                |> ASet.contains true
                |> Mod.bind (function x -> if x then Mod.constant C4b.VRVisGreen else a.color.c)
        
        color

      let isSingleSelect (model : MGroupsModel) (a : MAnnotation) =
          model.singleSelectLeaf |> Mod.map( fun x -> 
              match x with 
                | Some selected -> selected = a.key
                | None -> false )

      let annotationMenu = 
          div [ clazz "ui dropdown item"] [
              text "Annotations"
              i [clazz "dropdown icon"][] 
              div [ clazz "menu"] [                    
                  div [ clazz "ui inverted item"; Dialogs.onChooseFiles Load;
                  clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));" )][
                          text "Load"
                  ]
                  div [ clazz "ui inverted item"; onMouseClick (fun _ -> Clear)][
                          text "Clear List"
                  ]
                  div [ clazz "ui inverted item"; Dialogs.onSaveFile SaveCSV;
                      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements as .csv'}]}));")][
                          text "Save .csv"
                  ]
              ]
          ]                    

      let viewAnnotationImportExport (m:MDrawingModel) =
         div [clazz "ui buttons inverted"] [
                              
              button [clazz "ui icon button"; Dialogs.onSaveFile Save;
                      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements'}]}));")] [
                  i [clazz "save icon"] [] ] |> UI.wrapToolTipBottom "Save Annotations"
                              
              button [clazz "ui icon button"; Dialogs.onChooseFiles Load;
                  clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onchoose', top.aardvark.dialog.showOpenDialog({properties: ['openFile']}));") ] [
                  i [clazz "folder outline icon"] [] ] |> UI.wrapToolTipBottom "Load Annotations"
              
              button [clazz "ui icon button"; onMouseClick (fun _ -> Clear)] [
                  i [clazz "file outline icon"] [] ] |> UI.wrapToolTipBottom "Clear Annotation List"

              button [clazz "ui icon button"; Dialogs.onSaveFile SaveCSV;
                      clientEvent "onclick" ("top.aardvark.processEvent('__ID__', 'onsave', top.aardvark.dialog.showSaveDialog({options: [{title: 'Save Measurements as .csv'}]}));")] [
                  i [clazz "save icon inverted"] [] ] |> UI.wrapToolTipBottom "Save .csv"                                                                
          ] 

      let viewAnnotations (annotations : alist<MAnnotation>) : alist<DomNode<Action>> =
        
          annotations |> AList.map(fun a ->
            div [clazz "item"] [
              i [clazz "large Sticky Note middle aligned icon"][]
              div [clazz "content"] [
                div [clazz "header"][Incremental.text (a.geometry |> Mod.map(string))]
                div [clazz "description"][Incremental.text (a.points |> AList.count |> Mod.map(string))]
              ]
            ])
                       
      let viewAnnotationsInGroup (path:list<Index>) (model:MGroupsModel)(singleSelect : MAnnotation*list<Index> -> 'outer)(multiSelect : MAnnotation*list<Index> -> 'outer)(lift : Groups.Action -> 'outer) (annotations: alist<MAnnotation>) : alist<DomNode<'outer>> =                         
        annotations 
          |> AList.map(fun a ->
            
            let singleSelect = fun _ -> singleSelect(a,path)
            let multiSelect  = fun _ -> multiSelect(a,path)
            
            let ac = sprintf "color: %s" (Html.ofC4b C4b.Gray)
            
            let visibleIcon = 
              amap {
                yield onMouseClick (fun _ -> lift <| Groups.ToggleChildVisibility (a.key,path))
                let! visible = a.visible
                if visible then 
                  yield clazz "unhide icon" 
                else 
                  yield clazz "hide icon"
              } |> AttributeMap.ofAMap

            let iconAttributes =
              amap {                  
                yield clazz "large Sticky Note middle aligned icon"
                yield onMouseClick (multiSelect)

                let! c = mkColor model a
                yield style (sprintf "color: %s" (Html.ofC4b c))
              } |> AttributeMap.ofAMap
            
            let headerAttributes =
              amap {
                yield onClick singleSelect
                let! selected = isSingleSelect model a
                yield if selected then style "text-transform:uppercase" else style "text-transform:none"
              } |> AttributeMap.ofAMap

            let headerText = 
              adaptive {
                let! geometry = a.geometry
                let! semantic = a.semantic

                return 
                  match semantic with
                    | Semantic.None -> 
                      geometry  |> sprintf "%A"
                    | _ -> 
                      semantic  |> sprintf "%A"
            
              }

            div [clazz "item"] [
              Incremental.i iconAttributes AList.empty
              div [clazz "content"] [                  
                let desc   = Mod.map2 (fun x y -> sprintf "#Points: %A | taken on: %A" x y) (a.points |> AList.count) a.surfaceName
                
                //header
                yield Incremental.div (AttributeMap.ofList [clazz "header"; style ac]) (
                    alist {                          
                        yield div[][
                          Incremental.span headerAttributes ([Incremental.text headerText] |> AList.ofList)
                          Incremental.i visibleIcon AList.empty |> UI.wrapToolTip "Toggle Visible"
                          i [clazz "binoculars icon"; onClick (fun _ -> FlyToAnnotation a.key)][] |> UI.wrapToolTip "FlyTo"
                        ]
                    } 
                )

                //description
                yield div [clazz "description";style ac ] [Incremental.text desc]      
              ]
            ]                            
          )     
            
      let rec viewTree path (group : MNode) (model : MGroupsModel) (lookup : amap<Guid, MAnnotation>) : DomNode<Action> =
                                                  
          let icon =
              adaptive {
                  let! s = model.activeGroup    
                  let active = s.id
                  let! group  =  group.key

                  return if (active = group) then "circle icon" else "circle thin icon"
              }
                
          let map = Groups.mkGroupAttributeMap icon (GroupsMessage(Groups.ActiveGroup (group.key |> Mod.force, path, group.name |> Mod.force)))
             
          let toggleIcon =
              adaptive {
                  let! toggle = group.visible
                  return if toggle then "unhide icon" else "hide icon"
              }

          let toggleMap = Groups.mkGroupAttributeMap toggleIcon (GroupsMessage(Groups.ToggleGroup path))
          
          let color = sprintf "color: %s" (Html.ofC4b C4b.White)
          let desc =
              div [style color] [       
                  Incremental.text group.name
                  Incremental.i map AList.empty |> UI.wrapToolTip "Set active"
                  
                  i [clazz "plus icon"
                     onMouseClick (fun _ -> 
                       GroupsMessage(Groups.AddGroup path))] [] 
                  |> UI.wrapToolTip "Add Group"                        

                  Incremental.i toggleMap AList.empty |> UI.wrapToolTip "Toggle Group"
              ]
           
          let itemAttributes =
              amap {
                  yield onMouseClick (fun _ -> Action.GroupsMessage(Groups.ToggleExpand path))
                  let! expanded = group.expanded
                  if expanded then 
                    yield clazz "icon outline open folder"
                  else 
                    yield clazz "icon outline folder"
                  yield style "overflow-y : visible"
              } |> AttributeMap.ofAMap
          
          let childrenAttribs =
              amap {
                  yield clazz "list"
                  let! isExpanded = group.expanded
                  if (not isExpanded) then 
                    yield style "display:none"                             
              }         

          let singleSelect = 
              fun (a:MAnnotation,path:list<Index>) -> 
                  Action.GroupsMessage(Groups.SingleSelectLeaf (path, a.key, ""))

          let multiSelect = 
              fun (a:MAnnotation,path:list<Index>) -> 
                  Action.GroupsMessage(Groups.AddLeafToSelection (path, a.key, ""))

          let lift = fun (a:Groups.Action) -> (GroupsMessage a)

          let subNodes = group.subNodes |> AList.mapi (fun i v -> viewTree (i::path) v model lookup) 
          
          let annos = 
            group.leaves 
              |> AList.map(fun x -> lookup |> AMap.find x |> Mod.force) 
              |> viewAnnotationsInGroup path model singleSelect multiSelect lift                

          let nodes = annos |> AList.append subNodes

          div [ clazz "item"] [
            Incremental.i itemAttributes AList.empty
            div [ clazz "content" ] [
              div [ clazz "description noselect"] [desc]
              Incremental.div (AttributeMap.ofAMap childrenAttribs) (nodes)
            ]
          ]
          
      let toMAnnotation (leaf : MLeaf) =
        match leaf with 
        | MAnnotations a -> a
        | _ -> failwith "wrong type"

      let viewAnnotationGroups (model:MDrawingModel) = 
        let a = 
          model.annotations.flat 
            |> AMap.map(fun _ v -> (v |> Mod.force) |> toMAnnotation)              
         
        require GuiEx.semui (
            let tree = viewTree [] model.annotations.rootGroup model.annotations a
            //Incremental.div (AttributeMap.ofList [clazz "ui list"]) ([])
            div [clazz "ui list"] [tree]
        )  
