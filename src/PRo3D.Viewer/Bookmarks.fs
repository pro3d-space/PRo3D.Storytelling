namespace PRo3D

open Aardvark.Base
open Aardvark.Application
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Incremental

open System

open PRo3D

module Bookmarks = 
    open Aardvark.UI.Primitives.Mutable

    type Action =
        | AddBookmark 
        | UpdateCam of Guid  
        | Select of Guid
        | Remove of Guid
    
    let tryGet (ans:plist<Bookmark>) key = 
            ans |> Seq.tryFind(fun x -> x.key = key)

    let getNewBookmark (ccs:CameraControllerState) (count:int)= 
        let name = sprintf "bm%d" count
        {
            key = Guid.NewGuid()
            name = name
            camState = ccs
        }
        

    let update (model : BookmarksModel) (act : Action) (cam : Lens<'a,CameraControllerState>) (outerModel:Viewer.Model) =
        match act with
            | AddBookmark -> 
                let newBm = getNewBookmark (cam.Get outerModel) model.bookmarks.Count
                let bookmarks' = model.bookmarks |> PList.append newBm
                { model with bookmarks = bookmarks' }
            | UpdateCam id -> 
                let bm = tryGet model.bookmarks id
                match bm with
                    | Some b ->
                        let cam' = b.camState
                        cam.Set(outerModel,cam')
                        model
                    | None -> model
            | Select id ->
                let sb = tryGet model.bookmarks id   
                { model with selectedBookmark = sb }
            | Remove id ->
                let toRemove = model.bookmarks |> PList.toList |> List.tryFindIndex (fun d -> d.key = id)
                match toRemove with
                    | Some index ->
                        let b' = model.bookmarks |> PList.removeAt index
                        { model with bookmarks = b'}
                    | None -> model     

    let mkColor (activeId : IMod<Guid>) (bmId : IMod<Guid>) = 
        if activeId = bmId then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White


    let viewGUI = 
        
         div [clazz "ui buttons inverted"] [
                    onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                        button [clazz "ui icon button"; attribute "data-content" "Add Bookmark"; onMouseClick (fun _ -> AddBookmark)] [
                                i [clazz "plus icon"] [] ] 
                    )
                    //onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                    //    button [clazz "ui icon button"; attribute "data-content" "Clear Bookmarks"; onMouseClick (fun _ -> Clear)] [
                    //            i [clazz "file outline icon"] [] ]
                    //)
                ] 

    let view (model : MBookmarksModel) =
    
      Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : auto;max-height: 30%"    ]) (                      
                        alist {
                            for b in model.bookmarks do
                             
                                //let bgc = sprintf "color: %s" (Html.ofC4b C4b.White)

                                let icon =
                                    adaptive {
                                        let! active = model.selectedBookmark
                                        let key = match active with
                                                    | Some x -> x.key
                                                    | None -> Guid.Empty |> Mod.constant
                                        let! key= key
                                        let! bm  =  b.key

                                        return if (key = bm) then "circle icon" else "circle thin icon"
                                    }
                      
                                let map = 
                                    amap {
                                        let! icon = icon
                                        yield clazz icon
                                        yield onClick (fun _ -> Select (b.key |> Mod.force))
                                    } |> AttributeMap.ofAMap

                                //let select = fun _ -> Select (b.key |> Mod.force)

                                yield div [clazz "item"] [
                                        //i [clazz "large Sticky Note middle aligned icon";onClick select; style bgc][]
                                        div [clazz "content"] [
                                            let desc = b.name
                                            yield Incremental.div (AttributeMap.ofList [clazz "header"])(
                                                alist {
                                                    yield Incremental.i map AList.empty |> UI.wrapToolTip "Set active"
                                                    yield i [clazz "home icon"                                                
                                                             onClick (fun _ -> UpdateCam (b.key |> Mod.force))
                                                             ][] |> UI.wrapToolTip "FlyTo"               
                                                    yield i [clazz "Remove icon"
                                                             onClick (fun _ -> Remove (b.key |> Mod.force))
                                                             ][]  |> UI.wrapToolTip "Remove"          
                                                              
                                                    } 
                                            ) 
                                            yield div [clazz "description"] [Incremental.text desc]      
                                        ]
                                    ]
                               
                        }     
                ) 

    module UI =
        let view (model:MBookmark) =
            let view = model.camState.view |> Mod.force
            require Html.semui (
                Html.table [                                                
                    Html.row "Pos:"     [Incremental.text (view.Location.ToString("0.00") |> Mod.constant )]
                    Html.row "LookAt:"  [text (view.Forward.ToString("0.00"))]
                    Html.row "Up:"      [text (view.Up.ToString("0.00"))]
                    Html.row "Sky:"     [text (view.Sky.ToString("0.00"))]
                ]
            )

      