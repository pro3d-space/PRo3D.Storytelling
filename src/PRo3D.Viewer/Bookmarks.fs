namespace PRo3D

module Bookmarks = 
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.Base.Incremental

    open System

    open PRo3D.BookmarksX
    open PRo3D.Navigation2
    open Aardvark.UI.Primitives.Mutable
    open Aardvark.SceneGraph.SgFSharp.Sg

    type Action =
        | AddBookmark 
        | UpdateCam of Guid  
        | Select of Guid
        | Remove of Guid
        | SetBookmarkName          of string
    
    let tryGet (ans:plist<Bookmark>) key = 
            ans |> Seq.tryFind(fun x -> x.key = key)

    let getNewBookmark (navigation:NavigationModel) (count:int) =  
        let name = sprintf "bm%d" count
        {
            key = Guid.NewGuid()
            name = name
            navigationState = navigation
        }
        
    
    let update (model : BookmarksModel) (act : Action) (navigation : Lens<'a,NavigationModel>) (outerModel:'a) : ('a*BookmarksModel) = 
        match act with
            | AddBookmark -> 
                let newBm = getNewBookmark (navigation.Get outerModel) model.bookmarks.Count
                (outerModel,{ model with bookmarks = model.bookmarks |> PList.append newBm})
                
            | UpdateCam id -> 
                let bm = tryGet model.bookmarks id
                match bm with
                    | Some b ->
                        let navigation' = b.navigationState 
                        let newOuterModel = navigation.Set(outerModel,navigation')
                        (newOuterModel,model)                      
                    | None -> (outerModel,model)
            | Select id ->
                let sb = tryGet model.bookmarks id   
                (outerModel,{ model with selectedBookmark = sb })
            | Remove id ->
                let toRemove = model.bookmarks |> PList.toList |> List.tryFindIndex (fun d -> d.key = id)
                match toRemove with
                    | Some index ->
                        let b' = model.bookmarks |> PList.removeAt index
                        (outerModel, { model with bookmarks = b'})
                    | None -> (outerModel, model) 
            | SetBookmarkName n -> 
                match model.selectedBookmark with
                        | Some x -> let b' = { x with name = n }
                                    let bookmarks' = 
                                        model.bookmarks.AsList |> List.updateIf (fun x -> x.key = b'.key) (fun _ -> b')   
                                    (outerModel, {model with bookmarks = bookmarks' |> PList.ofList })
                        | None -> (outerModel,model)


    let mkColor (activeId : IMod<Guid>) (bmId : IMod<Guid>) = 
        if activeId = bmId then Mod.constant C4b.VRVisGreen else Mod.constant C4b.White


    let viewGUI = 
        
         div [clazz "ui buttons inverted"] [
                    onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (
                        button [clazz "ui icon button"; attribute "data-content" "Add Bookmark"; onMouseClick (fun _ -> AddBookmark)] [
                                i [clazz "plus icon"] [] ] 
                    )
                ] 

    let view (model : MBookmarksModel) =
    
      Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list inverted segment"; style "overflow-y : visible;max-height: 30%" ]) (                      
                        alist {
                            for b in model.bookmarks do

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

                              
                                yield div [clazz "item"] [
                                        div [clazz "content"] [
                                            let desc = b.name
                                            yield Incremental.div (AttributeMap.ofList [clazz "header"])(
                                                alist {
                                                    yield Incremental.i map AList.empty |> UI.wrapToolTipRight "Set active"
                                                    yield i [clazz "home icon"                                                
                                                             onClick (fun _ -> UpdateCam (b.key |> Mod.force))
                                                             ][] |> UI.wrapToolTipRight "FlyTo"               
                                                    yield i [clazz "Remove icon"
                                                             onClick (fun _ -> Remove (b.key |> Mod.force))
                                                             ][]  |> UI.wrapToolTipRight "Remove"          
                                                              
                                                    } 
                                            ) 
                                            yield div [clazz "description"] [Incremental.text desc]      
                                        ]
                                    ]
                               
                        }     
                ) 

    module UI =
        let view (model:MBookmark) =
            let view = model.navigationState.camera.view |> Mod.force
            require Html.semui (
                Html.table [  
                    Html.row "Change Name:"[Html.SemUi.textBox model.name SetBookmarkName ]
                    Html.row "Pos:"     [Incremental.text (view.Location.ToString("0.00") |> Mod.constant )]
                    Html.row "LookAt:"  [text (view.Forward.ToString("0.00"))]
                    Html.row "Up:"      [text (view.Up.ToString("0.00"))]
                    Html.row "Sky:"     [text (view.Sky.ToString("0.00"))]
                ]
            )

       