namespace PRo3D

open Aardvark.UI

module GuiEx =
    
    module Layout =
        let boxH ch = td [clazz "collapsing"; style "padding: 0px 5px 0px 0px"] ch

        let horizontal ch = table [clazz "ui table inverted segment"; style "backgroundColor: transparent"] [ tbody [] [ tr [] ch ] ]

        let finish<'msg> = td[] []

    let menu (c : string )(entries : list<string * list<DomNode<'msg>>>) =
            div [ clazz c ] (
                entries |> List.map (fun (name, children) ->
                    div [ clazz "item"] [ 
                        b [] [text name]
                        div [ clazz "menu" ] (
                            children |> List.map (fun c ->
                                div [clazz "item"] [c]
                            )
                        )
                    ]
                )
            )
       
    let adornerMenu (sectionsAndItems : list<string * list<DomNode<'msg>>>)(top : list<DomNode<'msg>>) (rest : list<DomNode<'msg>>) =            
        let pushButton() = 
            div [
                clazz "ui black big launch right attached fixed button menubutton"
                js "onclick"        "$('.sidebar').sidebar('toggle');"
            ] [
                i [clazz "content icon"] [] 
                span [clazz "text"] [text "Menu"]
            ]
        [
            let c = [pushButton()] @ rest

            yield 
                menu "ui vertical inverted sidebar menu" sectionsAndItems
            yield
                div [clazz "ui bottom inverted fixed menu"] top 
            yield 
                div [clazz "pusher"] c
        ]
