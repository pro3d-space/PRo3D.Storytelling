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
       
    let adornerMenu2 (sectionsAndItems : list<string * list<DomNode<'msg>>>)(top : list<DomNode<'msg>>) (rest : list<DomNode<'msg>>) =            
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
                div [clazz "ui bottom fixed menu"] top 
            yield 
                div [clazz "pusher"] c
        ]

    let adornerMenu (sectionsAndItems : list<string * list<DomNode<'msg>>>)(top : list<DomNode<'msg>>) (rest : list<DomNode<'msg>>) =       
        [
            div[clazz "ui top attached demo menu blarg"] [
                a [clazz "item blarg"] [
                    i [clazz "sidebar icon"] []
                ]
                div [] top
                
            ]
            div [clazz "ui bottom attached segment pushable"] [
                menu "ui vertical inverted sidebar menu" sectionsAndItems
                div [clazz "pusher"; style "height:95%; background:#000"] rest
            ]

             
            
            
            //div [clazz "ui bottom fixed menu"] top 
             
            
        ]
