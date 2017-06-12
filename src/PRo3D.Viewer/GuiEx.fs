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
            div [clazz "ui bottom attached segment pushable blarg"] [
                menu "ui vertical inverted sidebar menu" sectionsAndItems
                div [clazz "pusher"; style "background:#000"] rest
            ]

             
            
            
            //div [clazz "ui bottom fixed menu"] top 
             
            
        ]

    let tabbed attr content active =            
            onBoot "$('.menu .item').tab();" (
                div attr [
                    yield div [clazz "ui inverted segment top attached tabular menu"] [
                            for (name,ch) in content do
                                let active = if name = active then "inverted item active" else "inverted item"
                                yield Static.a [clazz active; attribute "data-tab" name][text name]
                          ]
                                        
                    for (name,ch) in content do
                        let classAttr = "ui inverted bottom attached tab segment"
                        let active = if name = active then (sprintf "%s %s" classAttr "active") else classAttr
                        yield div [clazz active; attribute "data-tab" name] [ch]         
                ]
            )
