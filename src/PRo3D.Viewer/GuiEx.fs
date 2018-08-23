namespace PRo3D

open Aardvark.UI

module GuiEx =
    
    let semui = 
      [ 
          { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
          { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
      ]

    module Layout =
        let boxH ch = td [clazz "collapsing"; style "padding: 0px 5px 0px 0px"] ch

        let horizontal ch = table [clazz "ui table inverted segment"; style "backgroundColor: transparent"] [ tbody [] [ tr [] ch ] ]

        let finish<'msg> = td[] []

    let menu (c : string )(s : string)(entries : list<string * list<DomNode<'msg>>>) =                    
        div [ clazz c; style s ] (            
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
                js "onclick"        "$('.sidebar')
                                        .sidebar('setting', 'transition', 'overlay')
                                        .sidebar('toggle');"
            ] [
                i [clazz "content icon"] [] 
                span [clazz "text"] [text "Menu"]
            ]
       
        [
            let c = [pushButton()] @ rest

            yield 
                menu "ui vertical inverted sidebar menu" "" sectionsAndItems
            yield
                div [clazz "ui bottom fixed menu"] top 
            yield 
                div [clazz "pusher"] c
        ]

    let adornerMenu (sectionsAndItems : list<string * list<DomNode<'msg>>>)(top : list<DomNode<'msg>>) (rest : list<DomNode<'msg>>) =       
        [
            div[clazz "ui top small fixed menu blarg"] [
                yield 
                    a [clazz "item blarg topmenu"] [
                        i [clazz "large sidebar icon"] []
                    ]
                for t in top do
                    yield div [clazz "item topmenu"] [t]
                
            ]
            div [clazz "ui bottom attached segment blarg pushable";] [
                menu "ui vertical inverted sidebar menu" "" sectionsAndItems
                div [clazz "pusher"; style "background:#000;"] rest
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

    let accordion text' icon active content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"
           // let arrow = if active then 
                                    
            onBoot "$('#__ID__').accordion();" (
                div [clazz "ui inverted segment"] [
                    div [clazz "ui inverted accordion fluid"] [
                        div [clazz title; style "background-color: #282828"] [
                                i [clazz ("dropdown icon")][]
                                text text'                                
                                div[style "float:right"][i [clazz (icon + " icon")][]]
                                
                        ]
                        div [clazz content;  style "overflow-y : auto; "] content' //max-height: 35%
                    ]
                ]
            )
