module ConfigTest

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open Aardvark.Base.Incremental

module LittleConfigTest =
   
    type Action = SetArrowSize of double

    type InnerConfig<'a> =  
        {
            arrowSize : Lens<'a,double>
        }
    
    let update<'a> (bigConfig : 'a) (innerConfig : InnerConfig<'a>) (a : Action) : 'a =
        match a with
            | SetArrowSize d -> 
                innerConfig.arrowSize.Set(bigConfig, d)

    let view<'a> (bigConfig : IMod<'a>) (innerConfig : InnerConfig<'a>) : DomNode<Action> =
        let arrowSize = bigConfig |> Mod.map (fun b -> innerConfig.arrowSize.Get b)
        let button =
            button [onClick (fun _ -> SetArrowSize (Mod.force arrowSize))] [text "hallo"]
        button

module BigConfigTest =

   open ConfigLensTest
   open LittleConfigTest
   
   type Action = InnerAction of LittleConfigTest.Action


   let innerConfig = { arrowSize = BigConfig.Lens.arrowSize }

   let update (a : Action) (m : BigConfig) =
        match a with
            | InnerAction(inner)-> LittleConfigTest.update m innerConfig inner

   let view (m : MBigModel) : DomNode<Action> =
        LittleConfigTest.view m.bigConfig innerConfig |> UI.map InnerAction