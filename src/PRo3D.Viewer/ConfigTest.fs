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

    type MInnerConfig<'ma> =
        {
            getArrow : 'ma -> IMod<double>
        }
    
    // only returns bigconfig. realworld inner apps often have their own model and return an outer model.
    let update<'a> (bigConfig : 'a) (innerConfig : InnerConfig<'a>) (a : Action) : 'a =
        match a with
            | SetArrowSize d -> innerConfig.arrowSize.Set(bigConfig, d)

    let view<'ma> (mbigConfig : 'ma) (minnerConfig : MInnerConfig<'ma>) : DomNode<Action> =
        let arrowSize = minnerConfig.getArrow mbigConfig
        let button =
            button [onClick (fun _ -> SetArrowSize (Mod.force arrowSize + 1.0))] [text "increase arrowsize"]
        button

module BigConfigTest =

   open ConfigLensTest
   open LittleConfigTest
   
   type Action = InnerAction of LittleConfigTest.Action


   let innerConfig = { arrowSize = BigConfig.Lens.arrowSize }

   let update (a : Action) (m : BigConfig) =
        match a with
            | InnerAction(inner)-> LittleConfigTest.update m innerConfig inner

   let view (m : MBigConfig) : DomNode<Action> =

        let c : MInnerConfig<MBigConfig> =
            {
                getArrow = fun (x:MBigConfig) -> x.arrowSize
            }
        
        LittleConfigTest.view m c |> UI.map InnerAction