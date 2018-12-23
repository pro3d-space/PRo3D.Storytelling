namespace PRo3D.Minerva

open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Events

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

type MinervaAction =
  | PerformQueries

module App =

  let update (model : Model) (msg : MinervaAction) : Model =
      match msg with
      | PerformQueries -> 
        let data = model.queries |> MinervaGeoJSON.loadMultiple

        { model with data = data }

  let viewFeaturesGui (model :MModel) =
    
     let content =
        alist {
          yield button [clazz "ui button"; onClick (fun _ -> PerformQueries)][text "Load Minerva"]
        
          for f in model.data.features do
                
            let attr = [clazz "ui header small"]
              
            let attr2 = [clazz "ui large map pin inverted middle aligned icon"]
            
            let attr : AttributeMap<_> = attr |> AttributeMap.ofList
                
          //  let (FeatureId id) = f.properties.id
            let item =             
              div [clazz "ui inverted item"][
                i attr2 []
                div [clazz "ui content"] [
                  Incremental.div (attr) (AList.single (text "Feature"))
                  div [clazz "ui description"] [text (f.id.ToString())] //[text (id.Replace('_',' '))]
                ]            
              ]
            yield item 
        }
         
     Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) content  