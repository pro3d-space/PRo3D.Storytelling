namespace PRo3D.Versioned

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open PRo3D
open Chiron

#nowarn "0686"

[<AutoOpen>]
module ChironExt =      
  type NumericInput
    with 
      static member FromJson(_ : NumericInput) = 
        json {
            let! value  = Json.read "value"
            let! min    = Json.read "min"
            let! max    = Json.read "max"
            let! step   = Json.read "step"
            let! format = Json.read "format"
      
            return {
                value   = value
                min     = min
                max     = max
                step    = step
                format  = format
            }
        }
      static member ToJson (x : NumericInput) =
        json {
            do! Json.write "value"  x.value
            do! Json.write "min"    x.min
            do! Json.write "max"    x.max
            do! Json.write "step"   x.step
            do! Json.write "format" x.format
          }

type Ext = Ext
 
type Ext with
     //NumericInput
     static member FromJson1 (ext : Ext, _ : NumericInput) = 
         json {
             let! value  = Json.read "value"
             let! min    = Json.read "min"
             let! max    = Json.read "max"
             let! step   = Json.read "step"
             let! format = Json.read "format"
     
             return {
                 value   = value
                 min     = min
                 max     = max
                 step    = step
                 format  = format
             }
         }
     static member ToJson1 (ext : Ext, v : NumericInput) = 
       json {
         do! Json.write "value"  v.value
         do! Json.write "min"    v.min
         do! Json.write "max"    v.max
         do! Json.write "step"   v.step
         do! Json.write "format" v.format
       }

     //ColorInput
     static member FromJson1 (ext : Ext, _ : ColorInput) = 
         json {
             let! c  = Json.read "color"                  
             return {
               c = c |> C4b.Parse
             }
         }
     static member ToJson1 (ext : Ext, v : ColorInput) = 
         json {
           do! Json.write "color"  (v.c.ToString())
         }
       
     //int
     static member FromJson1 (ext : Ext, v : int) = 
           json {       
               return 1
           }
module Ext =
  let inline fromJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b or ^e) : (static member FromJson1: ^e * ^a -> ^a Json)(Unchecked.defaultof<_>,a))

  let inline fromJson x =
    fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)

  let inline toJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b or ^e) : (static member ToJson1: ^e * ^a -> unit Json)(Unchecked.defaultof<_>,a))

  let inline toJson (x: 'a) =
    snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))
