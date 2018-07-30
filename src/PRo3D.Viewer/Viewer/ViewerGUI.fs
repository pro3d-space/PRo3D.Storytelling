namespace PRo3D.Viewer

open Aardvark.Service

open System
open System.Windows.Forms
open System.Diagnostics

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Operators
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text

open Aardvark.SceneGraph.Opc
open Aardvark.SceneGraph.SgPrimitives.Sg
open Aardvark.VRVis

open MBrace.FsPickler
open System.IO

open PRo3D
open PRo3D.ReferenceSystem

module Gui = 

  let pitchAndBearing (r:MReferenceSystem) (view:IMod<CameraView>) =
          adaptive {
            let! up    = r.up.value
            let! north = r.north.value   
            let! v     = view

            return (DipAndStrike.pitch up v.Forward, DipAndStrike.bearing up north v.Forward)
          }

  let textOverlays (m : MReferenceSystem) (cv : IMod<CameraView>) = 
    div [js "oncontextmenu" "event.preventDefault();"] [ 
      let planet = m.planet |> Mod.map(fun x -> sprintf "%s" (x.ToString()))  
    
      
      let pnb = pitchAndBearing m cv
    
      let pitch    = pnb |> Mod.map(fun (p,_) -> sprintf "%s deg" (p.ToString("0.00")))
      let bearing  = pnb |> Mod.map(fun (_,b) -> sprintf "%s deg" (b.ToString("0.00")))
      
      let position = cv |> Mod.map(fun x -> x.Location.ToString("0.00"))
    
      let spericalc = 
        Mod.map2 (fun (a : CameraView) b -> 
          CooTransformation.getLatLonAlt(a.Location) b ) cv m.planet
      
      let lon = spericalc |> Mod.map(fun x -> sprintf "%s deg" ((x.longitude).ToString("0.00")))
      let lat = spericalc |> Mod.map(fun x -> sprintf "%s deg" ((x.latitude).ToString("0.00")))
      let alt = spericalc |> Mod.map(fun x -> sprintf "%s deg" ((x.altitude).ToString("0.00")))             
      
      let style' = "color: white; font-family:Consolas;"
    
      yield div [clazz "ui"; style "position: absolute; top: 45px; float:left" ] [
          //arrowOverlay
          yield table [] [
              tr[][
                  td[style style'][Incremental.text planet]
              ]
              tr[][
                  td[style style'][text "Bearing: "]
                  td[style style'][Incremental.text bearing]
              ]
              tr[][
                  td[style style'][text "Pitch: "]
                  td[style style'][Incremental.text pitch]
              ]
              tr[][
                  td[style style'][text "Position: "]
                  td[style style'][Incremental.text position]
              ]
              tr[][
                  td[style style'][text "Longitude: "]
                  td[style style'][Incremental.text lon]
              ]
              tr[][
                  td[style style'][text "Latitude: "]
                  td[style style'][Incremental.text lat]
              ]
              tr[][
                  td[style style'][text "Altitude: "]
                  td[style style'][Incremental.text alt]
              ]
          ]
      ]                              
    ]