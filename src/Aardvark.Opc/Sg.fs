namespace Aardvark.Opc

open System.IO
open Aardvark.Base

module OpcHelpers =

  let getPatchPositionPath (h:PatchHierarchy) patchName =
    let fileName = "positions.aara"
    Path.combine [h.baseDir; "patches"; patchName; fileName]

module Sg2 = 
  
  open Aardvark.Base  
  open Aardvark.SceneGraph
  open PatchLod

  let createFlatISg (scene : OpcScene) : ISg =
        
    let patchHierarchies =
      [ 
        for h in scene.patchHierarchies do
          let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
          yield PatchHierarchy.load p
      ] 

    let leaves = 
      patchHierarchies 
        |> List.collect(fun x ->  
           x.tree |> QTree.getLeaves |> Seq.toList |> List.map(fun y -> (x.baseDir, y)))

    let sg = 
      leaves 
        |> List.map(fun (dir,patch) -> Patch.load dir patch.info) 
        |> List.map fst 
        |> List.map (fun x -> 
           let sgi = Sg.ofIndexedGeometry x
           sgi
           
           )
        |> Sg.ofList
        
    sg
