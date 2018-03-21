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
  open Aardvark.Base.Rendering
  open Aardvark.Base.Incremental

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
      let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }

      leaves 
        |> List.map(fun (dir,patch) -> (Patch.load dir patch.info,dir, patch.info)) 
        |> List.map(fun ((a,b),c,d) -> (a,c,d))
        |> List.map (fun (g,dir,info) -> 

           let texPath = Patch.extractTexturePath dir info 0
           let tex = FileTexture(texPath,config) :> ITexture

           Sg.ofIndexedGeometry g
             |> Sg.trafo (Mod.constant info.Local2Global)
             //|> Sg.diffuseTexture DefaultTextures.checkerboard            
             |> Sg.diffuseTexture (Mod.constant tex)
             //|> Sg.texture DefaultSemantic.DiffuseColorTexture 
             //|> Sg.effect [
             //   DefaultSurfaces.trafo |> toEffect
             //   DefaultSurfaces.constantColor C4f.White |> toEffect
             //   DefaultSurfaces.diffuseTexture |> toEffect                    
             //]
           )
        |> Sg.ofList
        
    sg
