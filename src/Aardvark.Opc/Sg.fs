namespace Aardvark.Opc

module Sg2 = 
  
  open Aardvark.Base  
  open Aardvark.SceneGraph

  let createFlatISg (scene : OpcScene) : ISg =

    let bb = scene.boundingBox      
    let preTransform = scene.preTransform
    
    let patchHierarchies =
      [ 
        for h in scene.patchHierarchies do
          let p = Path.combine [h; @"Patches\patchhierarchy.xml" ]
          yield PatchHierarchy.load p
      ] 

    Sg.empty