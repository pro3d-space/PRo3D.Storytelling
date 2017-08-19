namespace PRo3D.ReferenceSystem

open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI

[<DomainType>]
type ReferenceSystem = {    

    origin      : V3d
    north       : V3d
    up          : V3dInput
    isVisible   : bool       
}

type ReferenceSystemConfig<'a> = {
    arrowLength    : Lens<'a,double>
    arrowThickness : Lens<'a,double>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReferenceSystem =
    open PRo3D

    let initNum = {
        value   = 0.0
        min     = -1.0
        max     = +1.0
        step    = 0.1
        format  = "{0:0.000}"
    }

    let setV3d (v : V3d) = {
        x = { initNum with value = v.X }
        y = { initNum with value = v.Y }
        z = { initNum with value = v.Z }
        value = v    
    }

    let initArrowSize = {
        value   = 1.0
        min     = 0.1
        max     = 10.0
        step    = 0.1
        format  = "{0:0.0}"
    }

    let initial = {
        origin = V3d.OOO
        north = V3d.IOO
        up = setV3d V3d.OOI
        isVisible = true              
    }

    let initialConfig : ReferenceSystemConfig<ViewConfigModel> = {
        arrowLength =    ViewConfigModel.Lens.arrowLength    |. NumericInput.Lens.value
        arrowThickness = ViewConfigModel.Lens.arrowThickness |. NumericInput.Lens.value
    }
