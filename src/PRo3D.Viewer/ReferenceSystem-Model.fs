namespace PRo3D.ReferenceSystem

open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI
open PRo3D

[<DomainType>]
type ReferenceSystem = {    

    origin      : V3d
    north       : V3dInput
    up          : V3dInput
    isVisible   : bool       
    size        : NumericInput
    scaleChart    : plist<string>
    selectedScale : string
    planet        : Planet
}

type ReferenceSystemConfig<'a> = {
    arrowLength    : Lens<'a,double>
    arrowThickness : Lens<'a,double>
    nearPlane      : Lens<'a,double>
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

    let initNum2 = {
        value   = 1.0
        min     = 0.0
        max     = 1000.0
        step    = 1.0
        format  = "{0:0.000}"
    }

    let initial = {
        origin        = V3d.IIO
        north         = setV3d V3d.IOO
        up            = setV3d V3d.OOI
        isVisible     = true              
        size          = initNum2
        scaleChart = ["100km"; "10km"; "1km";"100m";"10m";"2m";"1m";"10cm";"1cm";"1mm";"0.1mm"] |> PList.ofList
        selectedScale = "2m"
        planet = Planet.Mars
    }

    let initialConfig : ReferenceSystemConfig<ViewConfigModel> = {
        arrowLength =    ViewConfigModel.Lens.arrowLength    |. NumericInput.Lens.value
        arrowThickness = ViewConfigModel.Lens.arrowThickness |. NumericInput.Lens.value
        nearPlane = ViewConfigModel.Lens.nearPlane |. NumericInput.Lens.value
    }
