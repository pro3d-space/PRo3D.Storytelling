namespace Aardvark.Opc

open Aardvark.Base
open Aardvark.Base.Incremental

open PatchLod

module DefaultMetrics =

    let mars (self : IAdaptiveObject) (vp : IMod<Trafo3d>) (p : RenderPatch) =
        let m = p.trafo.GetValue self
        let vp = vp.GetValue self
        let mvp = m * vp
        let area = computeScreenSpaceArea p.info.LocalBoundingBox mvp

        area > 3.5

    let tunnel (self : IAdaptiveObject)  (vp : IMod<Trafo3d>)  (p : RenderPatch) =
        let m = p.trafo.GetValue self
        let vp = vp.GetValue self
        let mvp = m * vp
        let area = computeScreenSpaceArea p.info.LocalBoundingBox mvp
                    
        if p.level <= 1 then true
        else log area > log 0.8