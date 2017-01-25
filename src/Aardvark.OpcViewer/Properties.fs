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


[<AutoOpen>]
module Configurations =

    type FileName = string
    type OpcViewer = { 
        patchHierarchies : seq<FileName> 
        boundingBox      : Box3d
        preTransform     : Trafo3d 
        near             : float
        far              : float
        speed            : float
        lodDecider       : PatchLod.LodDecider
        useCompressedTextures : bool
    }

    // JR_export: 142GB
    // mola: 15.1GB
    // 
    
     module OrtiPc =

        let mars () = 
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.Identity
                patchHierarchies = 
                      System.IO.Directory.GetDirectories(@"D:\_WORK\Aardwork\Exomars\_Scenes\MSL_Mastcam_Sol318\Surface") 
                      |> Seq.collect System.IO.Directory.GetDirectories
                boundingBox      = Box3d.Parse("[[-2490136.401072104, 2285878.413405098, -271417.754880811], [-2490134.590369504, 2285880.810397286, -271414.202594255]]") 
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.mars
            }

    module HarriPc =

        let mars () = 
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.Identity
                patchHierarchies = 
                      System.IO.Directory.GetDirectories(@"D:\JR_Export\OPCs\Surface","MSL_Mastcam_Sol_*") 
                      |> Seq.collect System.IO.Directory.GetDirectories
                boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.mars
            }

        let mola () = 
            { 
              useCompressedTextures = false
              preTransform     = Trafo3d.Identity
              patchHierarchies = System.IO.Directory.GetDirectories(@"D:\Mars_MGS_MOLA_DEM_mosaic_global_0.01") 
              boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
              near             = 100.0
              far              = 100000000.0
              speed            = 32768.0
              lodDecider       = DefaultMetrics.mars
            }

        let perjen () =
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.FromOrthoNormalBasis(V3d.IOO, -V3d.OOI, V3d.OIO)
                patchHierarchies = System.IO.Directory.GetDirectories(@"D:\perjen\2011_Bestandsaufnahme\OPC")
                boundingBox      = Box3d.Parse("[[16034.950038118, 222765.173562629, 802.291122034], [16037.049953225, 222767.420825919, 802.333067577]]")
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.tunnel
            }

    module Relative =

        let mars () = 
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.Identity
                patchHierarchies = 
                      System.IO.Directory.GetDirectories(@"..\..\..\JR_Export\OPCs\Surface","MSL_Mastcam_Sol_*") 
                      |> Seq.collect System.IO.Directory.GetDirectories
                boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.mars
            }

        let mola () = 
            let molaPrefix = @"..\..\..\..\JR_LandingSites\LandingSites\Surface\"
            { 
              useCompressedTextures = false
              preTransform     = Trafo3d.Identity
              patchHierarchies = System.IO.Directory.GetDirectories ( Path.combine [molaPrefix; @"Mars_MGS_MOLA_DEM_mosaic_global_0.01" ] )
              boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
              near             = 100.0
              far              = 100000000.0
              speed            = 32768.0
              lodDecider       = DefaultMetrics.mars
            }

        let perjen () =
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.FromOrthoNormalBasis(V3d.IOO, -V3d.OOI, V3d.OIO)
                patchHierarchies = System.IO.Directory.GetDirectories(@"..\..\..\perjen\2011_Bestandsaufnahme\OPC")
                boundingBox      = Box3d.Parse("[[16034.950038118, 222765.173562629, 802.291122034], [16037.049953225, 222767.420825919, 802.333067577]]")
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.tunnel
            }

    module Network =
    
        let perjen () =
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.FromOrthoNormalBasis(V3d.IOO, -V3d.OOI, V3d.OIO)
                patchHierarchies = System.IO.Directory.GetDirectories(@"\\euclid\Assets\opc\perjen\2011_Bestandsaufnahme\OPC")
                boundingBox      = Box3d.Parse("[[16034.950038118, 222765.173562629, 802.291122034], [16037.049953225, 222767.420825919, 802.333067577]]")
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.tunnel
            }

        let mars () = 
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.Identity
                patchHierarchies = 
                        System.IO.Directory.GetDirectories(@"\\euclid\Assets\opc\mastcam","MSL_Mastcam_Sol_*") 
                        |> Seq.collect System.IO.Directory.GetDirectories
                boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.mars
            }

        let mola () = 
            { 
                useCompressedTextures = false
                preTransform     = Trafo3d.Identity
                patchHierarchies = System.IO.Directory.GetDirectories(@"\\euclid\Assets\opc\Mars_MGS_MOLA_DEM_mosaic_global_0.01") 
                boundingBox      = Box3d.Parse("[[-2490137.081477872, 2285880.860800383, -271377.497591413], [-2490136.544600942, 2285881.420182895, -271376.955064115]]") 
                near             = 100.0
                far              = 100000000.0
                speed            = 32768.0
                lodDecider       = DefaultMetrics.mars
            }