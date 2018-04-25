namespace Aardvark.Opc

open System
open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.Base.Incremental

open PatchLod

module DefaultMetrics =
    
    let mars (self : AdaptiveToken) (v : IMod<Trafo3d>) (pr : IMod<Trafo3d>) (p : RenderPatch) (c : IMod<LodParameters>) =
        let lodParams = c.GetValue self
       
        let pos     = (v.GetValue self).Backward.C3.XYZ
        let bb      = p.info.GlobalBoundingBox.Transformed(lodParams.trafo)
        let closest = bb.GetClosestPointOn(pos)
        let dist    = (closest - pos).Length

        let unitPxSize = lodParams.frustum.right / (float lodParams.size.X / 2.0)
        let px = (lodParams.frustum.near * p.triangleSize) / dist
                
    //    Log.warn "%f to %f - avgSize: %f" px (unitPxSize * lodParams.factor) p.triangleSize
        px > unitPxSize * (exp lodParams.factor)

    let tunnel (self : AdaptiveToken) (v : IMod<Trafo3d>) (pr : IMod<Trafo3d>) (p : RenderPatch) (c : IMod<LodParameters>)=
        let m = p.trafo.GetValue self
        let view = v.GetValue self
        let proj = pr.GetValue self
        let vp   = view * proj
        let mvp = m * vp
        let area = computeScreenSpaceArea p.info.LocalBoundingBox mvp
                    
        if p.level <= 1 then true
        else log area > log 0.8


[<AutoOpen>]
module Configurations =

    type FileName = string
    type OpcScene = { 
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
                        System.IO.Directory.GetDirectories(@"E:\Aardwork\Exomars\_SCN\GardenCity") 
                        |> Seq.collect System.IO.Directory.GetDirectories
                boundingBox      = Box3d.Parse("[[-2486972.923809925, 2288926.293124544, -275794.479790366], [-2486972.675580427, 2288926.581096648, -275793.402162172]]") 
                near             = 0.1
                far              = 10000.0
                speed            = 3.0
                lodDecider       = DefaultMetrics.mars
            }        

        let perjen () =
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.FromOrthoNormalBasis(V3d.IOO, -V3d.OOI, V3d.OIO)
                patchHierarchies = System.IO.Directory.GetDirectories(@"I:\demo\perjen\08b_Dibit-Projekt_OPC-Export_2.5mm\2011_Bestandsaufnahme\OPC")
                boundingBox      = Box3d.Parse("[[16034.950038118, 222765.173562629, 802.291122034], [16037.049953225, 222767.420825919, 802.333067577]]")
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.tunnel
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
                patchHierarchies = System.IO.Directory.GetDirectories(@"D:\opc\perjen\2011_Bestandsaufnahme\OPC")
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

    module Empty = 
         let mars () = 
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.Identity
                patchHierarchies = [@"D:\_WORK\Aardwork\Exomars\_Scenes\Dingo_Gap_2\Surface\MSL_Mastcam_Sol_529_id_47596\OPC_000_000"]
                boundingBox      = Box3d.Parse("[[3376372.058677169, -325173.566694686, -121309.194857123], [3376385.170513898, -325152.282144333, -121288.943956908]]")
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.mars
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
                near             = 80.0
                far              = 200000000.0
                speed            = 32768.0
                lodDecider       = DefaultMetrics.mars
            }



        let testSurf () = 
            { 
                useCompressedTextures = true
                preTransform     = Trafo3d.Identity
                patchHierarchies = 
                        System.IO.Directory.GetDirectories(@"\\euclid\Assets\opc\mastcam\","MSL_Mastcam_Sol_107_id_46992") 
                        |> Seq.collect System.IO.Directory.GetDirectories
                boundingBox      = Box3d.Parse("[[-2490165.052869780, 2285846.454951702, -271376.741140408], [-2490140.981372352, 2285877.000650112, -271365.128791433]]") 
                near             = 0.1
                far              = 10000.0
                speed            = 5.0
                lodDecider       = DefaultMetrics.mars
            }