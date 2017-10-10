using Aardvark.Base;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using ViewPlanner;

namespace ViewPlanner
{
    public static class IPWrapperConversions
    {
        //public static Rover ToRover(this IPWrapper.SPlatform self)
        //{
        //    var wheels = IPWrapper.UnMarshalArray<IPWrapper.SPoint3D>(
        //        self.m_poPointsOnGround, (int)self.m_nNrOfPlatformPointsOnGround).ToV3ds();

        //    var axes = IPWrapper.UnMarshalArray<IPWrapper.SAxis>(
        //        self.m_poPlatformAxes, (int)self.m_nNrOfPlatformAxes).ToAxes();

        //    var instruments = IPWrapper.UnMarshalArray<IPWrapper.SInstrument>(
        //        self.m_poPlatformInstruments, (int)self.m_nNrOfPlatformInstruments).ToInstruments();

        //    var rover = new Rover()
        //    {
        //        Id = self.m_pcPlatformId.ToStrAnsi(),
        //        Platform2Ground = self.m_oPlatform2Ground.m_oHelmertTransfMatrix.ToM44d(),
        //        WheelPositions = wheels,
        //        Instruments = instruments.ToDictionary(x => x.Id),
        //        Axes = axes.ToDictionary(x => x.Id),
        //        Box = self.m_oBoundingBox.ToBox3d(),
        //    };

        //    return rover;
        //}

        public static List<V3d> ToV3ds(this IPWrapper.SPoint3D[] self)
        {
            return self.Select(x => x.ToV3d()).ToList();
        }

        //public static List<Axis> ToAxes(this IPWrapper.SAxis[] self)
        //{
        //    return self.Select((axis, i) => new Axis()
        //    {
        //        Index = i,
        //        Id = axis.m_pcAxisId.ToStrAnsi(),
        //        Description = axis.m_pcAxisDescription.ToStrAnsi(),
        //        StartPoint = axis.m_oStartPoint.ToV3d(),
        //        EndPoint = axis.m_oEndPoint.ToV3d(),
        //        MinAngle = axis.m_fMinAngle.DegreesFromGons(),
        //        MaxAngle = axis.m_fMaxAngle.DegreesFromGons(),
        //        CurrentAngle = axis.m_fCurrentAngle.DegreesFromGons(),
        //    }).ToList();
        //}

        //public static List<Instrument> ToInstruments(this IPWrapper.SInstrument[] self)
        //{
        //    var list = self.Select((inst,i) =>
        //    {
        //        var pos = inst.m_oInstrumentExtrinsics.m_oPosition.ToV3d();
        //        var up = inst.m_oInstrumentExtrinsics.m_oUp.ToV3d();
        //        var lookAt = inst.m_oInstrumentExtrinsics.m_oLookAt.ToV3d();
        //        var box = inst.m_oInstrumentExtrinsics.m_oBoundingBox.ToBox3d();                           

        //        var focals = IPWrapper.UnMarshalArray<double>(
        //            inst.m_pdCalibratedFocalLengths, (int)inst.m_nNrOfCalibratedFocalLengths);               

        //        var extrinsics = new InstrumentExtrinsicParameters()
        //        {
        //            Position = pos,
        //            CamUp = up,
        //            CamLookAt = lookAt,
        //            Box = box,
        //        };

        //        var intrinsics = new InstrumentIntrinsicParameters()
        //        {
        //            HorizontalFieldOfView = inst.m_oInstrumentIntrinsics.m_dFieldOfViewH,
        //            VerticalFieldOfView = inst.m_oInstrumentIntrinsics.m_dFieldOfViewV,
        //            HorizontalResolution = inst.m_oInstrumentIntrinsics.m_nResolutionH,
        //            VerticalResolution = inst.m_oInstrumentIntrinsics.m_nResolutionV,
        //            HorizontalPrinciplePoint = inst.m_oInstrumentIntrinsics.m_dPrinciplePointH,
        //            VerticalPrinciplePoint = inst.m_oInstrumentIntrinsics.m_dPrinciplePointV,
        //            HorizontalFocalLengthPerPixel = inst.m_oInstrumentIntrinsics.m_dFocalLengthPerPxH,
        //            VerticalFocalLengthPerPixel = inst.m_oInstrumentIntrinsics.m_dFocalLengthPerPxV,
        //            //   HorizontalDistortionMap = inst.m_oInstrumentIntrinsics.m_pcDistortionMapH.ToStrAnsi(),
        //            //   VerticalDistortionMap = inst.m_oInstrumentIntrinsics.m_pcDistortionMapV.ToStrAnsi(),
        //            //   VignettingMap = inst.m_oInstrumentIntrinsics.m_pcvignettingMap.ToStrAnsi(),
        //        };

        //        return new Instrument()
        //        {
        //            Index = i,
        //            Id = inst.m_pcInstrumentName.ToStrAnsi(),
        //            CalibratedFocalLengths = focals.ToList(),
        //            CurrentFocalLength = inst.m_dCurrentFocalLengthInMm,
        //            Intrinsics = intrinsics,
        //            Extrinsics = extrinsics
        //        };


        //    }).ToList();            

        //    return list;
        //}

        public static V3d ToV3d(this IPWrapper.SPoint3D self)
        {
            return new V3d(self.m_dX, self.m_dY, self.m_dZ);
        }

        public static V3d ToV3d(this IPWrapper.SVector3D self)
        {
            return new V3d(self.m_dX, self.m_dY, self.m_dZ);
        }

        public static Box3d ToBox3d(this IPWrapper.SBoundingBox self)
        {
            var origin = self.m_oOriginBB.ToV3d();
            var edge1 = self.m_oEdge1.ToV3d();
            var edge2 = self.m_oEdge2.ToV3d();
            var edge3 = self.m_oEdge3.ToV3d();

            return new Box3d(origin,
                origin + edge1,
                origin + edge2,
                origin + edge3);
        }

        public static M44d ToM44d(this IPWrapper.STransformationMatrix self)
        {
            return new M44d(self.m_adElement);
        }

        public static IPWrapper.STransformationMatrix ToSTransfMatrix(this M44d self)
        {
            return new IPWrapper.STransformationMatrix()
            {
                m_adElement = self.Elements.ToArray()
            };
        }
    }
}
