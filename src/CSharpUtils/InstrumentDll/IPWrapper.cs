using Aardvark.Base;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Exomars.Base.ViewPlanner
{
    /// <summary>
    /// Contains functions and marshalling structs to call routines from the InstrumentPlatforms.dll 
    /// provided by Joanneum Research. The provided dll 
    /// </summary>
    public static partial class IPWrapper
    {        
        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern int Init(string configDir, string logDir);

        #region GetPlatformNames()
        //[DllImport("TestLib.dll")]
        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern uint GetNrOfAvailablePlatforms();

        // [DllImport("TestLib.dll", CharSet = CharSet.Ansi)]
        [DllImport(@".\bin\InstrumentPlatforms.dll", CharSet = CharSet.Ansi)]
        public static extern void GetAvailablePlatforms([MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] IntPtr[] platformNames, uint numberOfPlatforms);

        //[DllImport("TestLib.dll")]
        [DllImport(@".\bin\InstrumentPlatforms.dll", CharSet = CharSet.Ansi)]
        public static extern int InitPlatform(ref SPlatform poPlatform, uint nNrOfPlatformPointsOnGround, uint nNrOfPlatformInstruments, uint nNrOfPlatformAxes);
        #endregion

        #region GetPlatform(string platformId)     
        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern uint GetNrOfPlatformPointsOnGround(string pcPlatformId);
        
        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern uint GetNrOfPlatformInstruments(string pcPlatformId);

        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern uint GetNrOfPlatformAxes(string pcPlatformId);

        [DllImport(@".\bin\InstrumentPlatforms.dll")]
        public static extern int UpdatePlatform(ref SPlatform poPlatform);
        #endregion

        //#region debugging functions
        //[DllImport("TestLib.dll")]
        //public static extern void DisplayHelloFromDLL(ref STransformation trafo);

        //[DllImport("TestLib.dll")]
        //public static extern void PassAxes(IntPtr axes, uint numOfAxes);

        //[DllImport("TestLib.dll")]
        //public static extern void PassPoint(ref SPoint3D point);    

        //[DllImport("TestLib.dll")]
        ////public static extern void DisplayTrafoMatrix(ref IntPtr array);
        //public static extern void DisplayTrafoMatrix([In] [MarshalAs(UnmanagedType.LPArray, SizeConst = 16)] double[] array);

        //[DllImport("TestLib.dll")]
        //public static extern void PassInstruments(IntPtr instruments, int numOfInstruments);
        //#endregion

        public class ArrayBuffer
        {
            public IntPtr Pointer { get; private set; }
            public IntPtr[] ElementPointers { get; private set; }

            public int Count { get { return ElementPointers.Length; } }

            public ArrayBuffer(IntPtr pointer, IntPtr[] elementPointers)
            {
                Pointer = pointer;
                ElementPointers = elementPointers;
            }
        }

        public static void DoStuff()
        {
            //var t = new STransformation
            //{
            //    m_modified = 0,
            //    m_pcTransfName = ("Trans"),
            //    m_pcSourceFrame = ("Source"),
            //    m_pcTargetFrame = ("Target"),
            //    m_oHelmertTransfMatrix = new STransformationMatrix()
            //    {
            //        m_adElement = new double[] { 1, 2, 3, 4, 3, 4, 5, 6, 5, 6, 7, 8, 7, 8, 9, 10 }
            //    }
            //};

            ////  DisplayHelloFromDLL(ref t);
            ////  var array = t.m_oHelmertTransfMatrix.m_adElement;
            ////  DisplayTrafoMatrix(array);
            //Report.Line("Modified value " + t.m_modified.ToString());

            //var axes = new SAxis[] { new SAxis() { m_pcAxisId = "horizontal" },
            //                         new SAxis() { m_pcAxisId = "vertical" },
            //                         new SAxis() { m_pcAxisId = "diagonal" }};

            //var axes2 = new SAxis[] { new SAxis() { m_pcAxisId = "horizontal2" },
            //                         new SAxis() { m_pcAxisId = "vertical2" },
            //                         new SAxis() { m_pcAxisId = "diagonal2" }};

            //var axes_m = MarshalArray(axes);
            //var axes_m2 = MarshalArray(axes2);
            //var instruments = new SInstrument[] { new SInstrument()
            //{
            //    m_pcInstrumentName = "Geige",
            //    m_poInstrumentAxes = axes_m,
            //    m_nNrOfInstrumentAxes = (uint)axes.Count(),
            //    //m_oInstrumentIntrinsics = new SInstrumentIntrinsics()
            //    //{
            //    //    m_pcDistortionMapH = "DistortionHorizontal"
            //    //}
            //}, new SInstrument()
            //{
            //    m_pcInstrumentName = "CHello",
            //    m_poInstrumentAxes = axes_m2,
            //    m_nNrOfInstrumentAxes = (uint)axes2.Count(),
            //    //m_oInstrumentIntrinsics = new SInstrumentIntrinsics()
            //    //{
            //    //    m_pcDistortionMapH = "DistortionHorizontal"
            //    //}
            //}};

            //var instrument_m = MarshalArray(instruments);
            //PassInstruments(instrument_m, instruments.Count());
            //Report.Line("sent data");

            //var instremensRec = UnMarshalArray<SInstrument>(instrument_m);
            //foreach (var ins in instremensRec)
            //{
            //    var axesArray = UnMarshalArray<SAxis>(ins.m_poInstrumentAxes);
            //    foreach (var ax in axesArray)
            //    {
            //        Report.Line(ax.m_pcAxisId);
            //    }                
            //}

            //Marshal.FreeHGlobal(instrument_m);
        }
    }
}
