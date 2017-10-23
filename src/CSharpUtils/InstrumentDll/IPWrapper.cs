using Aardvark.Base;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ViewPlanner
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
    }
}
