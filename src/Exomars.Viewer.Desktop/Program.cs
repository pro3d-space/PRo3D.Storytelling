using System;
using Eto;
using Eto.Forms;

namespace Exomars.Viewer.Desktop
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {
            new Application(new Eto.GtkSharp.Platform()).Run(new MainForm());
        }
    }
}
