namespace PRo3DGui

open System
open System.Drawing
open System.Windows.Forms

module SplashScreen =
    
    let spawn () =
        let load = new Form()
        load.FormBorderStyle <- FormBorderStyle.None
        let pic = new System.Windows.Forms.PictureBox()
        pic.ImageLocation <- @"logo.png"
        load.Name <- "Splash";
        pic.Dock <- DockStyle.Fill
        load.Controls.Add(pic)
        load.ShowIcon <- false;
        load.ShowInTaskbar <- false;
        load.Size <- System.Drawing.Size(538,189)
        load.StartPosition <- System.Windows.Forms.FormStartPosition.CenterScreen
        load.ResumeLayout(false);
        load.PerformLayout();
        load.Show()
        load

