using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SdlDotNet.Graphics;
using SdlDotNet.Core;
using SdlDotNet.Audio;

namespace Synth
{
    internal class Synth
    {
        internal static readonly Random Random = new Random();

        [STAThread]
        public static void Main()
        {
            Synth app = new Synth();
            app.Go();
        }

        private Synth()
        {
            Video.SetVideoMode(400, 300);
            Video.WindowCaption = "Synth";

            Sound sound = new Sound(Wave.Random);
            sound.Play(false);
        }

        private void Go()
        {
            Events.Quit += new EventHandler<QuitEventArgs>(this.Quit);
            Events.Run();
        }

        private void Quit(object sender, QuitEventArgs e)
        {
            Events.QuitApplication();
        }
    }

}
