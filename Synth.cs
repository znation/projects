using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SdlDotNet.Graphics;
using SdlDotNet.Core;
using SdlDotNet.Audio;
using System.IO;

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

            //using (FileStream fs = new FileStream("test.wav", FileMode.Create))
            //{
            //    using (BinaryWriter bw = new BinaryWriter(fs))
            //    {
            //        bw.Write(Wave.Random);
            //    }
            //}
        }

        private Synth()
        {
            Video.SetVideoMode(400, 300);
            Video.WindowCaption = "Synth";

            Sound sound = new Sound(Wave.Random);
            sound.Play(true);
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
