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
        internal static readonly Random RANDOM = new Random();
        internal const UInt32 SAMPLES_PER_SECOND = 44100;
        internal const UInt16 BITS_PER_SAMPLE = 16;

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

            Sound sound = new Sound(Wave.FromHz(440));
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
