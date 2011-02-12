using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SdlDotNet.Graphics;
using SdlDotNet.Core;
using SdlDotNet.Audio;
using System.IO;
using System.Threading;

namespace Synth
{
    internal class Synth
    {
        internal static readonly Random RANDOM = new Random();
        internal const uint SAMPLES_PER_SECOND = 44100;
        internal const ushort BITS_PER_SAMPLE = 16;
        internal const ushort CHANNELS = 1;

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

            Sound sound = Wave.FromHz(4000, new Frequency(110, .3),
                                            new Frequency(220, .6),
                                            new Frequency(440, .9),
                                            new Frequency(880, .6),
                                            new Frequency(1760, .3));
            sound.Play(false);

            /*
            for (uint i = 0; i < 4; i++)
            {
                Sound sound = Wave.FromHz(2000, (uint)(110 * Math.Pow(2, i)), 1.0);
                sound.Play(false);
                Thread.Sleep(5000);
            }
            */
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
