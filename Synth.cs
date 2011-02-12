using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SdlDotNet.Graphics;
using SdlDotNet.Core;
using SdlDotNet.Audio;
using System.IO;
using System.Threading;
using System.Diagnostics;

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
            //        bw.Write(Bytes.FromHz(1000, new Note("B", 4)));
            //    }
            //}
        }

        private Synth()
        {
            Video.SetVideoMode(400, 300);
            Video.WindowCaption = "Synth";

            char c;
            ushort i;
            for (c = 'C', i = 3; i <=5; c++)
            {
                if (c == 'H')
                {
                    c = 'A';
                    i++;
                }

                Sound sound = Sound.FromHz(200, new Note(c.ToString(), i));
                sound.Play(false);
                Thread.Sleep(210);

                if (c == 'C' && i == 5)
                    break;
            }
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
