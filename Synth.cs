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
        private List<Thread> threads = new List<Thread>();

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

        private void StartThreads()
        {
            threads.Add(new Thread(Sequencer.PlayLoop));
            foreach (Thread t in threads)
            {
                t.Start();
            }
        }

        private Synth()
        {
            Note.RenderNotes();
            StartThreads();
            Video.SetVideoMode(400, 300);
            Video.WindowCaption = "Synth";
            Sequencer.Enqueue("C", 4, 1000, DateTime.Now);
            Sequencer.Enqueue("D", 4, 1000, DateTime.Now.AddMilliseconds(1000));
            Sequencer.Enqueue("E", 4, 1000, DateTime.Now.AddMilliseconds(2000));
            Sequencer.Enqueue("F", 4, 1000, DateTime.Now.AddMilliseconds(3000));
            Sequencer.Enqueue("G", 4, 1000, DateTime.Now.AddMilliseconds(4000));
        }

        private void Go()
        {
            Events.Quit += new EventHandler<QuitEventArgs>(this.Quit);
            Events.Run();
        }

        private void Quit(object sender, QuitEventArgs e)
        {
            foreach (Thread t in threads)
            {
                t.Abort();
            }
            Events.QuitApplication();
        }
    }

}
