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
using SdlDotNet.Input;

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
        }

        private void Tick(object sender, TickEventArgs e)
        {

        }

        private void KeyboardHandler(object sender, KeyboardEventArgs e)
        {
            String key = e.KeyboardCharacter.ToUpper();
            char c = key[0];
            if (c >= 'A' && c <= 'G')
            {
                if (e.Down)
                    Sequencer.Play(key, 4);
                else
                    Sequencer.Stop(key, 4);
            }
        }

        private void Go()
        {
            Events.Quit += new EventHandler<QuitEventArgs>(this.Quit);
            Events.Tick += new EventHandler<TickEventArgs>(this.Tick);
            Events.KeyboardUp += new EventHandler<KeyboardEventArgs>(this.KeyboardHandler);
            Events.KeyboardDown += new EventHandler<KeyboardEventArgs>(this.KeyboardHandler);
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
