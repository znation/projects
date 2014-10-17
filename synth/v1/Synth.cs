using System;
using System.Collections.Generic;
using System.Threading;
using SdlDotNet.Core;
using SdlDotNet.Graphics;
using SdlDotNet.Input;

namespace Synth
{
    internal static class Synth
    {
        internal static readonly Random RANDOM = new Random();
        internal const uint SAMPLES_PER_SECOND = 44100;
        internal const ushort BITS_PER_SAMPLE = 16;
        internal const ushort CHANNELS = 1;
        internal const int SLEEP_DELAY = 10;
        private static List<Thread> threads = new List<Thread>();

        [STAThread]
        public static void Main()
        {
            Note.RenderNotes();
            Mixer.InitializeChannels();
            Go();
        }

        private static void StartThreads()
        {
            threads.Add(new Thread(Mixer.MixLoop));
            foreach (Thread t in threads)
            {
                t.Start();
            }
        }

        private static void Tick(object sender, TickEventArgs e)
        {

        }

        private static void KeyboardHandler(object sender, KeyboardEventArgs e)
        {
            String key = e.KeyboardCharacter.ToUpper();
            char c = key[0];
            if (c >= 'A' && c <= 'G')
            {
                if (e.Down)
                    Mixer.Play(key, 4);
                else
                    Mixer.Stop(key, 4);
            }
        }

        private static void Go()
        {
            StartThreads();
            Video.SetVideoMode(400, 300);
            Video.WindowCaption = "Synth";

            Sequencer s = new Sequencer(4, 90);
            for (int i = 0; i < 2; i++)
            {
                s.Add("C", 4, 4, 0 + i, 0);
                s.Add("E", 4, 4, 0 + i, 0);

                s.Add("D", 4, 4, 0 + i, 1);
                s.Add("F", 4, 4, 0 + i, 1);

                s.Add("E", 4, 4, 0 + i, 2);
                s.Add("G", 4, 4, 0 + i, 2);

                s.Add("C", 4, 4, 0 + i, 3);
                s.Add("E", 4, 4, 0 + i, 3);
            }
            for (int i = 0; i < 2; i++)
            {
                s.Add("E", 4, 4, 2 + i, 0);
                s.Add("C", 4, 4, 2 + i, 0);

                s.Add("F", 4, 4, 2 + i, 1);
                s.Add("Bb", 3, 4, 2 + i, 1);

                s.Add("G", 4, 2, 2 + i, 1);
                s.Add("C", 4, 2, 2 + i, 1);
            }

            threads.Add(s.Play());
            
            Events.Quit += new EventHandler<QuitEventArgs>(Quit);
            Events.Tick += new EventHandler<TickEventArgs>(Tick);
            Events.KeyboardUp += new EventHandler<KeyboardEventArgs>(KeyboardHandler);
            Events.KeyboardDown += new EventHandler<KeyboardEventArgs>(KeyboardHandler);
            Events.Run();
        }

        private static void Quit(object sender, QuitEventArgs e)
        {
            foreach (Thread t in threads)
            {
                t.Abort();
            }
            Events.QuitApplication();
        }
    }

}
