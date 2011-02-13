using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SdlDotNet.Audio;
using System.Collections;
using System.Threading;

namespace Synth
{
    internal static class Mixer
    {
        private const int NUM_CHANNELS = 1024;
        private const int SLEEP_DELAY = 20;
        private static Stack<Channel> unused = new Stack<Channel>();
        private static Dictionary<Sound, Channel> playing = new Dictionary<Sound, Channel>();
        private static Queue playQueue = Queue.Synchronized(new Queue());
        private static Queue stopQueue = Queue.Synchronized(new Queue());

        internal static void InitializeChannels()
        {
            SdlDotNet.Audio.Mixer.ChannelsAllocated = NUM_CHANNELS;
            for (int i = 0; i < NUM_CHANNELS; i++)
            {
                unused.Push(SdlDotNet.Audio.Mixer.CreateChannel(i));
            }
        }

        internal static void MixLoop()
        {
            while (true)
            {
                while (playQueue.Count > 0)
                {
                    Sound s = (Sound)playQueue.Dequeue();
                    Channel c = unused.Pop();
                    playing[s] = c;
                    c.Play(s, true);
                }
                while (stopQueue.Count > 0)
                {
                    Sound s = (Sound)stopQueue.Dequeue();
                    Channel c = playing[s];
                    playing.Remove(s);
                    c.Stop();
                    unused.Push(c);
                }

                Thread.Sleep(SLEEP_DELAY);
            }
        }

        internal static void Play(Sound s)
        {
            playQueue.Enqueue(s);
        }

        internal static void Stop(Sound s)
        {
            stopQueue.Enqueue(s);
        }
    }
}
