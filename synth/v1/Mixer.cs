using System.Collections;
using System.Collections.Generic;
using System.Threading;
using SdlDotNet.Audio;
using System;

namespace Synth
{
    internal struct SoundContainer
    {
        internal Sound sound;
    }

    internal static class Mixer
    {
        private const int NUM_CHANNELS = 1024;
        private static Stack<Channel> unused = new Stack<Channel>();
        private static Dictionary<SoundContainer, Channel> playing = new Dictionary<SoundContainer, Channel>();
        private static Queue playQueue = Queue.Synchronized(new Queue()); // Queue<SoundContainer>
        private static Queue stopQueue = Queue.Synchronized(new Queue()); // Queue<SoundContainer>
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
                    SoundContainer s = (SoundContainer)playQueue.Dequeue();
                    Channel c = unused.Pop();
                    playing[s] = c;
                    c.Play(s.sound, true);
                }
                while (stopQueue.Count > 0)
                {
                    SoundContainer s = (SoundContainer)stopQueue.Dequeue();
                    Channel c = playing[s];
                    playing.Remove(s);
                    c.Stop();
                    unused.Push(c);
                }

                Thread.Sleep(Synth.SLEEP_DELAY);
            }
        }

        internal static void Play(Sound s)
        {
            SoundContainer c = new SoundContainer();
            c.sound = s;
            playQueue.Enqueue(c);
        }

        internal static void Stop(Sound s)
        {
            SoundContainer c = new SoundContainer();
            c.sound = s;
            stopQueue.Enqueue(c);
        }

        // Plays until stopped
        internal static void Play(String note, int octave)
        {
            Sound s = Note.GetNoteSound(note, octave);
            Play(s);
        }

        internal static void Stop(String note, int octave)
        {
            Sound s = Note.GetNoteSound(note, octave);
            Stop(s);
        }
    }
}
