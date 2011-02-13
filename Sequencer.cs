using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;
using System.Threading;

namespace Synth
{
    internal class Sequencer
    {
        private const int SLEEP_LENGTH = 20; //ms
        private static Queue playQueue = Queue.Synchronized(new Queue()); // Queue<Tuple<Sound, int, DateTime>>

        internal static void Enqueue(String note, int octave, int playTime, DateTime startTime)
        {
            Sound s = Note.GetNoteSound(note, octave);
            playQueue.Enqueue(new Tuple<Sound, int, DateTime>(s, playTime, startTime));
        }

        internal static void PlayLoop()
        {
            while (true)
            {
                List<Tuple<Sound, int, DateTime>> list = new List<Tuple<Sound, int, DateTime>>();
                while (playQueue.Count > 0)
                {
                    list.Add((Tuple<Sound, int, DateTime>)playQueue.Dequeue());
                }
                foreach (Tuple<Sound, int, DateTime> tuple in list)
                {
                    if (tuple.Item3 <= DateTime.Now)
                    {
                        // Loop indefinitely, play for the desired number of milliseconds
                        tuple.Item1.Play(-1, tuple.Item2);
                    }
                    else
                    {
                        playQueue.Enqueue(tuple);
                    }
                }

                Thread.Sleep(SLEEP_LENGTH);
            }
        }
    }

    
}
