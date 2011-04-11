using System;
using SdlDotNet.Audio;
using System.Collections.Generic;
using System.Threading;

namespace Synth
{
    internal class Sequencer
    {
        private const int SMALLEST_NOTE = 32; // 32nd note
        private static readonly int NOTE_DURATIONS = Convert.ToInt32(Math.Log(SMALLEST_NOTE, 2));

        // Hardcode time signature for now
        private const int BEATS_PER_MEASURE = 4;
        private const int SIZE_OF_BEAT = 4;

        private Thread sequenceThread = null;
        private readonly int bpm;
        private readonly int measures;

        // TODO -- write own priorityqueue
        private readonly PriorityQueue<NoteDuration> sequence;
        private readonly PriorityQueue<NoteDuration> playing;
        internal Sequencer(int measures, int bpm)
        {
            this.measures = measures;
            this.bpm = bpm;
            this.sequence = new PriorityQueue<NoteDuration>();
            this.playing = new PriorityQueue<NoteDuration>();
        }

        internal void Add(string note,
            int octave,
            int duration, // length of note (1 for whole, 2 for half, etc)
            int measure, // index of measure in sequence
            int position) // within measure, relative to duration
        {
            if (sequenceThread != null)
                throw new Exception("Cannot add notes while playing");

            NoteDuration nd = new NoteDuration(note,
                octave,
                duration,
                measure,
                position,
                bpm,
                BEATS_PER_MEASURE,
                SIZE_OF_BEAT);
            sequence.Push(nd.Start, nd);
        }

        private void SequenceLoop()
        {
            DateTime startTime = DateTime.Now;
            double millisecondsPerBeat = (1000.0 * 60.0) / (double)bpm;
            while (true)
            {
                DateTime currentTime = DateTime.Now;
                double elapsed = currentTime.Subtract(startTime).TotalMilliseconds;

                NoteDuration nd = sequence.Peek();
                if (nd != null && nd.Start < elapsed)
                {
                    Mixer.Play(nd.Sound);
                    sequence.Pop();
                    playing.Push(nd.End, nd);
                }

                nd = playing.Peek();
                if (nd != null && nd.End < elapsed)
                {
                    Mixer.Stop(nd.Sound);
                    playing.Pop();
                }

                Thread.Sleep(Synth.SLEEP_DELAY);
            }
        }

        internal Thread Play()
        {
            sequenceThread = new Thread(SequenceLoop);
            sequenceThread.Start();
            return sequenceThread;
        }

        internal void Stop()
        {
            sequenceThread.Abort();
            sequenceThread = null;
        }
    }
}
