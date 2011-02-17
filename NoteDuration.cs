using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SdlDotNet.Audio;

namespace Synth
{
    internal class NoteDuration
    {
        internal double Start // in milliseconds
        {
            get;
            private set;
        }

        internal double End // in milliseconds
        {
            get;
            private set;
        }

        internal Sound Sound
        {
            get;
            private set;
        }

        internal NoteDuration(string note,
            int octave,
            int duration, // length of note (1 for whole, 2 for half, etc)
            int measure, // index of measure in sequence
            int position, // within measure, relative to duration
            int beatsPerMinute,
            int beatsPerMeasure, // last 2 are time signature info
            int beatSize)
        {
            double millisecondsPerBeat = (1000.0 * 60.0) / (double)beatsPerMinute;
            double millisecondsPerMeasure = millisecondsPerBeat * beatsPerMeasure;
            double measureMs = (double)measure * (double)millisecondsPerBeat * (double)beatsPerMeasure;
            double durationMs = (millisecondsPerMeasure / duration) * (beatSize / beatsPerMeasure);
            double positionMs = (double)position * durationMs;

            this.Start = measureMs + positionMs;
            this.End = this.Start + durationMs - 20.0;
            this.Sound = Note.GetNoteSound(note, octave);
        }
    }
}
