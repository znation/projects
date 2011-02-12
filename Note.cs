using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    class Note : Frequency
    {
        private const double A1 = 55.0; // starting note
        private static readonly Dictionary<string, double> notePosition = new Dictionary<string, double>()
        {
            {"A", 1},
            {"A#", 2},
            {"Bf", 2},
            {"B", 3},
            {"Cf", 3},
            {"B#", 4},
            {"C", 4},
            {"C#", 5},
            {"Df", 5},
            {"D", 6},
            {"D#", 7},
            {"Ef", 7},
            {"E", 8},
            {"E#", 9},
            {"F", 9},
            {"F#", 10},
            {"Gf", 10},
            {"G", 11},
            {"G#", 12},
            {"Af", 12}
        };

        internal Note(String note, ushort octave) : this(note, octave, 1.0) { }
        internal Note(String note, ushort octave, double volume)
        {
            note = note.ToUpperInvariant();

            // calculate distance from starting note
            double distance = (notePosition[note] - notePosition["A"]) + (12.0 * ((double)octave - 1.0));

            // frequency = starting note * 2^(distance/12)
            this.hz = A1 * Math.Pow(2, (distance / 12.0));

            this.volume = volume;
        }
    }
}
