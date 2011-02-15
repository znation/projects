using System;
using System.Collections.Generic;
using SdlDotNet.Audio;

namespace Synth
{
    internal static class Note
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
            {"Ff", 8},
            {"E#", 9},
            {"F", 9},
            {"F#", 10},
            {"Gf", 10},
            {"G", 11},
            {"G#", 12},
            {"Af", 12}
        };

        private static Dictionary<double, Sound> renderedNotes = new Dictionary<double, Sound>();

        internal static Sound GetNoteSound(String note, int octave)
        {
            return renderedNotes[GetFrequency(note, octave)];
        }

        internal static void RenderNotes()
        {
            char c;
            ushort i;
            for (c = 'A', i = 1; i <= 8; c++)
            {
                if (c == 'H')
                {
                    c = 'A';
                    i++;
                }

                String[] noteNames = new String[]
                {
                    c.ToString(),
                    c.ToString() + "#",
                    c.ToString() + "f"
                };

                foreach(String noteName in noteNames)
                {
                    double frequency = GetFrequency(noteName, i);
                    double samples = (1.0 / frequency) * (double)Synth.SAMPLES_PER_SECOND;
                    renderedNotes[frequency] = new Sound(Bytes.FromHz(Convert.ToInt32(samples), // TODO: calculate the right number of hz for a loop at this frequency
                        new Frequency(frequency)));
                }
                
            }
        }

        private static double GetFrequency(String note, int octave)
        {
            // calculate distance from starting note
            double distance = (notePosition[note] - notePosition["A"]) + (12.0 * ((double)octave - 1.0));

            // frequency = starting note * 2^(distance/12)
            return A1 * Math.Pow(2, (distance / 12.0));
        }
    }
}
