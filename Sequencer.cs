using System;
using SdlDotNet.Audio;

namespace Synth
{
    internal class Sequencer
    {
        // Plays until stopped
        internal static void Play(String note, int octave)
        {
            Sound s = Note.GetNoteSound(note, octave);
            Mixer.Play(s);
        }

        internal static void Stop(String note, int octave)
        {
            Sound s = Note.GetNoteSound(note, octave);
            Mixer.Stop(s);
        }
    }

    
}
