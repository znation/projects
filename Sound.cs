using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    class Sound : SdlDotNet.Audio.Sound
    {
        private Sound(byte[] bytes) : base(bytes) { }

        internal static Sound FromHz(uint ms, params Frequency[] frequencies)
        {
            return new Sound(Bytes.FromHz(ms, frequencies));
        }
    }
}
