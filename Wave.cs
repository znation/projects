using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    // http://msdn.microsoft.com/en-us/library/aa446573.aspx

    internal class WaveFormatEx
    {
        private const UInt16 wFormatTag = 1; // pcm
        private const UInt16 nChannels = 2; // stereo
        private const UInt32 nSamplesPerSec = 44100; // hz
        private const UInt32 nAvgBytesPerSec = 2 * 44100; // 16 bits (2 bytes) per sample * 44100 samples per second
        private const UInt16 nBlockAlign = 0; // ???
        private const UInt16 wBitsPerSample = 16;

        internal byte[] ToByteArray()
        {
            List<byte> bytes = new List<byte>();
            bytes.AddRange(Bytes.ToByteArray(wFormatTag));
            bytes.AddRange(Bytes.ToByteArray(nChannels));
            bytes.AddRange(Bytes.ToByteArray(nSamplesPerSec));
            bytes.AddRange(Bytes.ToByteArray(nAvgBytesPerSec));
            bytes.AddRange(Bytes.ToByteArray(nBlockAlign));
            bytes.AddRange(Bytes.ToByteArray(wBitsPerSample));
            return bytes.ToArray();
        }
    }

    internal class Wave
    {
        private static readonly byte[] RIFF = Bytes.ToByteArray("RIFF".ToCharArray());
        private byte[] size
        {
            get
            {
                UInt32 s = (UInt32) (4 // 16
                    + 16 // the WaveFormatEx
                    + 4 // "data"
                    + 4 // dataSize
                    + data.Length); // whole file minus "WAVEfmt "
                return Bytes.ToByteArray(s);
            }
        }
        private static readonly byte[] WAVE = Bytes.ToByteArray("WAVE".ToCharArray());
        private static readonly byte[] fmt = Bytes.ToByteArray("fmt ".ToCharArray());
        private static readonly byte[] WaveFormatExSize = Bytes.ToByteArray(16);
        private static readonly byte[] WaveFormatEx = (new WaveFormatEx()).ToByteArray();
        private static readonly byte[] dataChars = Bytes.ToByteArray("data".ToCharArray());
        private byte[] dataSize
        {

            get
            {
                return Bytes.ToByteArray((UInt32)data.Length);
            }
        }

        private byte[] data;

        internal byte[] ToByteArray()
        {
            List<byte> list = new List<byte>();
            list.AddRange(RIFF);
            list.AddRange(size);
            list.AddRange(WAVE);
            list.AddRange(fmt);
            list.AddRange(WaveFormatExSize);
            list.AddRange(WaveFormatEx);
            list.AddRange(dataChars);
            list.AddRange(dataSize);
            list.AddRange(data);
            return list.ToArray();
        }

        internal static byte[] Random
        {
            get
            {
                Wave w = new Wave();
                w.data = new byte[1024 * 1024];
                Synth.Random.NextBytes(w.data);
                return w.ToByteArray();
            }
        }
    }
}
