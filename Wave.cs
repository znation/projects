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
                // TODO -- return the real size
                return Bytes.ToByteArray(1);
            }
        }
        private static readonly byte[] WAVE = Bytes.ToByteArray("WAVE".ToCharArray());
        private static readonly byte[] fmt = Bytes.ToByteArray("fmt ".ToCharArray());
        private static readonly byte[] WaveFormatExSize = Bytes.ToByteArray(16);
        private static readonly byte[] WaveFormatEx = (new WaveFormatEx()).ToByteArray();
        private static readonly byte[] data = Bytes.ToByteArray("data".ToCharArray());
        private byte[] dataSize
        {

            get
            {
                return Bytes.ToByteArray(1); // TODO -- return the real size
            }
        }

        private byte[] Data
        {
            get;
            set;
        }

        internal byte[] ToByteArray()
        {
            List<byte> list = new List<byte>();
            list.AddRange(RIFF);
            return list.ToArray();
        }

        internal static byte[] Random
        {
            get
            {
                Wave w = new Wave();
                w.Data = Bytes.GetRandomBytes(1024 * 1024);
                return w.ToByteArray();
            }
        }
    }
}
