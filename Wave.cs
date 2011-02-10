using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace Synth
{
    // http://msdn.microsoft.com/en-us/library/aa446573.aspx

    internal class WaveFormatEx
    {
        private const UInt16 wFormatTag = 1; // pcm
        private const UInt16 nChannels = 2; // stereo
        private const UInt32 nSamplesPerSec = Synth.SAMPLES_PER_SECOND; // hz
        private const UInt32 nAvgBytesPerSec = (Synth.BITS_PER_SAMPLE / 8) * Synth.SAMPLES_PER_SECOND; // 16 bits (2 bytes) per sample * 44100 samples per second
        private const UInt16 nBlockAlign = 0; // ???
        private const UInt16 wBitsPerSample = Synth.BITS_PER_SAMPLE;

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
        private UInt32 sizeInt
        {
            get
            {
                Debug.Assert(WAVE.Length == 4);
                Debug.Assert(fmt.Length == 4);
                Debug.Assert(WaveFormatExSize.Length == 4);
                Debug.Assert(WaveFormatEx.Length == 16);
                Debug.Assert(dataChars.Length == 4);
                Debug.Assert(dataSize.Length == 4);

                return (UInt32)(4 // "WAVE"
                    + 4 // "fmt "
                    + 4 // WaveFormatExSize
                    + 16 // the WaveFormatEx
                    + 4 // "data"
                    + 4 // dataSize
                    + data.Length); // whole file minus "RIFF" and this size
            }
        }
        private byte[] size
        {
            get
            {
                return Bytes.ToByteArray(sizeInt);
            }
        }
        private static readonly byte[] WAVE = Bytes.ToByteArray("WAVE".ToCharArray());
        private static readonly byte[] fmt = Bytes.ToByteArray("fmt ".ToCharArray());
        private static readonly byte[] WaveFormatExSize = Bytes.ToByteArray((UInt32)16);
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

            byte[] bytes = list.ToArray();
            Debug.Assert(bytes.Length == sizeInt + 8);
            return bytes;
        }

        internal static byte[] Random
        {
            get
            {
                Wave w = new Wave();
                w.data = new byte[1024 * 1024];
                Synth.RANDOM.NextBytes(w.data);
                return w.ToByteArray();
            }
        }

        internal static byte[] FromHz(UInt32 hz)
        {
            Wave w = new Wave();

            UInt32 seconds = 10; // seconds
            UInt32 samples = seconds * Synth.SAMPLES_PER_SECOND;
            UInt32 bits = samples * Synth.BITS_PER_SAMPLE;
            UInt32 bytes = bits / 8;
            w.data = new byte[bytes];
            int dataIdx = 0;

            for (UInt32 i = 0; i < samples; i++)
            {
                double value = Math.Sin((Math.PI * (((double)i * Synth.SAMPLES_PER_SECOND) / (double)seconds)) / (double)hz);

                // normalize to UInt16
                value *= (UInt16.MaxValue / 2);
                value += (UInt16.MaxValue / 2);


                UInt16 sample = Convert.ToUInt16(value);
                byte[] sampleBytes = Bytes.ToByteArray(sample);
                for (int j = 0; j < sampleBytes.Length; j++)
                {
                    w.data[dataIdx + j] = sampleBytes[j];
                }
                dataIdx += sampleBytes.Length;
            }

            return w.ToByteArray();
        }
    }
}
