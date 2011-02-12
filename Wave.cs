using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using SdlDotNet.Audio;

namespace Synth
{
    // http://msdn.microsoft.com/en-us/library/aa446573.aspx

    internal class WaveFormatEx
    {
        private const ushort wFormatTag = 1; // pcm
        private const ushort nChannels = Synth.CHANNELS; // stereo
        private const uint nSamplesPerSec = Synth.SAMPLES_PER_SECOND; // hz
        private static readonly uint nAvgBytesPerSec = (uint)Math.Ceiling((double)Synth.BITS_PER_SAMPLE / (double)8) * Synth.SAMPLES_PER_SECOND * Synth.CHANNELS; // 16 bits (2 bytes) per sample * 44100 samples per second
        private const ushort nBlockAlign = Synth.CHANNELS * (Synth.BITS_PER_SAMPLE / 8);
        private const ushort wBitsPerSample = Synth.BITS_PER_SAMPLE;

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
        private readonly uint samples;
        private Wave(uint ms)
        {
            samples = (ms * Synth.SAMPLES_PER_SECOND) / 1000;
            data = new byte[dataSize];
        }

        private static readonly byte[] RIFF = Bytes.ToByteArray("RIFF".ToCharArray());
        private uint sizeInt
        {
            get
            {
                Debug.Assert(WAVE.Length == 4);
                Debug.Assert(fmt.Length == 4);
                Debug.Assert(WaveFormatExSize.Length == 4);
                Debug.Assert(WaveFormatEx.Length == 16);
                Debug.Assert(dataChars.Length == 4);
                Debug.Assert(Bytes.ToByteArray(dataSize).Length == 4);

                return (uint)(4 // "WAVE"
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
        private uint dataSize
        {

            get
            {
                return samples * Synth.CHANNELS * Synth.BITS_PER_SAMPLE / 8;
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
            list.AddRange(Bytes.ToByteArray(dataSize));
            list.AddRange(data);

            byte[] bytes = list.ToArray();
            Debug.Assert(bytes.Length == sizeInt + 8);
            return bytes;
        }

        internal static byte[] Random
        {
            get
            {
                Wave w = new Wave((uint)Synth.RANDOM.Next(10) * 1000);
                Synth.RANDOM.NextBytes(w.data);
                return w.ToByteArray();
            }
        }

        internal static Sound FromHz(uint ms, params Frequency[] frequencies)
        {
            Wave w = new Wave(ms);
            int dataIdx = 0;

            for (uint i = 0; i < w.samples; i++)
            {
                double[] values = new double[frequencies.Length];
                for (int j = 0; j < frequencies.Length; j++)
                {
                    if (frequencies[j].volume <= 0.0 || frequencies[j].volume > 1.0)
                        throw new Exception("Volume must be > 0.0 and <= 1.0");

                    double weight = ((double)Int16.MaxValue) * frequencies[j].volume;

                    double insideSineStuff = ((double)frequencies[j].hz * (double)i * 1000) / ((double)Synth.SAMPLES_PER_SECOND * (double)Synth.CHANNELS);

                    double sine = Math.Sin(Math.PI * 2 * insideSineStuff);

                    Debug.Assert(sine >= -1.0 && sine <= 1.0);

                    values[j] = (sine * weight);
                }

                double value = values.Average();

                Debug.Assert(value >= short.MinValue && value <= short.MaxValue);

                byte[] sample = Bytes.ToByteArray(Convert.ToInt16(value));
                Debug.Assert(sample.Length == 2);
                for (int j = 0; j < sample.Length; j++)
                {
                    w.data[dataIdx++] = sample[j];
                }
            }

            byte[] bytes = w.ToByteArray();
            return new Sound(bytes);
        }
    }
}
