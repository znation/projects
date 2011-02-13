using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    internal static class Bytes
    {
        internal static byte[] ToByteArray(char[] charArray)
        {
            byte[] bytes = new byte[charArray.Length];
            for (int i = 0; i < charArray.Length; i++)
            {
                bytes[i] = (byte)charArray[i];
            }
            return bytes;
        }

        internal static byte[] ToByteArray(int integer)
        {
            byte[] bytes = new byte[4];
            for (int i = 0; i < 4; i++)
            {
                bytes[i] = (byte)((integer & (0xff << (i * 8))) >> (i * 8));
            }
            return bytes;
        }

        internal static byte[] ToByteArray(uint integer)
        {
            byte[] bytes = new byte[4];
            for (int i = 0; i < 4; i++)
            {
                bytes[i] = (byte)((integer & (0xff << (i * 8))) >> (i * 8));
            }
            return bytes;
        }

        internal static byte[] ToByteArray(short integer)
        {
            byte[] bytes = new byte[2];
            for (int i = 0; i < 2; i++)
            {
                bytes[i] = (byte)((integer & (0xff << (i * 8))) >> (i * 8));
            }
            return bytes;
        }

        internal static byte[] ToByteArray(ushort integer)
        {
            byte[] bytes = new byte[2];
            for (int i = 0; i < 2; i++)
            {
                bytes[i] = (byte)((integer & (0xff << (i * 8))) >> (i * 8));
            }
            return bytes;
        }

        internal static byte[] FromHz(int samples, params Frequency[] frequencies)
        {
            return Wave.FromHz(samples, frequencies).ToByteArray();
        }
    }
}
