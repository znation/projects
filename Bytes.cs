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

        internal static byte[] ToByteArray(UInt32 integer, UInt32 byteCount)
        {
            byte[] bytes = new byte[byteCount];
            for (int i = 0; i < byteCount; i++)
            {
                bytes[(byteCount-1)-i] = (byte)((integer & (0xff << (i * 8))) >> (i * 8));
            }
            return bytes;
        }

        internal static byte[] ToByteArray(UInt32 integer)
        {
            return ToByteArray(integer, 4);
        }

        internal static byte[] ToByteArray(UInt16 integer)
        {
            return ToByteArray(integer, 2);
        }
    }
}
