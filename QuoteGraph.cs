using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class QuoteGraph
    {
        internal QuoteGraph(List<Quote> quotes)
        {
            // per quote, 100 pixels vertically, 1 horizontally, 4 bytes per pixel
            Bytes = new byte[quotes.Count * 100 * 4];

            decimal min = decimal.MaxValue, max = decimal.MinValue;
            foreach (Quote q in quotes)
            {
                min = Math.Min(min, q.close);
                max = Math.Max(max, q.close);
            }
            int i = 0;
            foreach (Quote q in quotes)
            {
                int weighted = (int)(100*4*Math.Round((q.close - min) / (max - min)));
                Bytes[weighted + i] = 255;
                Bytes[weighted + i + 1] = 255;
                Bytes[weighted + i + 2] = 255;
                Bytes[weighted + i + 3] = 255;
                i++;
            }
        }

        internal byte[] Bytes { get; set; }
    }
}
