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
            Values = new int[quotes.Count];

            decimal min = decimal.MaxValue, max = decimal.MinValue;
            foreach (Quote q in quotes)
            {
                min = Math.Min(min, q.close);
                max = Math.Max(max, q.close);
            }
            int i = 0;
            foreach (Quote q in quotes)
            {
                int weighted = (int)(100.0m * ((q.close - min) / (max - min)));
                Values[i] = weighted;
                i++;
            }
        }

        internal int[] Values { get; private set; }
    }
}
