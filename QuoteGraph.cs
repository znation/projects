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
                decimal close = q.close.ToDecimal();
                min = Math.Min(min, close);
                max = Math.Max(max, close);
            }
            int i = 0;
            foreach (Quote q in quotes)
            {
                decimal close = q.close.ToDecimal();
                int weighted = (int)(100.0m * ((close - min) / (max - min)));
                Values[i] = weighted;
                i++;
            }
        }

        internal int[] Values { get; private set; }
    }
}
