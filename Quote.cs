using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class Quote
    {
        internal int month { get; private set; }
        internal int day { get; private set; }
        internal int year { get; private set; }
        internal double open { get; private set; }
        internal double close { get; private set; }
        internal double high { get; private set; }
        internal double low { get; private set; }
        internal long volume { get; private set; }
        internal Quote(int m, int d, int y,
            double o, double c, double h, double l,
            long v)
        {
            month = m;
            day = d;
            year = y;
            open = o;
            close = c;
            high = h;
            low = l;
            volume = v;
        }
    }
}
