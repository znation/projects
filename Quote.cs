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
        internal decimal open { get; private set; }
        internal decimal close { get; private set; }
        internal decimal high { get; private set; }
        internal decimal low { get; private set; }
        internal long volume { get; private set; }
        internal Quote(int m, int d, int y,
            decimal o, decimal c, decimal h, decimal l,
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
