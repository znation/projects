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
        internal Money open { get; private set; }
        internal Money close { get; private set; }
        internal Money high { get; private set; }
        internal Money low { get; private set; }
        internal long volume { get; private set; }
        internal Quote(int m, int d, int y,
            Money o, Money c, Money h, Money l,
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
