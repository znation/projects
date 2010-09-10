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
        internal Money close { get; private set; }
        internal Quote(int m, int d, int y, Money c)
        {
            month = m;
            day = d;
            year = y;
            close = c;
        }
    }
}
