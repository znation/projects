using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class Quote
    {
        internal int month, day, year;
        internal double open,
            close,
            high,
            low;
        internal long volume;
    }
}
