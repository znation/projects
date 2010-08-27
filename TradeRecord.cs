using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class TradeRecord
    {
        internal TradeAction type; // BOUGHT or SOLD
        internal int month, day, year;
        internal double price;
        internal int shares;
        internal double money;
    }
}
