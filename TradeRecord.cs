using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class TradeRecord
    {
        internal TradeAction type { get; private set; } // BOUGHT or SOLD
        internal int month { get; private set; }
        internal int day { get; private set; }
        internal int year { get; private set; }
        internal Money price { get; private set; }
        internal int shares { get; private set; }
        internal Money money { get; private set; }

        internal TradeRecord(TradeAction type,
            int month, int day, int year,
            Money price, int shares, Money money)
        {
            this.type = type;
            this.month = month;
            this.day = day;
            this.year = year;
            this.price = price;
            this.shares = shares;
            this.money = money;
        }
    }
}
