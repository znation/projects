using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class Portfolio
    {
        internal int shares;
        internal double money;
        internal int trades;

        internal Portfolio()
        {
            shares = 0;
            money = Stockmarket.STARTING_MONEY;
            trades = 0;
        }

        internal Portfolio copy()
        {
            Portfolio p = new Portfolio();
            p.shares = shares;
            p.money = money;
            p.trades = trades;
            return p;
        }

        internal void Clear()
        {
            shares = 0;
            money = Stockmarket.STARTING_MONEY;
            trades = 0;
        }
    }
}