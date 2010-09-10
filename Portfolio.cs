using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class Portfolio
    {
        internal int shares { get; private set; }
        internal decimal money { get; private set; }
        internal int trades { get; private set; }

        internal Portfolio()
        {
            shares = 0;
            money = Stockmarket.STARTING_MONEY;
            trades = 0;
        }

        internal Portfolio(int shares, decimal money, int trades)
        {
            this.shares = shares;
            this.money = money;
            this.trades = trades;
        }

        internal Portfolio copy()
        {
            return new Portfolio(shares, money, trades);
        }

        internal void Clear()
        {
            shares = 0;
            money = Stockmarket.STARTING_MONEY;
            trades = 0;
        }
    }
}