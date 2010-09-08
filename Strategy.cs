using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class Strategy : IComparable
    {
        private TradeWeight buyWeight;
        private TradeWeight sellWeight;
        private double result;
        private List<TradeRecord> trades;
        private Portfolio portfolio;

        internal Strategy()
        {
            buyWeight = TradeWeight.RandomWeight;
            sellWeight = TradeWeight.RandomWeight;
            result = 0.0;
            trades = new List<TradeRecord>();
            portfolio = new Portfolio();
        }

        internal double Result
        {
            get { return result; }
            set { result = value; }
        }

        internal TradeWeight BuyWeight
        {
            get { return buyWeight; }
        }
        internal TradeWeight SellWeight
        {
            get { return sellWeight; }
        }
        internal Portfolio Portfolio
        {
            get { return portfolio; }
        }
        internal List<TradeRecord> Trades
        {
            get { return trades; }
        }

        int IComparable.CompareTo(object obj)
        {
            Strategy other = obj as Strategy;
            return other.result.CompareTo(this.result);
        }

        internal Strategy copy()
        {
            Strategy s = new Strategy();
            s.buyWeight = buyWeight.copy();
            s.sellWeight = sellWeight.copy();
            s.portfolio = portfolio.copy();
            s.result = result;
            s.trades = copyTradeRecords(trades);
            return s;
        }

        private static Object copyLock = new Object();
        private static List<TradeRecord> copyTradeRecords(List<TradeRecord> list)
        {
            List<TradeRecord> listCopy = new List<TradeRecord>();
            lock (copyLock)
            {
                foreach (TradeRecord original in list)
                {
                    TradeRecord copy = new TradeRecord()
                    {
                        day = original.day,
                        money = original.money,
                        month = original.month,
                        price = original.price,
                        shares = original.shares,
                        type = original.type,
                        year = original.year
                    };
                    listCopy.Add(copy);
                }
            }
            return listCopy;
        }
    }
}
