using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
    internal class Strategy : IComparable
    {
        internal Strategy()
        {
            BuyWeight = TradeWeight.RandomWeight;
            SellWeight = TradeWeight.RandomWeight;
            Result = 0.0;
            Trades = new List<TradeRecord>();
            Portfolio = new Portfolio();
        }

        // TODO: refactor to be more purely functional, don't modify Result (make it private)
        internal Portfolio Portfolio { get; set; }
        internal double Result { get; set; }

        internal TradeWeight BuyWeight { get; private set; }
        internal TradeWeight SellWeight { get; private set; }
        internal List<TradeRecord> Trades { get; private set; }

        int IComparable.CompareTo(object obj)
        {
            Strategy other = obj as Strategy;
            return other.Result.CompareTo(this.Result);
        }

        internal Strategy copy()
        {
            Strategy s = new Strategy();
            s.BuyWeight = BuyWeight.copy();
            s.SellWeight = SellWeight.copy();
            s.Portfolio = Portfolio.copy();
            s.Result = Result;
            s.Trades = copyTradeRecords(Trades);
            return s;
        }

        private static List<TradeRecord> copyTradeRecords(List<TradeRecord> list)
        {
            List<TradeRecord> listCopy = new List<TradeRecord>();
            foreach (TradeRecord original in list)
            {
                TradeRecord copy = new TradeRecord(original.type,
                    original.month,
                    original.day,
                    original.year,
                    original.price,
                    original.shares,
                    original.money);
                listCopy.Add(copy);
            }
            return listCopy;
        }

        internal static List<Strategy> copy(List<Strategy> strategies)
        {
            List<Strategy> copies = new List<Strategy>();
            foreach (Strategy s in strategies)
            {
                copies.Add(s.copy());
            }
            return copies;
        }

        internal static void Sort(List<Strategy> strategies)
        {
            for (int i = 0; i < strategies.Count - 1; i++)
            {
                for (int j = 0; j < strategies.Count - 1; j++)
                {
                    if (strategies[j].Result < strategies[j + 1].Result)
                    {
                        Strategy temp = strategies[j];
                        strategies[j] = strategies[j + 1];
                        strategies[j + 1] = temp;
                    }
                }
            }
        }

        internal static bool Sorted(List<Strategy> strategies)
        {
            for (int i = 0; i < strategies.Count - 1; i++)
            {
                if (strategies[i + 1].Result > strategies[i].Result)
                    return false;
            }
            return true;
        }
    }
}
