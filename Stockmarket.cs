using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Diagnostics;

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

    internal class Quote
    {
        internal int month, day, year;
        internal double open,
            close,
            high,
            low;
        internal long volume;
    }

    internal class TradeWeight
    {
        internal class Day
        {
            internal double open,
            close,
            high,
            low,
            volume;

            internal Day copy()
            {
                Day d = new Day();
                d.open = open;
                d.close = close;
                d.high = high;
                d.low = low;
                d.volume = volume;
                return d;
            }

            internal void Normalize()
            {
                close = normalizeWeight(close);
                high = normalizeWeight(high);
                low = normalizeWeight(low);
                open = normalizeWeight(open);
                volume = normalizeWeight(volume);
            }

            internal void Randomize()
            {
                close = Stockmarket.rand.NextDouble();
                high = Stockmarket.rand.NextDouble();
                low = Stockmarket.rand.NextDouble();
                open = Stockmarket.rand.NextDouble();
                volume = Stockmarket.rand.NextDouble();
            }
        }

        internal TradeWeight()
        {
            this.overall = 0.0;
            this.today = new Day();
            this.yesterday = new Day();
        }

        internal TradeWeight copy()
        {
            TradeWeight w = new TradeWeight();
            w.overall = overall;
            w.yesterday = yesterday.copy();
            w.today = today.copy();
            return w;
        }

        internal Day yesterday;
        internal Day today;
        internal double overall;

        internal void Mutate()
        {
            double increment = (((double)(Stockmarket.rand.Next() % 1000)) - 500.0) / 1000.0;

            // pick a double index randomly
            int idx = Stockmarket.rand.Next(11);
            switch (idx)
            {
                case 0:
                    overall += increment;
                    break;
                case 1:
                    yesterday.close += increment;
                    break;
                case 2:
                    yesterday.high += increment;
                    break;
                case 3:
                    yesterday.low += increment;
                    break;
                case 4:
                    yesterday.open += increment;
                    break;
                case 5:
                    yesterday.volume += increment;
                    break;
                case 6:
                    today.close += increment;
                    break;
                case 7:
                    today.high += increment;
                    break;
                case 8:
                    today.low += increment;
                    break;
                case 9:
                    today.open += increment;
                    break;
                case 10:
                    today.volume += increment;
                    break;
            }

            this.Normalize();
        }
        internal void Randomize()
        {
            overall = Stockmarket.rand.NextDouble();
            yesterday.Randomize();
            today.Randomize();
            this.Normalize();
        }
        public static TradeWeight RandomWeight
        {
            get
            {
                TradeWeight w = new TradeWeight();
                w.Randomize();
                return w;
            }
        }
        private void Normalize()
        {
            overall = normalizeWeight(overall);
            yesterday.Normalize();
            today.Normalize();
        }
        private static double normalizeWeight(double d)
        {
            if (d > 1.0)
                return 1.0;
            if (d < 0.0)
                return 0.0;
            return d;
        }

    }

    internal enum TradeAction
    {
        BUY,
        SELL,
        NONE
    }

    internal class TradeRecord
    {
        internal TradeAction type; // BOUGHT or SOLD
        internal int month, day, year;
        internal double price;
        internal int shares;
        internal double money;
    }

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
            return this.result.CompareTo(other.result);
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

        private static List<TradeRecord> copyTradeRecords(List<TradeRecord> list)
        {
            List<TradeRecord> listCopy = new List<TradeRecord>();
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
            return listCopy;
        }
    }

    internal static class Stockmarket
    {
        internal const double STARTING_MONEY = 100000.0;
        private const double COMMISSION = 8.0;
        private const int MAX_QUOTES = 2858;
        private const int MAX_TRADES = MAX_QUOTES;
        internal static readonly Random rand = new Random();


        private static List<Quote> buildQuotes(int count)
        {
            List<Quote> quotes = new List<Quote>();

            using (FileStream fs = new FileStream("data.csv", FileMode.Open))
            {
                StreamReader sr = new StreamReader(fs);


                //				m  d  y open high low close volume
                string line;
                while ((line = sr.ReadLine()) != null)
                {
                    line = line.Trim();
                    string[] parts = line.Split(',');
                    string[] dateParts = parts[0].Split('/');

                    Quote q = new Quote();
                    q.month = int.Parse(dateParts[0]);
                    q.day = int.Parse(dateParts[1]);
                    q.year = int.Parse(dateParts[2]);
                    q.open = double.Parse(parts[1]);
                    q.high = double.Parse(parts[2]);
                    q.low = double.Parse(parts[3]);
                    q.close = double.Parse(parts[4]);
                    q.volume = long.Parse(parts[5]);
                    quotes.Add(q);
                }
            }


            return quotes;
        }
        private static int buy(double price, int shares, Portfolio portfolio)
        {
            Debug.Assert(shares > 0);
            if (portfolio.money >= (price * shares) + COMMISSION)
            {
                portfolio.shares += shares;
                portfolio.money -= (price * shares) + COMMISSION;
                portfolio.trades += 1;
                return shares;
            }
            return 0;
        }
        private static int sell(double price, int shares, Portfolio portfolio)
        {
            Debug.Assert(shares > 0);
            if (portfolio.shares >= shares)
            {
                portfolio.shares -= shares;
                portfolio.money += (price * shares) - COMMISSION;
                portfolio.trades += 1;
                return shares;
            }
            return 0;
        }
        private static double dblRemainder(double a)
        {
            double retVal = Math.IEEERemainder(a, 1);
            Debug.Assert(retVal >= -1.0);
            Debug.Assert(retVal <= 1.0);
            return retVal;
        }
        private static bool maybe(Quote yesterday, Quote today, TradeWeight weight)
        {
            double x = ((yesterday.open * weight.yesterday.open)
                + (yesterday.close * weight.yesterday.close)
                + (yesterday.high * weight.yesterday.high)
                + (yesterday.low * weight.yesterday.low)
                + (yesterday.volume * weight.yesterday.volume)
                + (today.open * weight.today.open)
                + (today.close * weight.today.close)
                + (today.high * weight.today.high)
                + (today.low * weight.today.low)
                + (today.volume * weight.today.volume))
                * weight.overall;
            if (x >= 0.5)
                return true;
            return false;
        }
        private static int maybeBuy(Quote yesterday, Quote today, Strategy s)
        {
            if (s.Portfolio.money <= COMMISSION)
                return 0;

            int shares = (int)((s.Portfolio.money - COMMISSION) / today.close);
            if (shares <= 0)
                return 0;

            if (maybe(yesterday, today, s.BuyWeight))
                return buy(today.close, shares, s.Portfolio);

            return 0;
        }
        private static int maybeSell(Quote yesterday, Quote today, Strategy s)
        {
            if (s.Portfolio.shares < 1)
                return 0;

            // don't sell if we'd lose money
            if ((s.Trades)[s.Portfolio.trades - 1].price > today.close)
                return 0;

            if (maybe(yesterday, today, s.SellWeight))
                return sell(today.close, s.Portfolio.shares, s.Portfolio);

            return 0;
        }
        private static double score(Strategy s)
        {
            if (s.Portfolio.trades == 0)
                return double.MinValue; // the worst possible strategy is one that didn't trade at all
            return s.Result * ((Math.Log10(s.Portfolio.trades) / 10.0) + 1);
        }
        private static void runStrategy(Strategy s, List<Quote> q, int qFirst, int qLast)
        {
            // initialize portfolio
            s.Portfolio.money = STARTING_MONEY;
            s.Portfolio.shares = 0;
            s.Portfolio.trades = 0;

            // initialize trade history
            s.Trades.Clear();

            double lastPrice = 0.0;
            int i;
            for (i = qFirst; i < qLast; i++)
            {
                Quote yesterday = q[i - 1];
                Quote today = q[i];

                int shares = maybeBuy(yesterday, today, s);
                TradeAction type = TradeAction.NONE;
                if (shares > 0)
                    type = TradeAction.BUY;
                else
                {
                    shares = maybeSell(yesterday, today, s);
                    if (shares > 0)
                        type = TradeAction.SELL;
                }
                if (type != TradeAction.NONE)
                {
                    Debug.Assert(s.Portfolio.trades >= 1);
                    TradeRecord trade = new TradeRecord()
                    {
                        shares = shares,
                        type = type,
                        price = today.close,
                        month = today.month,
                        day = today.day,
                        year = today.year,
                        money = s.Portfolio.money
                    };
                    s.Trades.Add(trade);
                    Debug.Assert(s.Trades.Count == s.Portfolio.trades);
                }

                lastPrice = today.close;
            }
            s.Result = s.Portfolio.money + (s.Portfolio.shares * lastPrice);
        }
        private static void generation(List<Strategy> s, int sCount, List<Quote> q, int qCount)
        {
            int j;
            for (j = 0; j < sCount; j++)
            {
                runStrategy(s[j], q, 1, (qCount - (qCount / 5)));
            }
        }
        private static void copyBytes(TradeWeight twSource, TradeWeight twDest)
        {
            twDest.overall = twSource.overall;
            Action<TradeWeight.Day, TradeWeight.Day> copy = new Action<TradeWeight.Day, TradeWeight.Day>((src, dest) =>
            {
                dest.close = src.close;
                dest.high = src.high;
                dest.low = src.low;
                dest.open = src.open;
                dest.volume = src.volume;
            });
            copy(twSource.yesterday, twDest.yesterday);
            copy(twSource.today, twDest.today);
        }
        private static Strategy spawn(Strategy source)
        {
            // reset the Strategy members
            source.Result = 0.0;
            source.Portfolio.Clear();

            Strategy dest = new Strategy();

            // copy source to dest
            copyBytes(source.BuyWeight, dest.BuyWeight);
            copyBytes(source.SellWeight, dest.SellWeight);

            // mutate destination
            // pick either buy or sell weight randomly
            TradeWeight weight;
            if (rand.Next(2) == 0)
                weight = dest.BuyWeight;
            else
                weight = dest.SellWeight;

            weight.Mutate();

            return dest;
        }

        private static void mutate(List<Strategy> s, int sCount)
        {
            // For the ones that traded, drop the bottom half,
            // and spawn new ones out of the top half.
            // For the ones that didn't trade, drop them and randomize

            int i;
            int tCount = 0; // traded count

            for (i = 0; i < sCount && s[i].Portfolio.trades != 0; i++)
                tCount++;

            for (i = 0; i < tCount / 2; i++)
            {
                Strategy source = s[i];
                s[i + (tCount / 2)] = spawn(source);
            }

            // didn't trade -- randomize
            for (i = tCount; i < sCount; i++)
            {
                s[i].BuyWeight.Randomize();
                s[i].SellWeight.Randomize();
            }
        }
        private static double percentProfit(Strategy s)
        {
            double profit = s.Result - STARTING_MONEY;
            return (profit / STARTING_MONEY) * 100;
        }
        private static void debugPrintTradeHistory(Strategy s, double shareAmt, int tCount)
        {

        }
        private static void printTwoColumns(int line, int start, int offset, string text)
        {

        }
        private static void printResults(List<Strategy> s, int sCount, long gIdx, List<Quote> q, int qCount)
        {
            StringBuilder sb = new StringBuilder();

            double mean = 0.0;
            int meanTrades = 0;
            int i;
            for (i = 0; i < sCount; i++)
            {
                mean += s[i].Result;
                meanTrades += s[i].Portfolio.trades;
            }
            mean /= sCount;
            meanTrades /= sCount;

            sb.AppendFormat("Generation:    {0}\n", gIdx);
            sb.AppendFormat("Median:        {0}\n", s[sCount / 2].Result);
            sb.AppendFormat("Median Trades: {0}\n", s[sCount / 2].Portfolio.trades);
            sb.AppendFormat("Mean:          {0}\n", mean);
            sb.AppendFormat("Mean Trades:   {0}\n", meanTrades);
            sb.AppendFormat("Worst:         {0}\n", s[s.Count - 1].Result);
            sb.AppendFormat("Worst Trades:  {0}\n", s[s.Count - 1].Portfolio.trades);
            sb.AppendFormat("Best:          {0}\n", s[0].Result);
            sb.AppendFormat("Best Trades:   {0}\n", s[0].Portfolio.trades);

            MainWindow.resultText = sb.ToString();
        }
        private static double proofStrategy(Strategy s, List<Quote> q, int qCount)
        {
            // Set up a copy with the same weights
            // but a new portfolio and history
            Strategy copy = s.copy();

            // Run the strategy and get the percent profit
            runStrategy(copy, q, (qCount - (qCount / 5)), qCount);
            double profit = percentProfit(copy);

            return profit;
        }
        internal static void main()
        {
            long gCount = long.MaxValue; // generations
            int qCount = MAX_QUOTES, // quotes
                sCount = 100; // strategies

            // initialize quotes
            List<Quote> quotes = buildQuotes(qCount);

            // initialize trade weights / strategies
            List<Strategy> strategies = new List<Strategy>();
            long i;
            for (i = 0; i < sCount; i++)
            {
                Strategy s = new Strategy();
                strategies.Add(s);
            }

            for (i = 0; i < gCount; i++)
            {
                generation(strategies, sCount, quotes, qCount);
                strategies.Sort();

                printResults(strategies, sCount, i, quotes, qCount);

                if (i != gCount - 1)
                    mutate(strategies, sCount);
            }
        }
    }
}
