using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Diagnostics;

namespace stockmarket
{
    internal enum TradeAction
    {
        BUY,
        SELL,
        NONE
    }
    internal static class Stockmarket
    {
        internal const double STARTING_MONEY = 100000.0;
        private const double COMMISSION = 8.0;
        internal static readonly Random rand = new Random();

        private static List<Quote> sineWaveQuotes()
        {
            List<Quote> quotes = new List<Quote>();
            for (int i = 0; i < 365 * 10; i++)
            {

            }
            return quotes;
        }

        private static List<Quote> buildQuotes()
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
        private static void generation(List<Strategy> s, int sCount, List<Quote> q)
        {
            int qCount = q.Count;
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

       

        private static void printResults(List<Strategy> s, int sCount, long gIdx, List<Quote> q)
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
            sb.AppendFormat("Median:        {0}\n", s[sCount / 2].Result.ToString("F2"));
            sb.AppendFormat("Median Trades: {0}\n", s[sCount / 2].Portfolio.trades);
            sb.AppendFormat("Mean:          {0}\n", mean.ToString("F2"));
            sb.AppendFormat("Mean Trades:   {0}\n", meanTrades);
            sb.AppendFormat("Worst:         {0}\n", s[s.Count - 1].Result.ToString("F2"));
            sb.AppendFormat("Worst Trades:  {0}\n", s[s.Count - 1].Portfolio.trades);
            sb.AppendFormat("Best:          {0}\n", s[0].Result.ToString("F2"));
            sb.AppendFormat("Best Trades:   {0}\n", s[0].Portfolio.trades);
            sb.AppendFormat("Profitability: {0}\n", proofStrategy(s[0], q).ToString("F2"));

            MainWindow.resultText = sb.ToString();

            printTradeWeight(s[0]);
        }

        private static void printTradeWeight(Strategy s)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendFormat("           BuyWeight   SellWeight\n");
            sb.AppendFormat("   overall {0} {1}\n", s.BuyWeight.overall.ToString("F9"), s.SellWeight.overall.ToString("F9"));
            sb.AppendFormat("y: open    {0} {1}\n", s.BuyWeight.yesterday.open.ToString("F9"), s.SellWeight.yesterday.open.ToString("F9"));
            sb.AppendFormat("   close   {0} {1}\n", s.BuyWeight.yesterday.close.ToString("F9"), s.SellWeight.yesterday.close.ToString("F9"));
            sb.AppendFormat("   high    {0} {1}\n", s.BuyWeight.yesterday.high.ToString("F9"), s.SellWeight.yesterday.high.ToString("F9"));
            sb.AppendFormat("   low     {0} {1}\n", s.BuyWeight.yesterday.low.ToString("F9"), s.SellWeight.yesterday.low.ToString("F9"));
            sb.AppendFormat("   volume  {0} {1}\n", s.BuyWeight.yesterday.volume.ToString("F9"), s.SellWeight.yesterday.volume.ToString("F9"));
            sb.AppendFormat("t: open    {0} {1}\n", s.BuyWeight.today.open.ToString("F9"), s.SellWeight.today.open.ToString("F9"));
            sb.AppendFormat("   close   {0} {1}\n", s.BuyWeight.today.close.ToString("F9"), s.SellWeight.today.close.ToString("F9"));
            sb.AppendFormat("   high    {0} {1}\n", s.BuyWeight.today.high.ToString("F9"), s.SellWeight.today.high.ToString("F9"));
            sb.AppendFormat("   low     {0} {1}\n", s.BuyWeight.today.low.ToString("F9"), s.SellWeight.today.low.ToString("F9"));
            sb.AppendFormat("   volume  {0} {1}\n", s.BuyWeight.today.volume.ToString("F9"), s.SellWeight.today.volume.ToString("F9"));
            MainWindow.tradeWeightText = sb.ToString();
        }

        private static double proofStrategy(Strategy s, List<Quote> q)
        {
            // Set up a copy with the same weights
            // but a new portfolio and history
            Strategy copy = s.copy();
            int qCount = q.Count;

            // Run the strategy and get the percent profit
            runStrategy(copy, q, (qCount - (qCount / 5)), qCount);
            double profit = percentProfit(copy);

            return profit;
        }
        internal static void main()
        {
            long gCount = long.MaxValue; // generations
            int sCount = 100; // strategies

            // initialize quotes
            List<Quote> quotes = buildQuotes();

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
                generation(strategies, sCount, quotes);

                Enumerable.OrderBy<Strategy, double>(strategies, s => { return s.Result; });
                
                //strategies.Sort();

                printResults(strategies, sCount, i, quotes);

                if (i != gCount - 1)
                    mutate(strategies, sCount);
            }
        }
    }
}
