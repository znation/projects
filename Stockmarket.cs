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
            int low = 20,
                high = 80,
                period = 200;
            DateTime date = DateTime.Today.AddDays(-(365*10));
            for (int i = 0; i < 365 * 10; i++)
            {
                double today = Math.Sin(((double)i / (double)period) * 2 * Math.PI);
                today *= (high - low);
                today += low;
                Quote q = new Quote(date.Month, date.Day, date.Year, today, today, today, today, 100000);
                quotes.Add(q);
                date = date.AddDays(1);
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

                    Quote q = new Quote(int.Parse(dateParts[0]),
                        int.Parse(dateParts[1]),
                        int.Parse(dateParts[2]),
                        double.Parse(parts[1]),
                        double.Parse(parts[4]),
                        double.Parse(parts[2]),
                        double.Parse(parts[3]),
                        long.Parse(parts[5]));
                    quotes.Add(q);
                }
            }

            return quotes;
        }
        private static int buy(double price, int shares, Strategy s)
        {
            Debug.Assert(shares > 0);
            if (s.Portfolio.money >= (price * shares) + COMMISSION)
            {
                s.Portfolio = new Portfolio(s.Portfolio.shares + shares,
                    s.Portfolio.money - (price * shares) + COMMISSION,
                    s.Portfolio.trades + 1);
                return shares;
            }
            return 0;
        }
        private static int sell(double price, Strategy s)
        {
            int shares = s.Portfolio.shares;
            Debug.Assert(shares > 0);
            if (s.Portfolio.shares >= shares)
            {
                s.Portfolio = new Portfolio(
                    s.Portfolio.shares - shares,
                    s.Portfolio.money + (price * shares) - COMMISSION,
                    s.Portfolio.trades + 1);
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
            if ((yesterday.close * weight.yesterday.close) > (today.close * weight.today.close))
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
                return buy(today.close, shares, s);

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
                return sell(today.close, s);

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
            s.Portfolio = new Portfolio();

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
                    TradeRecord trade = new TradeRecord(type,
                        today.month,
                        today.day,
                        today.year,
                        today.close,
                        shares,
                        s.Portfolio.money);
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
        private static Strategy spawn(Strategy source)
        {
            Strategy dest = source.copy();

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

        internal static double proofStrategy(Strategy s, List<Quote> q)
        {
            // Set up a copy with the same weights
            // but a new portfolio and history
            int qCount = q.Count;

            // Run the strategy and get the percent profit
            runStrategy(s, q, (qCount - (qCount / 5)), qCount);
            double profit = percentProfit(s);

            return profit;
        }
        internal static void main()
        {
            long gCount = long.MaxValue; // generations
            int sCount = 100; // strategies

            // initialize quotes
            //List<Quote> quotes = buildQuotes();
            List<Quote> quotes = sineWaveQuotes();

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

                Strategy.Sort(strategies);

                Debug.Assert(Strategy.Sorted(strategies));
                
                // set context for the render thread
                lock (MainWindow.updateLock)
                {
                    MainWindow.s_strategies = Strategy.copy(strategies);
                    MainWindow.s_quotes = quotes;
                    MainWindow.s_gIdx = i;
                }

                if (i != gCount - 1)
                    mutate(strategies, sCount);
            }
        }
    }
}
