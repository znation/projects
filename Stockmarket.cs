using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Diagnostics;

namespace stockmarket
{
    internal struct Portfolio
    {
        internal int shares;
        internal double money;
        internal int trades;
    }

    internal struct Quote
    {
        internal int month, day, year;
        internal double open,
            close,
            high,
            low;
        internal long volume;
    }

    internal struct TradeWeight
    {
        internal struct Day
        {
            internal double open,
            close,
            high,
            low,
            volume;
        }
        internal Day yesterday;
        internal Day today;
        internal double overall;
    }

    internal enum TradeAction
    {
        BUY,
        SELL,
        NONE
    }

    internal struct TradeRecord
    {
        internal TradeAction type; // BOUGHT or SOLD
        internal int month, day, year;
        internal double price;
        internal int shares;
        internal double money;
    }

    internal class Strategy : IComparable
    {
        internal TradeWeight buyWeight;
        internal TradeWeight sellWeight;
        internal double result;
        internal List<TradeRecord> trades;
        internal Portfolio portfolio;

        int IComparable.CompareTo(object obj)
        {
            Strategy other = obj as Strategy;
            return this.result.CompareTo(other.result);
        }
    }

    internal static class Stockmarket
    {
        private const double STARTING_MONEY = 100000.0;
        private const double COMMISSION = 8.0;
        private const int MAX_QUOTES = 2858;
        private const int MAX_TRADES = MAX_QUOTES;
        private static readonly Random rand = new Random();


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
            if (s.portfolio.money <= COMMISSION)
                return 0;

            int shares = (int)((s.portfolio.money - COMMISSION) / today.close);
            if (shares <= 0)
                return 0;

            if (maybe(yesterday, today, s.buyWeight))
                return buy(today.close, shares, s.portfolio);

            return 0;
        }
        private static int maybeSell(Quote yesterday, Quote today, Strategy s)
        {
            if (s.portfolio.shares < 1)
                return 0;

            // don't sell if we'd lose money
            if ((s.trades)[s.portfolio.trades - 1].price > today.close)
                return 0;

            if (maybe(yesterday, today, s.sellWeight))
                return sell(today.close, s.portfolio.shares, s.portfolio);

            return 0;
        }
        private static TradeWeight randomWeight()
        {
            TradeWeight weight = new TradeWeight();
            randomizeWeight(weight);
            return weight;
        }
        private static void randomizeWeight(TradeWeight w)
        {
            w.overall = rand.NextDouble();

            Action<TradeWeight.Day> a = d =>
            {
                d.close = rand.NextDouble();
                d.high = rand.NextDouble();
                d.low = rand.NextDouble();
                d.open = rand.NextDouble();
                d.volume = rand.NextDouble();
            };

            a(w.today);
            a(w.yesterday);

            normalizeWeight(w);
        }
        private static double score(Strategy s)
        {
            if (s.portfolio.trades == 0)
                return double.MinValue; // the worst possible strategy is one that didn't trade at all
            return s.result * ((Math.Log10(s.portfolio.trades) / 10.0) + 1);
        }
        /*
        private static void bubbleSort(List<Strategy> s, int length)
        {
            int i, j;
            bool flag = true;    // set flag to true to start first pass
            Strategy temp;             // holding variable
            for (i = 1; (i <= length) && flag; i++)
            {
                flag = false;
                for (j = 0; j < (length - 1); j++)
                {
                    if (score(s[j + 1]) > score(s[j]))     // ascending order simply changes to <
                    {
                        temp = s[j];             // swap elements
                        s[j] = s[j + 1];
                        s[j + 1] = temp;
                        flag = true;               // indicates that a swap occurred.
                    }
                }
            }
            return;
        }*/
        private static void runStrategy(Strategy s, List<Quote> q, int qFirst, int qLast)
        {
            // initialize portfolio
            s.portfolio.money = STARTING_MONEY;
            s.portfolio.shares = 0;
            s.portfolio.trades = 0;

            // initialize trade history
            s.trades = new List<TradeRecord>();

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
                    Debug.Assert(s.portfolio.trades >= 1);
                    int idx = s.portfolio.trades - 1;
                    TradeRecord trade = s.trades[idx];
                    trade.shares = shares;
                    trade.type = type;
                    trade.price = today.close;
                    trade.month = today.month;
                    trade.day = today.day;
                    trade.year = today.year;
                    trade.money = s.portfolio.money;
                }

                lastPrice = today.close;
            }
            s.result = s.portfolio.money + (s.portfolio.shares * lastPrice);
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
        private static Portfolio initializePortfolio()
        {
            Portfolio p = new Portfolio()
            {
                money = STARTING_MONEY,
                shares = 0,
                trades = 0
            };
            return p;
        }
        private static Strategy spawn(Strategy source)
        {
            // reset the Strategy members
            source.result = 0.0;
            source.portfolio = initializePortfolio();

            Strategy dest = new Strategy();

            // copy source to dest
            copyBytes(source.buyWeight, dest.buyWeight);
            copyBytes(source.sellWeight, dest.sellWeight);

            // mutate destination
            // pick either buy or sell weight randomly
            TradeWeight weight;
            if (rand.Next(2) == 0)
                weight = dest.buyWeight;
            else
                weight = dest.sellWeight;

            double increment = (((double)(rand.Next() % 1000)) - 500.0) / 1000.0;

            // pick a double index randomly
            int idx = rand.Next(11);
            switch (idx)
            {
                case 0:
                    weight.overall += increment;
                    break;
                case 1:
                    weight.yesterday.close += increment;
                    break;
                case 2:
                    weight.yesterday.high += increment;
                    break;
                case 3:
                    weight.yesterday.low += increment;
                    break;
                case 4:
                    weight.yesterday.open += increment;
                    break;
                case 5:
                    weight.yesterday.volume += increment;
                    break;
                case 6:
                    weight.today.close += increment;
                    break;
                case 7:
                    weight.today.high += increment;
                    break;
                case 8:
                    weight.today.low += increment;
                    break;
                case 9:
                    weight.today.open += increment;
                    break;
                case 10:
                    weight.today.volume += increment;
                    break;
            }

            normalizeWeight(dest.buyWeight);
            normalizeWeight(dest.sellWeight);

            return dest;
        }

        private static void mutate(List<Strategy> s, int sCount)
        {
            // For the ones that traded, drop the bottom half,
            // and spawn new ones out of the top half.
            // For the ones that didn't trade, drop them and randomize

            int i;
            int tCount = 0; // traded count

            for (i = 0; i < sCount && s[i].portfolio.trades != 0; i++)
                tCount++;

            for (i = 0; i < tCount / 2; i++)
            {
                Strategy source = s[i];
                s[i + (tCount / 2)] = spawn(source);
            }

            // didn't trade -- randomize
            for (i = tCount; i < sCount; i++)
            {
                randomizeWeight(s[i].buyWeight);
                randomizeWeight(s[i].sellWeight);
            }
        }
        private static double percentProfit(Strategy s)
        {
            double profit = s.result - STARTING_MONEY;
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
            MainWindow.resultText = String.Format("best is {0}", s[0].result);
        }
        private static double proofStrategy(Strategy s, List<Quote> q, int qCount)
        {
            // Set up a copy with the same weights
            // but a new portfolio and history
            Strategy copy = copyStrategy(s);

            // Run the strategy and get the percent profit
            runStrategy(copy, q, (qCount - (qCount / 5)), qCount);
            double profit = percentProfit(copy);

            return profit;
        }

        private static Strategy copyStrategy(Strategy s)
        {
            Strategy copy = new Strategy()
            {
                buyWeight = copyTradeWeight(s.buyWeight),
                sellWeight = copyTradeWeight(s.sellWeight),
                portfolio = copyPortfolio(s.portfolio),
                result = s.result,
                trades = copyTradeRecords(s.trades)
            };
            return copy;
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

        private static Portfolio copyPortfolio(Portfolio portfolio)
        {
            Portfolio copy = new Portfolio()
            {
                money = portfolio.money,
                shares = portfolio.shares,
                trades = portfolio.trades
            };
            return copy;
        }
        private static TradeWeight copyTradeWeight(TradeWeight tradeWeight)
        {
            TradeWeight copy = new TradeWeight()
            {
                overall = tradeWeight.overall,
                yesterday = copyTradeWeightDay(tradeWeight.yesterday),
                today = copyTradeWeightDay(tradeWeight.today)
            };
            return copy;
        }

        private static TradeWeight.Day copyTradeWeightDay(TradeWeight.Day day)
        {
            TradeWeight.Day copy = new TradeWeight.Day()
            {
                close = day.close,
                high = day.high,
                low = day.low,
                open = day.open,
                volume = day.volume
            };
            return copy;
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
                Strategy s = new Strategy()
                {
                    buyWeight = randomWeight(),
                    sellWeight = randomWeight(),
                    result = 0.0,
                    portfolio = initializePortfolio(),
                    trades = new List<TradeRecord>()
                };
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
        private static void normalizeWeight(TradeWeight w)
        {
            w.overall = normalizeWeight(w.overall);
            Action<TradeWeight.Day> normalizeDay = new Action<TradeWeight.Day>(day =>
            {
                day.close = normalizeWeight(day.close);
                day.high = normalizeWeight(day.high);
                day.low = normalizeWeight(day.low);
                day.open = normalizeWeight(day.open);
                day.volume = normalizeWeight(day.volume);
            });
            normalizeDay(w.yesterday);
            normalizeDay(w.today);
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
}
