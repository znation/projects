using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Diagnostics;
using System.Threading;

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
        internal static readonly Money STARTING_MONEY = new Money(100000.0m);
        private static readonly Money COMMISSION = new Money(8.0m);
        internal static readonly Random rand = new Random();

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
                        new Money(decimal.Parse(parts[4])));
                    quotes.Add(q);
                }
            }

            return quotes;
        }
        private static int buy(Money price, int shares, Strategy s)
        {
            Debug.Assert(shares > 0);
            if (s.Portfolio.money.CanBuy(price, shares, COMMISSION))
            {
                s.Portfolio = new Portfolio(s.Portfolio.shares + shares,
                    s.Portfolio.money.Buy(price, shares, COMMISSION),
                    s.Portfolio.trades + 1);
                return shares;
            }
            return 0;
        }
        private static int sell(Money price, Strategy s)
        {
            int shares = s.Portfolio.shares;
            Debug.Assert(shares > 0);
            if (s.Portfolio.shares >= shares)
            {
                s.Portfolio = new Portfolio(
                    s.Portfolio.shares - shares,
                    s.Portfolio.money.Sell(price, shares, COMMISSION),
                    s.Portfolio.trades + 1);
                return shares;
            }
            return 0;
        }
        private static bool maybe(Quote yesterday, Quote today, TradeWeight weight)
        {
            if ((yesterday.close.ToDouble() * weight.yesterday.close) > (today.close.ToDouble() * weight.today.close))
                return true;
            return false;
        }
        private static int maybeBuy(Quote yesterday, Quote today, Strategy s)
        {
            if (s.Portfolio.money <= COMMISSION)
                return 0;

            int shares = s.Portfolio.money.CalculateShares(COMMISSION, today.close);
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
        private static void runStrategy(Strategy s, List<Quote> q, int qFirst, int qLast)
        {
            // initialize portfolio
            s.Portfolio = new Portfolio();

            // initialize trade history
            s.Trades.Clear();

            Money lastPrice = new Money(0.0m);
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
            s.Result = Money.CalculateResult(lastPrice, s.Portfolio);
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
            Money profit = s.Result - STARTING_MONEY;
            profit /= STARTING_MONEY;
            double percent = profit.ToDouble();
            return percent * 100.0;
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

        private static void printResults(List<Strategy> s_strategies, List<Quote> s_quotes, long s_gIdx)
        {
            StringBuilder sb = new StringBuilder();
            List<Money> results = new List<Money>();
            Money mean,
                median,
                    best,
                    worst;
            int meanTrades = 0,
                medianTrades,
                bestTrades,
                worstTrades,
                sCount;
            long generation;
            double profitability;

            sCount = s_strategies.Count;
            generation = s_gIdx;
            median = s_strategies[sCount / 2].Result;
            best = s_strategies[0].Result;
            worst = s_strategies[sCount - 1].Result;
            medianTrades = s_strategies[sCount / 2].Portfolio.trades;
            bestTrades = s_strategies[0].Portfolio.trades;
            worstTrades = s_strategies[sCount - 1].Portfolio.trades;
            profitability = Stockmarket.proofStrategy(s_strategies[0].copy(), s_quotes);

            Debug.Assert(s_strategies[0].Score() >= s_strategies[1].Score());
            Debug.Assert(s_strategies[0].Score() >= s_strategies[sCount - 1].Score());
            Debug.Assert(s_strategies[0].Result >= new Money(0.0m));

            for (int i = 0; i < sCount; i++)
            {
                results.Add(s_strategies[i].Result);
                meanTrades += s_strategies[i].Portfolio.trades;
            }
            mean = Money.Mean(results);

            meanTrades /= sCount;

            sb.AppendFormat("Generation:    {0}\n", generation);
            sb.AppendFormat("Median:        {0}\n", median.ToString());
            sb.AppendFormat("Median Trades: {0}\n", medianTrades);
            sb.AppendFormat("Mean:          {0}\n", mean.ToString());
            sb.AppendFormat("Mean Trades:   {0}\n", meanTrades);
            sb.AppendFormat("Worst:         {0}\n", worst.ToString());
            sb.AppendFormat("Worst Trades:  {0}\n", worstTrades);
            sb.AppendFormat("Best:          {0}\n", best.ToString());
            sb.AppendFormat("Best Trades:   {0}\n", bestTrades);
            sb.AppendFormat("Profitability: {0}\n", profitability.ToString("F3"));

            MainWindow.resultText = sb.ToString();
            MainWindow.tradeWeightText = printTradeWeight(s_strategies[0]);
            MainWindow.tradeGraph = BuildTradeGraph(s_strategies[0].Trades, s_quotes);
        }

        private static bool[] BuildTradeGraph(List<TradeRecord> list, List<Quote> quotes)
        {
            int qCount = quotes.Count;
            Quote firstDay = quotes[0];
            Quote lastDay = quotes[quotes.Count - 1];
            bool[] trades = new bool[qCount];
            bool holding = false;
            int listIdx = 0;
            DateTime prevTradeDate = new DateTime(firstDay.year, firstDay.month, firstDay.day);
            for (int i = 0; i < qCount; i++)
            {
                if (list.Count > listIdx)
                {
                    TradeRecord nextTrade = list[listIdx];
                    DateTime nextTradeDate = new DateTime(nextTrade.year, nextTrade.month, nextTrade.day);
                    int diff = nextTradeDate.Subtract(prevTradeDate).Days;
                    if (i == diff)
                    {
                        // trade nextTrade takes place today
                        if (nextTrade.type == TradeAction.BUY)
                        {
                            holding = true;
                        }
                        else
                        {
                            Debug.Assert(nextTrade.type == TradeAction.SELL);
                            holding = false;
                        }
                        listIdx++;
                    }
                }

                trades[i] = holding;
            }

            return trades;
        }

        private static string printTradeWeight(Strategy s)
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

            return sb.ToString();
        }

        internal static void main()
        {
            Thread.CurrentThread.Name = "Stockmarket Main";
            long gCount = long.MaxValue; // generations
            int sCount = 100; // strategies

            // initialize quotes
            List<Quote> quotes = buildQuotes();
            MainWindow.quoteGraph = new QuoteGraph(quotes);
            

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
                
                // set context for the render thread
                if (i % 20 == 0)
                    printResults(strategies, quotes, i);

                if (i != gCount - 1)
                    mutate(strategies, sCount);
            }
        }
    }
}
