using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Threading;
using System.ComponentModel;
using System.Diagnostics;

namespace stockmarket
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private List<Thread> threads;
        private string resultText = String.Empty;
        private string tradeWeightText = String.Empty;
        internal static Object updateLock = new Object();
        internal static List<Strategy> s_strategies;
        internal static long s_gIdx;
        internal static List<Quote> s_quotes;
        private bool resultsUpdated = false;
        private List<Rectangle[,]> gridContents = null;

        public MainWindow()
        {
            InitializeComponent();
            
            threads = new List<Thread>();
            threads.Add(new Thread(Stockmarket.main));

            this.Closing += new CancelEventHandler(MainWindow_Closing);
            CompositionTarget.Rendering += new EventHandler(Render);
        }

        private int renderCount = 0;
        void Render(object sender, EventArgs e)
        {
            renderCount++;
            if (renderCount % 10 != 0)
                return;

            UpdateResults();
            if (resultsUpdated && gridContents == null)
            {
                gridContents = new List<Rectangle[,]>();
                QuoteGraph qg = new QuoteGraph(s_quotes);

                int w = (int)Panel.ActualWidth;
                int h = 100;
                Rectangle[,] rect = new Rectangle[w, h];
                WrapPanel grid = new WrapPanel();
                grid.Width = w;
                grid.Height = h;
                for (int i = 0; i < h; i++)
                {
                    for (int j = 0; j < w; j++)
                    {
                        Rectangle r = new Rectangle();
                        r.Width = 1;
                        r.Height = 1;
                        if (i == qg.Values[j])
                            r.Fill = Brushes.Black;
                        else
                            r.Fill = Brushes.LightGray;
                        rect[j, i] = r;
                        grid.Children.Add(r);
                    }
                }
                gridContents.Add(rect);
                Panel.Children.Add(grid);
            }
        }

        void MainWindow_Closing(object sender, CancelEventArgs e)
        {
            foreach (Thread thread in threads)
            {
                thread.Abort();
            }
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            foreach (Thread thread in threads)
            {
                thread.Start();
            }
        }

        private void UpdateResults()
        {
            lock (updateLock)
            {
                printResults();
            }
            Results.Text = resultText;
            TradeWeight.Text = tradeWeightText;
        }

        private void printResults()
        {
            if (s_strategies == null
                || s_quotes == null)
                return;

            StringBuilder sb = new StringBuilder();
            int sCount = s_strategies.Count;

            decimal mean = 0.0m,
                median = s_strategies[sCount / 2].Result,
                best = s_strategies[0].Result,
                worst = s_strategies[s_strategies.Count - 1].Result;
            int meanTrades = 0,
                medianTrades = s_strategies[sCount / 2].Portfolio.trades,
                bestTrades = s_strategies[0].Portfolio.trades,
                worstTrades = s_strategies[s_strategies.Count - 1].Portfolio.trades;
            int i;
            for (i = 0; i < sCount; i++)
            {
                mean += s_strategies[i].Result;
                meanTrades += s_strategies[i].Portfolio.trades;
            }
            mean /= sCount;
            meanTrades /= sCount;

            sb.AppendFormat("Generation:    {0}\n", s_gIdx);
            sb.AppendFormat("Median:        {0}\n", median.ToString("F2"));
            sb.AppendFormat("Median Trades: {0}\n", medianTrades);
            sb.AppendFormat("Mean:          {0}\n", mean.ToString("F2"));
            sb.AppendFormat("Mean Trades:   {0}\n", meanTrades);
            sb.AppendFormat("Worst:         {0}\n", worst.ToString("F2"));
            sb.AppendFormat("Worst Trades:  {0}\n", worstTrades);
            sb.AppendFormat("Best:          {0}\n", best.ToString("F2"));
            sb.AppendFormat("Best Trades:   {0}\n", bestTrades);
            sb.AppendFormat("Profitability: {0}\n", Stockmarket.proofStrategy(s_strategies[0], s_quotes).ToString("F2"));

            resultText = sb.ToString();

            printTradeWeight(s_strategies[0]);

            resultsUpdated = true;
        }

        private void printTradeWeight(Strategy s)
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
            tradeWeightText = sb.ToString();
        }

    }
}
