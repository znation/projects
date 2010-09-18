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
        private const int ROWSEP = 15;
        private readonly Color TRANSPARENT;
        private readonly Color DARKRED;
        private readonly List<Thread> threads;

        internal static volatile string resultText = String.Empty;
        internal static volatile string tradeWeightText = String.Empty;
        internal static volatile QuoteGraph quoteGraph = null;
        internal static volatile bool[] tradeGraph = null;
        internal static volatile bool hasUpdates = false;

        private DrawingSurface priceGrid = null;
        private DrawingSurface tradeGrid = null;
        private int graphWidth;
        private int graphHeight;
        private int rows;
        
        public MainWindow()
        {
            TRANSPARENT = new Color();
            TRANSPARENT.A = 0;
            TRANSPARENT.R = 0;
            TRANSPARENT.G = 0;
            TRANSPARENT.B = 0;

            DARKRED = Colors.DarkRed;
            DARKRED.A = Byte.MaxValue / 2;

            threads = new List<Thread>();
            threads.Add(new Thread(Stockmarket.main));
            InitializeComponent();

            this.Closing += new CancelEventHandler(MainWindow_Closing);
            CompositionTarget.Rendering += new EventHandler(Render);
        }

        void Render(object sender, EventArgs e)
        {
            if (!hasUpdates)
                return;

            hasUpdates = false;
            Results.Text = resultText;
            TradeWeight.Text = tradeWeightText;
            AttachPriceGraph();
            UpdateTradeGraph();
        }

        private void UpdateTradeGraph()
        {
            if (tradeGraph == null ||
                tradeGrid == null)
                return;

            tradeGrid.Bitmap.Lock();
            for (int i = 0; i < rows; i++)
            {
                for (int y = 0; y < QuoteGraph.ROWHEIGHT; y++)
                {
                    for (int x = 0; x < graphWidth; x++)
                    {
                        int tradeIdx = (i * graphWidth) + x;
                        int adjustedY = y + (i * (QuoteGraph.ROWHEIGHT + ROWSEP));
                        if (tradeIdx < tradeGraph.Length && tradeGraph[tradeIdx])
                            tradeGrid.DrawPixelUnlocked(x, adjustedY, DARKRED);
                        else
                            tradeGrid.DrawPixelUnlocked(x, adjustedY, TRANSPARENT);
                    }
                }
            }
            tradeGrid.Bitmap.Unlock();
        }

        private void AttachPriceGraph()
        {
            if (quoteGraph == null ||
                priceGrid != null)
                return;

            graphWidth = (int)Panel.ActualWidth;
            rows = (int)Math.Ceiling((double)quoteGraph.Values.Length / (double)graphWidth);
            graphHeight = (QuoteGraph.ROWHEIGHT * rows) + (ROWSEP * (rows - 1));

            GraphCanvas.Width = graphWidth;

            priceGrid = new DrawingSurface(graphWidth, graphHeight);
            PriceGraph.Source = priceGrid.Bitmap;

            tradeGrid = new DrawingSurface(graphWidth, graphHeight);
            TradeGraph.Source = tradeGrid.Bitmap;

            priceGrid.Bitmap.Lock();
            tradeGrid.Bitmap.Lock();

            for (int row = 0; row < rows; row++)
            {
                for (int y = 0; y < QuoteGraph.ROWHEIGHT; y++)
                {
                    for (int x = 0; x < graphWidth; x++)
                    {
                        int quoteIdx = x+(row*graphWidth);
                        if (quoteGraph.Values.Length > quoteIdx)
                        {
                            if (y == quoteGraph.Values[quoteIdx])
                                priceGrid.DrawPixelUnlocked(x, y + (row * (QuoteGraph.ROWHEIGHT + ROWSEP)), Colors.Black);
                        }
                    }
                }
            }

            priceGrid.Bitmap.Unlock();
            tradeGrid.Bitmap.Unlock();
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
    }
}
