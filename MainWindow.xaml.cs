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
        private readonly List<Thread> threads;
        internal static volatile string resultText = String.Empty;
        internal static volatile string tradeWeightText = String.Empty;
        internal static volatile QuoteGraph quoteGraph = null;
        private DrawingSurface grid = null;
        
        public MainWindow()
        {
            threads = new List<Thread>();
            threads.Add(new Thread(Stockmarket.main));
            InitializeComponent();

            this.Closing += new CancelEventHandler(MainWindow_Closing);
            CompositionTarget.Rendering += new EventHandler(Render);
        }

        void Render(object sender, EventArgs e)
        {
            Results.Text = resultText;
            TradeWeight.Text = tradeWeightText;
            AttachQuoteGraph();
        }

        // The DrawPixel method updates the WriteableBitmap by using
        // unsafe code to write a pixel into the back buffer.
        

        private void AttachQuoteGraph()
        {
            if (quoteGraph == null)
                return;
            if (grid != null)
                return;

            int w = (int)Panel.ActualWidth;
            int rows = (int)Math.Ceiling((double)quoteGraph.Values.Length / (double)w);
            int h = (100 * rows) + (10 * (rows - 1));

            grid = new DrawingSurface(new WriteableBitmap(w, h, 96, 96, PixelFormats.Bgra32, null));
            PriceGraph.Source = grid.Bitmap;
            PriceGraph.Stretch = Stretch.None;

            grid.Bitmap.Lock();
            for (int row = 0; row < rows; row++)
            {
                for (int y = 0; y < 100; y++)
                {
                    for (int x = 0; x < w; x++)
                    {
                        if (y == quoteGraph.Values[x+(row*100)])
                            grid.DrawPixel(x, y+(row*110), Colors.Black);
                        else
                            grid.DrawPixel(x, y+(row*110), Colors.LightGray);
                    }
                }
            }
            grid.Bitmap.Unlock();
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
