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
        private WriteableBitmap gridBitmap = null;
        
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
        private void DrawPixel(int x, int y, Color c)
        {
            unsafe
            {
                // Get a pointer to the back buffer.
                int pBackBuffer = (int)gridBitmap.BackBuffer;

                // Find the address of the pixel to draw.
                pBackBuffer += y * gridBitmap.BackBufferStride;
                pBackBuffer += x * 4;

                // Compute the pixel's color.
                int color_data = c.A << 24;
                color_data |= c.R << 16;
                color_data |= c.G << 8;
                color_data |= c.B << 0;

                // Assign the color data to the pixel.
                *((int*)pBackBuffer) = color_data;
            }

            // Specify the area of the bitmap that changed.
            gridBitmap.AddDirtyRect(new Int32Rect(x, y, 1, 1));
        }

        private void AttachQuoteGraph()
        {
            if (quoteGraph == null)
                return;
            if (gridBitmap != null)
                return;

            int w = (int)Panel.ActualWidth;
            int h = 100; 
            gridBitmap = new WriteableBitmap(w, h, 96, 96, PixelFormats.Bgra32, null);
            PriceGraph.Source = gridBitmap;
            PriceGraph.Stretch = Stretch.None;

            gridBitmap.Lock();
            for (int i = 0; i < h; i++)
            {
                for (int j = 0; j < w; j++)
                {
                    if (i == quoteGraph.Values[j])
                        DrawPixel(j, i, Colors.Black);
                    else
                        DrawPixel(j, i, Colors.LightGray);
                }
            }
            gridBitmap.Unlock();
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
