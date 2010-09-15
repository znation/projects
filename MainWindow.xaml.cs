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
        private List<Rectangle[,]> gridContents = null;
        
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

        private void AttachQuoteGraph()
        {
            if (quoteGraph == null)
                return;
            if (gridContents != null)
                return;

            gridContents = new List<Rectangle[,]>();
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
                    if (i == quoteGraph.Values[j])
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
