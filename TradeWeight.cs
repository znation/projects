using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace stockmarket
{
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

        internal Day yesterday { get; private set; }
        internal Day today { get; private set; }
        internal double overall { get; private set; }

        internal void Mutate()
        {
            double increment = (((Stockmarket.rand.Next() % 1000.0)) - 500.0) / 1000.0;

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
            if (d < -1.0)
                return -1.0;
            return d;
        }

    }
}
