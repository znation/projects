using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Numerics;

namespace stockmarket
{
    class Money
    {
        private long money { get; set; }

        private Money(long l)
        {
            money = l;
        }

        internal Money(decimal d)
        {
            decimal dollars = Decimal.Truncate(d);
            decimal cents = Decimal.Round(Decimal.Remainder(d, Decimal.One), 2);
            money = Decimal.ToInt64((100 * dollars) + cents);
        }

        private Money(BigInteger b)
        {
            this.money = (long)b;
        }

        public static Money operator +(Money lhs, Money rhs)
        {
            return new Money(lhs.money + rhs.money);
        }

        public static Money operator -(Money lhs, Money rhs)
        {
            return new Money(lhs.money - rhs.money);
        }

        public static bool operator <=(Money lhs, Money rhs)
        {
            return lhs.money <= rhs.money;
        }

        public static bool operator >=(Money lhs, Money rhs)
        {
            return lhs.money >= rhs.money;
        }

        public static Money operator /(Money lhs, Money rhs)
        {
            return new Money(lhs.money / rhs.money);
        }

        public static bool operator >(Money lhs, Money rhs)
        {
            return lhs.money > rhs.money;
        }

        public static bool operator <(Money lhs, Money rhs)
        {
            return lhs.money < rhs.money;
        }

        internal decimal ToDecimal()
        {
            return new Decimal(ToDouble());
        }

        internal double ToDouble()
        {
            return (money / 100) + ((double)(money % 100) / 100.0);
        }

        internal int CalculateShares(Money commission, Money close)
        {
            return (int)((this.money - commission.money) / close.money);
        }

        internal bool CanBuy(Money price, int shares, Money commission)
        {
            return this.money >= (price.money * shares) + commission.money;
        }

        internal Money Buy(Money price, int shares, Money commission)
        {
            return new Money(this.money - (price.money * shares) - commission.money);
        }

        internal Money Sell(Money price, int shares, Money commission)
        {
            return new Money(this.money + (price.money * shares) - commission.money);
        }

        internal static Money CalculateResult(Money lastPrice, Portfolio portfolio)
        {
            return new Money(portfolio.money.money + (lastPrice.money * portfolio.shares));
        }

        internal static Money Mean(List<Money> l)
        {
            BigInteger avg = 0;
            foreach (Money m in l)
            {
                avg += m.money;
            }
            avg /= l.Count;
            return new Money(avg);
        }

        public override string ToString()
        {
            long dollars = this.money / 100;
            byte cents = (byte)(this.money % 100);
            return String.Format("{0}.{1}", dollars, cents.ToString("D2"));
        }
    }
}
