#include <stdio.h>
#include <stdlib.h>

struct Portfolio
{
	int shares;
	double money;
	int trades;
	double commission;
};

struct Quote
{
	int month,day,year;
	double open,
		close,
		high,
		low,
		volume;
};

struct TradeWeight
{
	double open,
		close,
		high,
		low,
		volume,
		price,
		shares;
};

int buildQuotes(struct Quote * quotes)
{
	FILE *fp;
	fp = fopen("charts.tsv", "r");
	
	if (fp == NULL)
	{
		fprintf(stderr, "Can't open input file!\n");
	}
	
	int month,day,year;
	double open,high,low,close,volume;
	int i=0;
	//				m  d  y open high low close
	while (fscanf(fp,
					"%d/%d/%d\t%lf %lf %lf %lf %lf\n",
					&month,
					&day,
					&year,
					&open,
					&high,
					&low,
					&close,
					&volume) != EOF)
	{
		quotes[i].month = month;
		quotes[i].day = day;
		quotes[i].year = year;
		quotes[i].open = open;
		quotes[i].high = high;
		quotes[i].low = low;
		quotes[i].close = close;
		i++;
	}
	
	return i;
}

int buy(double price, int shares, struct Portfolio *portfolio)
{
	if (portfolio->money >= (price * shares) + portfolio->commission)
	{
		portfolio->shares += shares;
		portfolio->money -= (price * shares) + portfolio->commission;
		portfolio->trades++;
		return shares;
	}
	return 0;
}

int buyMax(double price, struct Portfolio *portfolio)
{
	int shares = (portfolio->money - 8.00) / price;
	return buy(price, shares, portfolio);
}

int sell(double price, int shares, struct Portfolio *portfolio)
{
	if (portfolio->shares >= shares)
	{
		portfolio->shares -= shares;
		portfolio->money += (price * shares) - portfolio->commission;
		portfolio->trades++;
		return shares;
	}
	return 0;
}

int sellMax(double price, struct Portfolio * portfolio)
{
	return sell(price, portfolio->shares, portfolio);
}

int main(int argc, char ** argv)
{	
	struct Portfolio * portfolio = (struct Portfolio *) malloc(sizeof(struct Portfolio));
	portfolio->money = 10000.00;
	portfolio->shares = 0;
	portfolio->commission = 8.00;
	portfolio->trades = 0;

	struct Quote * quotes = (struct Quote *) calloc(2600, sizeof(struct Quote));
	int size = buildQuotes(quotes);
	int i;
	double lastPrice = 0.0;
	for (i=2; i<size; i++)
	{
		struct Quote dayBefore = quotes[i-2];
		struct Quote yesterday = quotes[i-1];
		struct Quote today = quotes[i];
		
		double dayBeforeDiffOpenClose = dayBefore.close - dayBefore.open;
		double yestDiffOpenClose = yesterday.close - yesterday.open;
		double todayDiffOpenClose = today.close - today.open;
		
		if (dayBeforeDiffOpenClose + yestDiffOpenClose + todayDiffOpenClose < 0 && portfolio->money > today.close)
			buyMax(today.close, portfolio);
		else if (dayBeforeDiffOpenClose + yestDiffOpenClose + todayDiffOpenClose > 0 && portfolio->shares > 0)
			sellMax(today.close, portfolio);
			
		lastPrice = today.close;
	}
	
	printf("Results:\n");
	printf("Days: %d\n", i);
	printf("Money: $%f\n", portfolio->money);
	printf("Trades: %d\n", portfolio->trades);
	printf("Shares: %d at $%f/share\n", portfolio->shares, lastPrice);
	printf("Total: $%f\n", portfolio->money + (portfolio->shares * lastPrice));
	
	return 0;
}
