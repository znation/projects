#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <time.h>

typedef unsigned char uchar;

typedef struct
{
	int shares;
	double money;
	int trades;
	double commission;
} Portfolio;

typedef struct
{
	int month,day,year;
	double open,
		close,
		high,
		low,
		volume;
} Quote;

typedef struct
{
	struct
	{
		double open,
		close,
		high,
		low,
		volume;
	} yesterday;
	struct
	{
		double open,
		close,
		high,
		low,
		volume;
	} today;
} TradeWeight;



Quote * buildQuotes(int count)
{
	Quote *quotes = (Quote *) calloc(count, sizeof(Quote));

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
	
	return quotes;
}

int buy(double price, int shares, Portfolio *portfolio)
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

int sell(double price, int shares, Portfolio *portfolio)
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

int maybe(Quote yesterday, Quote today, TradeWeight weight)
{
	return 0;
}

int maybeBuy(Quote yesterday, Quote today, TradeWeight buyWeight, Portfolio *portfolio)
{
	if (!maybe(yesterday, today, buyWeight))
		return 0;

	int shares = (portfolio->money - 8.00) / today.close;
	return buy(today.close, shares, portfolio);
}

int maybeSell(Quote yesterday, Quote today, TradeWeight sellWeight, Portfolio *portfolio)
{
	if (!maybe(yesterday, today, sellWeight))
		return 0;

	return sell(today.close, portfolio->shares, portfolio);
}

TradeWeight * randomWeights(int count)
{
	TradeWeight *weights = calloc(count, sizeof(TradeWeight));

	int i;
	for (i=0; i<count; i++)
	{
		
	}

	return weights;
}

int main(int argc, char ** argv)
{	
	// initialize random number generator
	srand(time(NULL));

	// initialize portfolio
	Portfolio *portfolio = (Portfolio *) malloc(sizeof(Portfolio));
	portfolio->money = 10000.00;
	portfolio->shares = 0;
	portfolio->commission = 8.00;
	portfolio->trades = 0;

	// initialize quotes
	int size = 2600;
	Quote *quotes = buildQuotes(size);

	// initialize trade weights (start with 50 each buy/sell)
	int numWeights = 50;
	TradeWeight *buyWeights = randomWeights(numWeights);
	TradeWeight *sellWeights = randomWeights(numWeights);

	double lastPrice = 0.0;
	int i;
	for (i=1; i<(size/5); i++)
	{
		int j;
		for (j=0; j<numWeights; j++)
		{
			Quote yesterday = quotes[i-1];
			Quote today = quotes[i];
		
			maybeBuy(yesterday, today, buyWeights[j], portfolio) ||
			maybeSell(yesterday, today, sellWeights[j], portfolio);
			
			lastPrice = today.close;
		}
	}
	
	printf("Results:\n");
	printf("Days: %d\n", i);
	printf("Money: $%f\n", portfolio->money);
	printf("Trades: %d\n", portfolio->trades);
	printf("Shares: %d at $%f/share\n", portfolio->shares, lastPrice);
	printf("Total: $%f\n", portfolio->money + (portfolio->shares * lastPrice));

	return 0;
}
