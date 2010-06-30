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
	double overall;
} TradeWeight;

typedef struct
{
	TradeWeight buyWeight;
	TradeWeight sellWeight;
	double result;
} Strategy;

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

double modDouble(double a, double b)
{
	double result = (int) (a/b);
	return a - (result * b);
}

int maybe(Quote yesterday, Quote today, TradeWeight weight)
{
	double x = ((yesterday.open * modDouble(weight.yesterday.open, 1.0))
		+ (yesterday.close * modDouble(weight.yesterday.close, 1.0))
		+ (yesterday.high * modDouble(weight.yesterday.high, 1.0))
		+ (yesterday.low * modDouble(weight.yesterday.low, 1.0))
		+ (yesterday.volume * modDouble(weight.yesterday.volume, 1.0))
		+ (today.open * modDouble(weight.today.open, 1.0))
		+ (today.close * modDouble(weight.today.close, 1.0))
		+ (today.high * modDouble(weight.today.high, 1.0))
		+ (today.low * modDouble(weight.today.low, 1.0))
		+ (today.volume * modDouble(weight.today.volume, 1.0)))
		* weight.overall;
	if (x >= 0.5)
		return 1;
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
	uchar *data = (uchar *) weights;

	int i;
	int chars = count * sizeof(TradeWeight);
	for (i=0; i<chars; i++)
	{
		data[i] = rand() % (UCHAR_MAX + 1);
	}

	return weights;
}

void bubbleSort(Strategy *s, int length)
{
	int i, j, flag = 1;    // set flag to 1 to start first pass
	Strategy temp;             // holding variable
	for(i = 1; (i <= length) && flag; i++)
	{
		flag = 0;
		for (j=0; j < (length -1); j++)
		{
			if (s[j+1].result > s[j].result)      // ascending order simply changes to <
			{ 
				temp = s[j];             // swap elements
				s[j] = s[j+1];
				s[j+1] = temp;
				flag = 1;               // indicates that a swap occurred.
			}
		}
	}
	return;
}

int main(int argc, char ** argv)
{	
	// initialize random number generator
	srand(time(NULL));

	// initialize quotes
	int size = 2600;
	Quote *quotes = buildQuotes(size);

	// initialize trade weights (start with 50 each buy/sell)
	int numWeights = 50;
	TradeWeight *buyWeights = randomWeights(numWeights);
	TradeWeight *sellWeights = randomWeights(numWeights);
	Strategy *strategies = calloc(numWeights, sizeof(Strategy));

	//printf("Days,Money,Trades,Shares,Price/Share,Total\n");

	int j;
	for (j=0; j<numWeights; j++)
	{
		// initialize portfolio
		Portfolio *portfolio = (Portfolio *) malloc(sizeof(Portfolio));
		portfolio->money = 10000.00;
		portfolio->shares = 0;
		portfolio->commission = 8.00;
		portfolio->trades = 0;

		double lastPrice = 0.0;
		int i;
		for (i=1; i<(size/5); i++)
		{
			Quote yesterday = quotes[i-1];
			Quote today = quotes[i];
		
			maybeBuy(yesterday, today, buyWeights[j], portfolio) ||
			maybeSell(yesterday, today, sellWeights[j], portfolio);
			
			lastPrice = today.close;
		}

		/*
		printf("%d,$%f,%d,%d,$%f,$%f\n",
			i,
			portfolio->money,
			portfolio->trades,
			portfolio->shares,
			lastPrice,
			portfolio->money + (portfolio->shares * lastPrice));
		*/

		strategies[j].buyWeight = buyWeights[j];
		strategies[j].sellWeight = sellWeights[j];
		strategies[j].result = portfolio->money + (portfolio->shares * lastPrice);
	}

	bubbleSort(strategies, numWeights);

	return 0;
}
