#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <time.h>
#include "analyzequotes.h"

Quote * buildQuotes(int count)
{
	int month,day,year;
	double open,high,low,close,volume;
	int i=0;
	Quote *quotes = (Quote *) calloc(count, sizeof(Quote));

	FILE *fp;
	fp = fopen("charts.tsv", "r");
	
	if (fp == NULL)
	{
		fprintf(stderr, "Can't open input file!\n");
	}
	
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
double dblRemainder(double a)
{
	return a - ((int) a);
}
int maybe(Quote yesterday, Quote today, TradeWeight *weight)
{
	double x = ((yesterday.open * weight->yesterday.open)
		+ (yesterday.close * weight->yesterday.close)
		+ (yesterday.high * weight->yesterday.high)
		+ (yesterday.low * weight->yesterday.low)
		+ (yesterday.volume * weight->yesterday.volume)
		+ (today.open * weight->today.open)
		+ (today.close * weight->today.close)
		+ (today.high * weight->today.high)
		+ (today.low * weight->today.low)
		+ (today.volume * weight->today.volume))
		* weight->overall;
	if (x >= 0.5)
		return 1;
	return 0;
}
int maybeBuy(Quote yesterday, Quote today, TradeWeight *buyWeight, Portfolio *portfolio)
{
	if (!maybe(yesterday, today, buyWeight))
		return 0;

	int shares = (portfolio->money - 8.00) / today.close;
	return buy(today.close, shares, portfolio);
}
int maybeSell(Quote yesterday, Quote today, TradeWeight *sellWeight, Portfolio *portfolio)
{
	if (!maybe(yesterday, today, sellWeight))
		return 0;

	return sell(today.close, portfolio->shares, portfolio);
}
TradeWeight * randomWeight()
{
	TradeWeight *weight = malloc(sizeof(TradeWeight));
	uchar *data = (uchar *) weight;

	int i;
	int chars = sizeof(TradeWeight);
	for (i=0; i<chars; i++)
	{
		data[i] = rand() % (UCHAR_MAX + 1);
	}

	return weight;
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
			if (s[j+1].result > s[j].result && s[j+1].trades > 0)      // ascending order simply changes to <
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
void generation(Strategy *s, int sCount, Quote *q, int qCount)
{
	int j;
	for (j=0; j<sCount; j++)
	{
		// initialize portfolio
		Portfolio portfolio;
		portfolio.money = 10000.00;
		portfolio.shares = 0;
		portfolio.commission = 8.00;
		portfolio.trades = 0;

		double lastPrice = 0.0;
		int i;
		for (i=1; i<(qCount/5); i++)
		{
			Quote yesterday = q[i-1];
			Quote today = q[i];
		
			maybeBuy(yesterday, today, s[j].buyWeight, &portfolio) ||
			maybeSell(yesterday, today, s[j].sellWeight, &portfolio);
			
			lastPrice = today.close;
		}

		s[j].result = portfolio.money + (portfolio.shares * lastPrice);
		s[j].trades = portfolio.trades;

	}
}
void copyBytes(TradeWeight *twSource, TradeWeight *twDest)
{
	uchar *source = (uchar *) twSource;
	uchar *dest = (uchar *) twDest;

	unsigned int i;
	for (i=0; i<sizeof(TradeWeight); i++)
	{
		dest[i] = source[i];
	}
}
void mutate(Strategy *s, int sCount)
{
	// leave the top 25 alone -- mutate the bottom 25 based on the top 25
	// (#26 is a spawn of #1, #27 of #2, etc.)

	int i;
	for (i=0; i<sCount/2; i++)
	{
		Strategy *source = &(s[i]);
		Strategy *dest = &(s[i+(sCount/2)]);

		// reset the result and trades
		source->result = 0.0;
		source->trades = 0;
		dest->result = 0.0;
		dest->trades = 0;
		
		// copy source to dest
		copyBytes(source->buyWeight, dest->buyWeight);
		copyBytes(source->sellWeight, dest->sellWeight);

		// mutate destination
		{
			// pick either buy or sell weight randomly
			uchar *weight = NULL;
			if (rand() % 2)
				weight = (uchar *) dest->buyWeight;
			else
				weight = (uchar *) dest->sellWeight;

			// pick a bit index randomly
			unsigned int idx = rand() % sizeof(TradeWeight);
			while (idx >= 8)
			{
				weight++;
				idx -= 8;
			}
			uchar mask = 1 << idx;
			weight[0] ^= mask;
		}

		normalizeWeight(dest->buyWeight);
		normalizeWeight(dest->sellWeight);
	}
}
void printResults(Strategy *s, int sCount, int gCount)
{
	double median = s[sCount/2].result;
	double mean = 0.0;
	int i;
	for (i=0; i<sCount; i++)
	{
		mean += s[i].result;
	}
	mean /= sCount;
	double worst = s[sCount-1].result;
	double best = s[0].result;

	printf("%d,%lf,%lf,%lf,%lf\n",
		gCount,
		median,
		mean,
		worst,
		best);
}
int main()
{	
	int gCount = 200, // generations
		qCount = 2600, // quotes
		sCount = 100; // strategies

	// initialize random number generator
	srand(time(NULL));

	// initialize quotes
	Quote *quotes = buildQuotes(qCount);

	// initialize trade weights / strategies
	Strategy *strategies = calloc(sCount, sizeof(Strategy));
	int i;
	for (i=0; i<sCount; i++)
	{
		strategies[i].buyWeight = randomWeight();
		strategies[i].sellWeight = randomWeight();
		strategies[i].result = 0.0;
		strategies[i].trades = 0;

		normalizeWeight(strategies[i].buyWeight);
		normalizeWeight(strategies[i].sellWeight);
	}

	printf("Generation,Median,Mean,Worst,Best\n");

	for (i=0; i<gCount; i++)
	{
		generation(strategies, sCount, quotes, qCount);
		bubbleSort(strategies, sCount);
		printResults(strategies, sCount, i);
		mutate(strategies, sCount);
	}
	
	return 0;
}
void normalizeWeight(TradeWeight *w)
{
	w->yesterday.open = dblRemainder(w->yesterday.open);
	w->yesterday.close = dblRemainder(w->yesterday.close);
	w->yesterday.high = dblRemainder(w->yesterday.high);
	w->yesterday.low = dblRemainder(w->yesterday.low);
	w->yesterday.volume = dblRemainder(w->yesterday.volume);
	w->today.open = dblRemainder(w->today.open);
	w->today.close = dblRemainder(w->today.close);
	w->today.high = dblRemainder(w->today.high);
	w->today.low = dblRemainder(w->today.low);
	w->today.volume = dblRemainder(w->today.volume);
}
