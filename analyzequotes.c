#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include "analyzequotes.h"

#define STARTING_MONEY 10000.0

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
	randomizeWeight(weight);
	return weight;
}
void randomizeWeight(TradeWeight *w)
{
	uchar *data = (uchar *) w;

	int i;
	int chars = sizeof(TradeWeight);
	for (i=0; i<chars; i++)
	{
		data[i] = rand() % (UCHAR_MAX + 1);
	}

	normalizeWeight(w);
}
int score(Strategy s)
{
	if (s.trades == 0)
		return INT_MIN; // the worst possible strategy is one that didn't trade at all
	return s.result * log10(s.trades);
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
			if (score(s[j+1]) > score(s[j]))     // ascending order simply changes to <
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
	Portfolio portfolio;
	for (j=0; j<sCount; j++)
	{
		// initialize portfolio
		portfolio.money = STARTING_MONEY;
		portfolio.shares = 0;
		portfolio.commission = 8.00;
		portfolio.trades = 0;

		double lastPrice = 0.0;
		int i;
		for (i=1; i<(qCount/3); i++)
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
void spawn(Strategy *source, Strategy *dest)
{
	// reset the result and trades
	source->result = 0.0;
	source->trades = 0;
	dest->result = 0.0;
	dest->trades = 0;
		
	// copy source to dest
	copyBytes(source->buyWeight, dest->buyWeight);
	copyBytes(source->sellWeight, dest->sellWeight);

	// mutate destination
	int j;
	for (j=0; j<10; j++)
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
void mutate(Strategy *s, int sCount)
{
	// For the ones that traded, drop the bottom half,
	// and spawn new ones out of the top half.
	// For the ones that didn't trade, drop them and randomize

	int i;
	int tCount = 0; // traded count

	for (i=0; i<sCount && s[i].trades != 0; i++)
		tCount++;

	for (i=0; i<tCount/2; i++)
	{
		Strategy *source = &(s[i]);
		Strategy *dest = &(s[i+(tCount/2)]);
		spawn(source, dest);
	}

	// didn't trade -- randomize
	for (i=tCount; i<sCount; i++)
	{
		randomizeWeight(s[i].buyWeight);
		randomizeWeight(s[i].sellWeight);
	}
}
void printResults(Strategy *s, int sCount, int gIdx)
{
	double median = s[sCount/2].result;
	int medianTrades = s[sCount/2].trades;

	double mean = 0.0;
	int meanTrades = 0;
	int i;
	for (i=0; i<sCount; i++)
	{
		mean += s[i].result;
		meanTrades += s[i].trades;
	}
	mean /= sCount;
	meanTrades /= sCount;
	
	double worst = s[sCount-1].result;
	int worstTrades = s[sCount-1].trades;

	double best = s[0].result;
	int bestTrades = s[0].trades;

	printf("%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d\n",
		gIdx,
		median,
		medianTrades,
		mean,
		meanTrades,
		worst,
		worstTrades,
		best,
		bestTrades);
}
int main()
{	
	int gCount = 40, // generations
		qCount = 2600, // quotes
		sCount = 500; // strategies

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
	}

	printf("Generation,Median,Median Trades,Mean,Mean Trades,Worst,Worst Trades,Best, Best Trades\n");

	for (i=0; i<gCount; i++)
	{
		generation(strategies, sCount, quotes, qCount);
		bubbleSort(strategies, sCount);
		printResults(strategies, sCount, i);
		if (i != gCount-1)
			mutate(strategies, sCount);
	}
	
	// Pick the best strategy and run with it
	double profit = strategies[0].result - STARTING_MONEY;
	double percentProfit = (profit / STARTING_MONEY) * 100;
	printf("Profitability: %lf%%\n", percentProfit);
	
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
