#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <ncurses/ncurses.h>
#include "analyzequotes.h"

Quote * buildQuotes(int count)
{
	int month,day,year;
	double open,high,low,close;
	long volume;
	int i=0;
	Quote *quotes = (Quote *) calloc(count, sizeof(Quote));

	FILE *fp;
	fp = fopen("data.csv", "r");
	
	if (fp == NULL)
	{
		fprintf(stderr, "Can't open input file!\n");
	}
	
	//				m  d  y open high low close
	while (fscanf(fp,
					"%d/%d/%d,%lf,%lf,%lf,%lf,%ld\n",
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
		quotes[i].volume = volume;
		i++;
	}
	
	return quotes;
}
int buy(double price, int shares, Portfolio *portfolio)
{
	assert(shares > 0);
	if (portfolio->money >= (price * shares) + COMMISSION)
	{
		portfolio->shares += shares;
		portfolio->money -= (price * shares) + COMMISSION;
		portfolio->trades += 1;
		return shares;
	}
	return 0;
}
int sell(double price, int shares, Portfolio *portfolio)
{
	assert(shares > 0);
	if (portfolio->shares >= shares)
	{
		portfolio->shares -= shares;
		portfolio->money += (price * shares) - COMMISSION;
		portfolio->trades += 1;
		return shares;
	}
	return 0;
}
double dblRemainder(double a)
{
	double throwaway;
	double retVal = modf(a, &throwaway);
	assert(retVal >= -1.0);
	assert(retVal <= 1.0);
	return retVal;
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
	if (portfolio->money <= COMMISSION)
		return 0;
	
	int shares = (int) ((portfolio->money - COMMISSION) / today.close);
	if (shares <= 0)
		return 0;
		
	if (maybe(yesterday, today, buyWeight))
		return buy(today.close, shares, portfolio);

	return 0;
}
int maybeSell(Quote yesterday, Quote today, TradeWeight *sellWeight, Portfolio *portfolio)
{
	if (portfolio->shares < 1)
		return 0;

	if (maybe(yesterday, today, sellWeight))
		return sell(today.close, portfolio->shares, portfolio);
		
	return 0;
}
TradeWeight * randomWeight()
{
	TradeWeight *weight = malloc(sizeof(TradeWeight));
	randomizeWeight(weight);
	return weight;
}
void randomizeWeight(TradeWeight *w)
{
	assert(sizeof(TradeWeight) % sizeof(uchar) == 0);

	uchar *data = (uchar *) w;

	int i;
	int chars = sizeof(TradeWeight);
	for (i=0; i<chars; i+=sizeof(uchar))
	{
		data[i] = rand() % (UCHAR_MAX + 1);
	}

	normalizeWeight(w);
}
double score(Strategy s)
{
	if (s.portfolio->trades == 0)
		return INT_MIN; // the worst possible strategy is one that didn't trade at all
	return s.result * ((log10(s.portfolio->trades)/10.0)+1);
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
void runStrategy(Strategy *s, Quote *q, int qFirst, int qLast)
{
	// initialize portfolio
	s->portfolio->money = STARTING_MONEY;
	s->portfolio->shares = 0;
	s->portfolio->trades = 0;
	
	// initialize trade history
	initializeTradeHistory(s->trades);
	
	double lastPrice = 0.0;
	int i;
	for (i=qFirst; i<qLast; i++)
	{
		Quote yesterday = q[i-1];
		Quote today = q[i];
	
		int shares = maybeBuy(yesterday, today, s->buyWeight, s->portfolio);
		uchar type = 0;
		if (shares)
			type = BOUGHT;
		else
		{
			shares = maybeSell(yesterday, today, s->sellWeight, s->portfolio);
			if (shares)
				type = SOLD;
		}
		if (type)
		{
			assert(s->portfolio->trades >= 1);
			int idx = s->portfolio->trades - 1;
			TradeRecord *trade = &(s->trades[idx]);
			trade->shares = shares;
			trade->type = type;
			trade->price = today.close;
			trade->month = today.month;
			trade->day = today.day;
			trade->year = today.year;
			trade->money = s->portfolio->money;
		}
		
		lastPrice = today.close;
	}
	s->result = s->portfolio->money + (s->portfolio->shares * lastPrice);
}
void generation(Strategy *s, int sCount, Quote *q, int qCount)
{
	int j;
	for (j=0; j<sCount; j++)
	{
		runStrategy(&(s[j]), q, 1, (qCount-(qCount/5)));
	}
}
void copyBytes(TradeWeight *twSource, TradeWeight *twDest)
{
	assert(sizeof(TradeWeight) % sizeof(uchar) == 0);
	
	uchar *source = (uchar *) twSource;
	uchar *dest = (uchar *) twDest;

	unsigned int i;
	for (i=0; i<sizeof(TradeWeight); i+=sizeof(uchar))
	{
		dest[i] = source[i];
	}
}
Portfolio * initializePortfolio()
{
	Portfolio *p = (Portfolio *) malloc(sizeof(Portfolio));
	p->money = STARTING_MONEY;
	p->shares = 0;
	p->trades = 0;
	return p;
}
void initializeTradeHistory(TradeRecord *trades)
{
	int i;
	for (i=0; i<MAX_TRADES; i++)
	{
		trades[i].type = 0;
		trades[i].month = 0;
		trades[i].day = 0;
		trades[i].year = 0;
		trades[i].price = 0;
		trades[i].shares = 0;
		trades[i].money = 0;
	}
}
void spawn(Strategy *source, Strategy *dest)
{
	// reset the Strategy members
	source->result = 0.0;
	free(source->portfolio);
	source->portfolio = initializePortfolio();
	dest->result = 0.0;
	free(dest->portfolio);
	dest->portfolio = initializePortfolio();
		
	// copy source to dest
	copyBytes(source->buyWeight, dest->buyWeight);
	copyBytes(source->sellWeight, dest->sellWeight);

	// mutate destination
	// pick either buy or sell weight randomly
	double *weight;
	if (rand() % 2)
		weight = (double *) dest->buyWeight;
	else
		weight = (double *) dest->sellWeight;

	// pick a double index randomly
	unsigned int idx = rand() % (sizeof(TradeWeight)/sizeof(double));
	double increment = (((double)(rand() % 1000)) - 500.0) / 1000.0;
	weight[idx] += increment;
	
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

	for (i=0; i<sCount && s[i].portfolio->trades != 0; i++)
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
double percentProfit(Strategy s)
{
	double profit = s.result - STARTING_MONEY;
	return (profit / STARTING_MONEY) * 100;
}
void debugPrintTradeHistory(Strategy s, double shareAmt, int tCount)
{
	clear();
	TradeRecord *trades = s.trades;
	
	mvprintw(0, 0, "DEBUG: starting money is %lf", STARTING_MONEY);
	mvprintw(1, 0, "DEBUG: shareAmt is %lf", shareAmt);
	if (s.portfolio->trades > 0)
	{
		mvprintw(2, 0, "DEBUG: lastTrade is %lf", trades[s.portfolio->trades - 1].money);
		mvprintw(3, 0, "DEBUG: total is %lf", shareAmt + trades[s.portfolio->trades - 1].money + (tCount * COMMISSION));
	}
	
	int i;
	for (i=0; i<MAX_TRADES && trades[i].type; i++)
	{
		TradeRecord trade = trades[i];
		mvprintw(i+4, 0, "DEBUG: %04d/%02d/%02d\t%s\t%lf\t%d\t%lf",
			trade.year, trade.month, trade.day,
			(trade.type & BOUGHT) ? "Buy" : "Sell",
			trade.price,
			trade.shares,
			trade.money);
	}
	
	refresh();
}
void printTwoColumns(int line, int start, int offset, char *text)
{
	mvprintw(line, start, text);
	mvprintw(line, start+offset, text);
}
void printResults(Strategy *s, int sCount, int gIdx, Quote *q, int qCount)
{
	clear();

	double median = s[sCount/2].result;
	int medianTrades = s[sCount/2].portfolio->trades;

	double mean = 0.0;
	int meanTrades = 0;
	int i;
	for (i=0; i<sCount; i++)
	{
		mean += s[i].result;
		meanTrades += s[i].portfolio->trades;
	}
	mean /= sCount;
	meanTrades /= sCount;
	
	double worst = s[sCount-1].result;
	int worstTrades = s[sCount-1].portfolio->trades;

	double best = s[0].result;
	int bestTrades = s[0].portfolio->trades;
	
	mvprintw(0, 0, "Generation:");
	mvprintw(1, 0, "Median:");
	mvprintw(2, 0, "Median Trades:");
	mvprintw(3, 0, "Mean:");
	mvprintw(4, 0, "Mean Trades:");
	mvprintw(5, 0, "Worst:");
	mvprintw(6, 0, "Worst Trades:");
	mvprintw(7, 0, "Best:");
	mvprintw(8, 0, "Best Trades:");
	
	mvprintw(0, 15, "%d", gIdx);
	mvprintw(1, 15, "%lf", median);
	mvprintw(2, 15, "%d", medianTrades);
	mvprintw(3, 15, "%lf", mean);
	mvprintw(4, 15, "%d", meanTrades);
	mvprintw(5, 15, "%lf", worst);
	mvprintw(6, 15, "%d", worstTrades);
	mvprintw(7, 15, "%lf", best);
	mvprintw(8, 15, "%d", bestTrades);
		
	// show profitability
	mvprintw(9, 0, "Profitability: %lf%%\n", percentProfit(s[0]));
	double proof = proofStrategy(s[0], q, qCount);
	mvprintw(10, 0, "Fitness: %lf%%\n", proof);

	
	// show a representation of the trade weight
	mvprintw(11, 11, "buyWeight");
	mvprintw(11, 44, "sellWeight");
	printTwoColumns(12, 0, 44, "y:open");
	printTwoColumns(12, 22, 44, "y:close");
	printTwoColumns(13, 0, 44, "y:high");
	printTwoColumns(13, 22, 44, "y:low");
	printTwoColumns(14, 0, 44, "y:volume");
	printTwoColumns(14, 22, 44, "t:open");
	printTwoColumns(15, 0, 44, "t:close");
	printTwoColumns(15, 22, 44, "t:high");
	printTwoColumns(16, 0, 44, "t:low");
	printTwoColumns(16, 22, 44, "t:volume");
	printTwoColumns(17, 0, 44, "overall");
	TradeWeight *buyWeight = s[0].buyWeight;
	TradeWeight *sellWeight = s[0].sellWeight;
	for (i=0; i<(int)sizeof(TradeWeight)/(int)sizeof(double); i+=2)
	{
		// bounds for the TradeWeight
		assert(i >= 0);
		assert(i <= 10);
		
		int row = 12+(i/2);
		mvprintw(row, 11, "%lf", ((double *)buyWeight)[i]);
		mvprintw(row, 11+44, "%lf", ((double *)sellWeight)[i]);
		
		if (i <10)
		{
			mvprintw(row, 33, "%lf", ((double *)buyWeight)[i+1]);
			mvprintw(row, 33+44, "%lf", ((double *)sellWeight)[i+1]);
		}
	}
	
	printTwoColumns(19, 0, 43, "--- Trade History ---");
	printTwoColumns(20, 0, 43, "YYYY/MM/DD");
	printTwoColumns(20, 11, 43, "Type");
	printTwoColumns(20, 17, 43, "Price");
	printTwoColumns(20, 24, 43, "Shares");
	printTwoColumns(20, 31, 43, "Money");
	int row = 21;
	int offset = 0;
	TradeRecord *trades = s[0].trades;
	for (i=0; i<bestTrades; i++)
	{
		TradeRecord trade = trades[i];
		if (!trade.type)
		{
			refresh();
			fprintf(stderr, "\n\nDEBUG: type is %d\n\n", trade.type);
			fprintf(stderr, "\n\nDEBUG: index is %d\n\n", i);
			assert(trade.type);
		}
		mvprintw(row, 0+offset, "%04d/%02d/%02d",
			trade.year, trade.month, trade.day);
		mvprintw(row, 11+offset, (trade.type & BOUGHT) ? "Buy" : "Sell");
		mvprintw(row, 16+offset, "%7.2lf", trade.price);
		mvprintw(row, 24+offset, "%d", trade.shares);
		mvprintw(row, 31+offset, "%9.2lf", trade.money);
		
		
		if (offset == 0)
		{
			mvprintw(row, 41+offset, "|");
			offset = 43;
		}
		else
		{
			offset = 0;
			row++;
		}
	}
	refresh();
}
double proofStrategy(Strategy s, Quote *q, int qCount)
{
	// Set up a copy with the same weights
	// but a new portfolio and history
	Strategy *copy = (Strategy *) malloc(sizeof(Strategy));
	copy->portfolio = initializePortfolio();
	copy->buyWeight = s.buyWeight;
	copy->sellWeight = s.sellWeight;
	copy->result = 0.0;
	copy->trades = (TradeRecord *) calloc(MAX_TRADES, sizeof(TradeRecord));
	
	// Run the strategy and get the percent profit
	runStrategy(copy, q, (qCount-(qCount/5)), qCount);
	double profit = percentProfit(*copy);
	
	// Teardown copy
	free(copy->portfolio);
	free(copy->trades);
	free(copy);
	
	return profit;
}
int main()
{	
	long gCount = LONG_MAX; // generations
	int	qCount = MAX_QUOTES, // quotes
		sCount = 100; // strategies

	// initialize random number generator
	srand(time(NULL));

	// initialize quotes
	Quote *quotes = buildQuotes(qCount);
	
	// initialize ncurses screen
	initscr();

	// initialize trade weights / strategies
	Strategy *strategies = calloc(sCount, sizeof(Strategy));
	long i;
	for (i=0; i<sCount; i++)
	{
		strategies[i].buyWeight = randomWeight();
		strategies[i].sellWeight = randomWeight();
		strategies[i].result = 0.0;
		strategies[i].portfolio = initializePortfolio();
		strategies[i].trades = calloc(MAX_TRADES, sizeof(TradeRecord));
		initializeTradeHistory(strategies[i].trades);
	}
	
	for (i=0; i<gCount; i++)
	{
		generation(strategies, sCount, quotes, qCount);
		bubbleSort(strategies, sCount);
		printResults(strategies, sCount, i, quotes, qCount);
		
		if (i != gCount-1)
			mutate(strategies, sCount);
	}
	
	// teardown ncurses screen
	getch();
	endwin();
	
	return 0;
}
void normalizeWeight(TradeWeight *w)
{
	assert(sizeof(TradeWeight) % sizeof(double) == 0);
	assert(sizeof(TradeWeight) == sizeof(double) * 11);
	
	double *d = (double *)w;
	int i;
	for (i=0; i<(int)sizeof(TradeWeight)/(int)sizeof(double); i++)
	{
		d[i] = dblRemainder(d[i]);
		assert(d[i] >= -1.0);
		assert(d[i] <= 1.0);
	}
}
