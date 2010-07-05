#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <ncurses/ncurses.h>
#include "analyzequotes.h"

#define STARTING_MONEY 100000.0

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
	if (portfolio->money >= (price * shares) + portfolio->commission)
	{
		portfolio->shares += shares;
		portfolio->money -= (price * shares) + portfolio->commission;
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
		portfolio->money += (price * shares) - portfolio->commission;
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
	if (portfolio->money <= 8.00)
		return 0;
	
	int shares = (portfolio->money - 8.00) / today.close;
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
int score(Strategy s)
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
	s->portfolio->commission = 8.00;
	s->portfolio->trades = 0;
	
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
			TradeRecord *trade = (TradeRecord *) malloc(sizeof(TradeRecord));
			
			trade->shares = shares;
			trade->type = type;
			trade->price = today.close;
			trade->month = today.month;
			trade->day = today.day;
			trade->year = today.year;
			trade->money = s->portfolio->money;
			trade->prev = NULL;
			trade->next = NULL;
			
			if (s->lastTrade == NULL)
			{
				assert(s->firstTrade == NULL);
				s->firstTrade = trade;
				s->lastTrade = trade;
			}
			else
			{
				assert(s->firstTrade != NULL);
				assert(s->lastTrade != NULL);
				s->lastTrade->next = trade;
				trade->prev = s->lastTrade;
				s->lastTrade = trade;
			}
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
	
	return p;
}
void spawn(Strategy *source, Strategy *dest)
{
	// reset the Strategy members
	source->result = 0.0;
	freeTradeHistory(source->firstTrade);
	source->firstTrade = NULL;
	source->lastTrade = NULL;
	free(source->portfolio);
	source->portfolio = initializePortfolio();
	dest->result = 0.0;
	dest->firstTrade = NULL;
	dest->lastTrade = NULL;
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
	TradeRecord *trade = s.firstTrade;
		
	fprintf(stderr, "DEBUG: starting money is %lf\n", STARTING_MONEY);
	fprintf(stderr, "DEBUG: shareAmt is %lf\n", shareAmt);
	fprintf(stderr, "DEBUG: lastTrade is %lf\n", s.lastTrade->money);
	fprintf(stderr, "DEBUG: total is %lf\n", shareAmt + s.lastTrade->money + (tCount * 8));
	
	while (trade != NULL)
	{
		fprintf(stderr, "DEBUG: %04d/%02d/%02d\t",
			trade->year, trade->month, trade->day);
		fprintf(stderr, (trade->type & BOUGHT) ? "Buy" : "Sell");
		fprintf(stderr, "\t%lf\t", trade->price);
		fprintf(stderr, "%d\t", trade->shares);
		fprintf(stderr, "%lf\n", trade->money);
		assert(trade->next != trade);
		trade = trade->next;
	}
}
int countTrades(Strategy s)
{
	if (s.firstTrade == NULL)
		return 0;
	
	int i;
	int buyTrades = 0;
	int sellTrades = 0;
	double shareAmt = 0;
	TradeRecord *trade = s.firstTrade;
	for (i=0; trade != NULL; i++)
	{
		if (trade->type == BOUGHT)
		{
			buyTrades++;
			shareAmt += (trade->shares * trade->price);
		}
		else if (trade->type == SOLD)
		{
			sellTrades++;
			shareAmt -= (trade->shares * trade->price);
			if (shareAmt >= 0)
			{
				debugPrintTradeHistory(s, shareAmt, i);
				assert(shareAmt >= 0);
			}
		}
		else
			assert(FALSE);
		
		assert(trade->next != trade);
		trade = trade->next;
	}
	
	double threshold = 0.00000001;
	double total = shareAmt + s.lastTrade->money + (i * 8);
	if (total - threshold > STARTING_MONEY
		|| total + threshold < STARTING_MONEY)
	{
		debugPrintTradeHistory(s, shareAmt, i);
		assert(total == STARTING_MONEY);
	}
	
	return i;
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

	assert(countTrades(s[0]) == s[0].portfolio->trades);
	
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
	mvprintw(11, 20, "buyWeight");
	mvprintw(11, 50, "sellWeight");
	mvprintw(12, 0, "y:");
	mvprintw(12, 4, "open");
	mvprintw(13, 4, "close");
	mvprintw(14, 4, "high");
	mvprintw(15, 4, "low");
	mvprintw(16, 4, "volume");
	mvprintw(17, 0, "t:");
	mvprintw(17, 4, "open");
	mvprintw(18, 4, "close");
	mvprintw(19, 4, "high");
	mvprintw(20, 4, "low");
	mvprintw(21, 4, "volume");
	mvprintw(22, 0, "overall");
	TradeWeight *buyWeight = s[0].buyWeight;
	TradeWeight *sellWeight = s[0].sellWeight;
	for (i=0; i<(int)sizeof(TradeWeight)/(int)sizeof(double); i++)
	{
		mvprintw(12+i, 20, "%lf", ((double *)buyWeight)[i]);
		mvprintw(12+i, 50, "%lf", ((double *)sellWeight)[i]);
		
		// bounds for the TradeWeight
		assert(i >= 0);
		assert(i <= 10);
	}
	
	mvprintw(24, 0, "--- Trade History ---");
	mvprintw(25, 0, "YYYY/MM/DD");
	mvprintw(25, 12, "Type");
	mvprintw(25, 17, "Price");
	mvprintw(25, 40, "Shares");
	mvprintw(25, 47, "Money");
	int row = 26;
	TradeRecord *trade = s[0].firstTrade;
	while (trade != NULL)
	{
		mvprintw(row, 0, "%04d/%02d/%02d",
			trade->year, trade->month, trade->day);
		mvprintw(row, 12, (trade->type & BOUGHT) ? "Buy" : "Sell");
		mvprintw(row, 17, "%lf", trade->price);
		mvprintw(row, 40, "%d", trade->shares);
		mvprintw(row, 47, "%lf", trade->money);
		row++;
		assert(trade->next != trade);
		trade = trade->next;
	}
	refresh();
}
double proofStrategy(Strategy s, Quote *q, int qCount)
{
	runStrategy(&s, q, (qCount-(qCount/5)), qCount);
	return percentProfit(s);
}
void freeTradeHistory(TradeRecord *rec)
{
	if (rec == NULL)
		return;
	
	if (rec->next != NULL)
	{
		freeTradeHistory(rec->next);
	}
	
	free(rec);
}
int main()
{	
	long gCount = LONG_MAX; // generations
	int	qCount = 2858, // quotes
		sCount = 20; // strategies

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
		strategies[i].firstTrade = NULL;
		strategies[i].lastTrade = NULL;
	}
	
	for (i=0; i<gCount; i++)
	{
		generation(strategies, sCount, quotes, qCount);
		bubbleSort(strategies, sCount);
		printResults(strategies, sCount, i, quotes, qCount);
		
		int j;
		for (j=0; j<sCount; j++)
		{
			freeTradeHistory(strategies[j].firstTrade);
			strategies[j].firstTrade = NULL;
			strategies[j].lastTrade = NULL;
		}
		
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
