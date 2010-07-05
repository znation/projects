typedef unsigned char uchar;

typedef struct Portfolio
{
	int shares;
	double money;
	int trades;
	double commission;
} Portfolio;

typedef struct Quote
{
	int month,day,year;
	double open,
		close,
		high,
		low;
	long volume;
} Quote;

typedef struct TradeWeight
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

#define BOUGHT 0x01
#define SOLD 0x02
typedef struct TradeRecord
{
	uchar type; // BOUGHT or SOLD
	int month,day,year;
	double price;
	int shares;
	double money;
	struct TradeRecord *next;
} TradeRecord;

typedef struct Strategy
{
	TradeWeight *buyWeight;
	TradeWeight *sellWeight;
	double result;
	TradeRecord *firstTrade;
	TradeRecord *lastTrade;
	Portfolio *portfolio;
} Strategy;

Quote * buildQuotes(int count);
int buy(double price, int shares, Portfolio *portfolio);
int sell(double price, int shares, Portfolio *portfolio);
double dblRemainder(double a);
int maybe(Quote yesterday, Quote today, TradeWeight *weight);
int maybeBuy(Quote yesterday, Quote today, TradeWeight *buyWeight, Portfolio *portfolio);
int maybeSell(Quote yesterday, Quote today, TradeWeight *sellWeight, Portfolio *portfolio);
TradeWeight * randomWeight(void);
void randomizeWeight(TradeWeight *w);
int score(Strategy s);
void bubbleSort(Strategy *s, int length);
void runStrategy(Strategy *s, Quote *q, int qFirst, int qLast);
void generation(Strategy *s, int sCount, Quote *q, int qCount);
void copyBytes(TradeWeight *twSource, TradeWeight *twDest);
Portfolio * initializePortfolio(void);
void spawn(Strategy *source, Strategy *dest);
void mutate(Strategy *s, int sCount);
double percentProfit(Strategy s);
void debugPrintTradeHistory(Strategy s, double shareAmt, int tCount);
int countTrades(Strategy s);
void printResults(Strategy *s, int sCount, int gCount, Quote *q, int qCount);
double proofStrategy(Strategy s, Quote *q, int qCount);
void freeTradeHistory(TradeRecord *rec);
int main(void);
void normalizeWeight(TradeWeight *w);
