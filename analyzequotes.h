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
	TradeWeight *buyWeight;
	TradeWeight *sellWeight;
	double result;
	uint trades;
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
void generation(Strategy *s, int sCount, Quote *q, int qCount);
void copyBytes(TradeWeight *twSource, TradeWeight *twDest);
void spawn(Strategy *source, Strategy *dest);
void mutate(Strategy *s, int sCount);
void printResults(Strategy *s, int sCount, int gCount);
int main(void);
void normalizeWeight(TradeWeight *w);
