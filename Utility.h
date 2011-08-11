#include <stdbool.h>
#include <glib.h>

#define MAX_PENTAGONAL 10000
#define MAX_GCD 10000

int max(int x, int y);
GList *digits(gint64 x);
gint64 undigits(GList *digits);
gint64 ipow(gint64 x, gint64 y);
int isqrt(int x);
bool prime(int x);
GList *listOfPrimes();
bool pandigital9(int x);
bool isPermutation(gint64 x, gint64 y);
bool numberInList(int x, GList *list);

GList *listOfPentagonals();
int pentagonal(int x);

GList *factors(int x);
gint64 totient(gint64 x);
gint64 gcd(gint64 a, gint64 b);

GList *g_list_remove_duplicates(GList *l);

