#include <stdbool.h>
#include <glib.h>

int max(int x, int y);
GList *digits(gint64 x);
gint64 undigits(GList *digits);
gint64 ipow(gint64 x, gint64 y);
int isqrt(int x);
bool prime(int x);
GList *listOfPrimes();
bool pandigital9(int x);
bool isPermutation(gint64 x, gint64 y);

