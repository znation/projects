#include <stdlib.h>
#include <glib.h>

#include "BoundedArray.h"

#define MAX_PENTAGONAL 10000

#ifdef GCC
gint max(gint, gint);
#endif

BoundedArrayInt32 digits(gint64 x);
gint64 undigits(BoundedArrayInt32 digits);
gint64 ipow(gint64 x, gint64 y);
gboolean uncachedPrime(gint x);
gboolean pandigital9(gint x);
gboolean isPermutation(gint64 x, gint64 y);
gboolean numberInList(gint x, GList *list);

GList *listOfPentagonals(void);
gint pentagonal(gint x);

gint64 totient(gint64 x);
gint64 gcd(gint64 a, gint64 b);

GList *g_list_remove_duplicates(GList *l);
BoundedArrayInt64 integer_permutations(gint64 x); 
BoundedArrayInt64 integer_permutations_condition(gint64 x, BoundedArrayInt64 *range, gboolean *condition(gint64, BoundedArrayInt64));

gint binary_search(gint64 x, BoundedArrayInt64 sortedArray);


