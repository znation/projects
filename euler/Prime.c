#include "Prime.h"
#include "PrecomputedPrimes.h"
#include "Utility.h"

gboolean prime(int x)
{
    gboolean ret;
    if (x < MAX_PRIME)
    {
        ret = computedPrimes[x];
    }
    else
    {
        ret = uncachedPrime(x);
    }

    //printf("Prime %d? %s\n", x, ret ? "true" : "false");

    return ret;
}

GList *listOfPrimes()
{
    GList *ret = NULL;
    int i;

    for (i=2; i<MAX_PRIME; i++)
    {
        if (prime(i))
        {
            ret = g_list_prepend(ret, GINT_TO_POINTER(i));
        }
    }
    ret = g_list_reverse(ret);
    return ret;
}

GList *factors(int x)
{
    GList *ret = NULL;
    int n, d, rem, result;

    if (x < 2)
    {
        ret = g_list_prepend(ret, GINT_TO_POINTER(x));
        return ret;
    }

    n = x;

    for (d=2; d<=n; d++)
    {
        rem = n % d;
        if (rem == 0)
        {
            // found a factor
            if (prime(d))
            {
                ret = g_list_prepend(ret, GINT_TO_POINTER(d));
            }

            result = n / d;
            n = result;
            d = 2;
        }
    }

    ret = g_list_remove_duplicates(ret);
    return ret;
}

