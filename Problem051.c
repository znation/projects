#include <stdio.h>
#include <assert.h>
#include <glib.h>
#include "Utility.h"
#include "Prime.h"

BoundedArrayInt32 substitute(int p, BoundedArrayInt32 r)
{
    //printf("DEBUG: testing prime %d\n", p);
    int i, j,
        count = 0;
    gint64 rettemp[10];
    BoundedArrayInt32 ret;

    for (i=0; i<=9; i++)
    {
        gint64 pp;
        BoundedArrayInt32 ds = digits(p);
        assert(r.length < ds.length);
        for (j=0; j<r.length; j++)
        {
            int k = r.array[j];
            ds.array[k] = i;
        }
        pp = undigits(ds);
        BoundedArrayInt32_free(ds);
        if (prime(pp))
        {
            // digit count must match
            BoundedArrayInt32 ppds = digits(pp);
            if (ppds.length == ds.length)
            {
                //printf("\tDEBUG: found family prime %lld\n", pp);
                rettemp[count++] = pp;
            }
            BoundedArrayInt32_free(ppds);
        }
    }

    ret = BoundedArrayInt32_new(count);
    for (i=0; i<count; i++)
    {
        ret.array[i] = rettemp[i];
    }
    return ret;
}

int substituteAll(int prime, int numSubstitutions)
{
    int k, ret = 0;
    gint64 r;
    BoundedArrayInt32 s;
    BoundedArrayInt64 ps;
    BoundedArrayInt32 range = BoundedArrayInt32_new(numSubstitutions);
    for (k=0; k<numSubstitutions; k++)
    {
        range.array[k] = k;
    }
    r = undigits(range);
    BoundedArrayInt32_free(range);

    ps = integer_permutations(r);
    for (k=0; k<ps.length; k++)
    {
        BoundedArrayInt32 pm = digits(ps.array[k]);
        s = substitute(prime, pm);
        if (s.length > ret)
        {
            int i;

            ret = s.length;
            printf("Found new family of length %d:\n", ret);
            for (i=0; i<ret; i++)
            {
                printf("\t%d\n", s.array[i]);
            }
        }
        BoundedArrayInt32_free(s);
        BoundedArrayInt32_free(pm);
    }

    BoundedArrayInt64_free(ps);

    return ret;
}

gint64 answer()
{
    int ret = 0;
/*
    GList *primes = listOfPrimes();
    GList *elem = primes;
    while (elem != NULL)
    {
        int prime,
            j;
        BoundedArrayInt32 ds;
        prime = GPOINTER_TO_INT(elem->data);

        ds = digits(prime);

        assert(ds.length < 10);
        for (j=1; j<ds.length; j++)
        {
            ret = max(ret, substituteAll(prime, j));
        }

        elem = g_list_next(elem);
    }
    */

    // DEBUG
    substituteAll(56003, 2);

    return ret;
}

