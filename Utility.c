#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <glib.h>

#include "Utility.h"

#ifdef GCC
int max(int x, int y)
{
    return (x > y) ? x : y;
}
#endif

BoundedArrayInt32 digits(gint64 x)
{
#define TEMP_LIMIT 15
    int temp[TEMP_LIMIT];
    int length = 0,
        i;
    BoundedArrayInt32 ret;
 
    while (TRUE)
    {
        int d = (int)(x % 10);
        temp[length++] = d;
        assert(length < TEMP_LIMIT);
        if (x < 10)
        {
            break;
        }
        else
        {
            x = x / 10;
        }
    }

    ret = BoundedArrayInt32_new(length);
    for (i=0; i<length; i++)
    {
        ret.array[length-i-1] = temp[i];
    }

    return ret;
}

gint64 undigits(BoundedArrayInt32 digits)
{
    #define MAX_IPOW 20
    static BoundedArrayInt64 ipow_cache;
    static BoundedArrayBool ipow_cache_set;
    gint64 ret, mult, data, exp;
    guint len;
    gint i;

    if (ipow_cache.length == 0)
    {
        ipow_cache = BoundedArrayInt64_new(MAX_IPOW);
        ipow_cache_set = BoundedArrayBool_new(MAX_IPOW);
    }

    assert(digits.length > 0);

    ret = 0;
    len = digits.length;

    for (i=0; i<len; i++)
    {
        data = digits.array[i];
        exp = len-i-1;
        assert(exp < MAX_IPOW);
        
        if (ipow_cache_set.array[exp])
        {
            mult = ipow_cache.array[exp];
        }
        else
        {
            mult = ipow(10, exp);
            ipow_cache.array[exp] = mult;
            ipow_cache_set.array[exp] = TRUE;
        }

        ret += mult * data;
    }

    return ret;
}

int isqrt(int x)
{
    return (int)sqrt((double)x);
}

gboolean uncachedPrime(int x)
{
    gboolean ret;
    int i;

    if (x == 2 || x == 3)
    {
        ret = TRUE;
    }
    else if ((x < 2) ||
            (x % 2 == 0))
    {
        ret = FALSE;
    }
    else
    {
        ret = TRUE;
        for (i=2; i<=isqrt(x); i++)
        {
            int rem = x % i;
            if (rem == 0)
            {
                ret = FALSE;
                break;
            }
        }
    }
    return ret;
}

gint64 ipow(gint64 base, gint64 exp)
{
    gint64 result = 1;
    while (exp)
    {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        base *= base;
    }

    return result;    
}

gboolean pandigital9(int x)
{
    gboolean ret;
    int data, i;
    GList *unique = NULL;
    BoundedArrayInt32 ds;
   
    ds = digits(x);

    for (i=0; i<ds.length; i++)
    {
        data = ds.array[i];
        if (!g_list_find(unique, GINT_TO_POINTER(data)))
        {
            if (data == 0)
            {
                ret = FALSE;
                goto CLEANUP;
            }

            unique = g_list_prepend(unique, GINT_TO_POINTER(data));
        }
    }

    ret = g_list_length(unique) == 9;

CLEANUP:
    g_list_free(unique);

    return ret;
}

gboolean isPermutation(gint64 x, gint64 y)
{
    BoundedArrayInt32 xs;
    BoundedArrayInt32 ys;
    gboolean ret;
    GList *ds = NULL;
    int i, xsl, ysl, elem;

    xs = digits(x);
    ys = digits(y);

    xsl = xs.length;
    ysl = ys.length;
    if (xsl != ysl)
    {
        ret = FALSE;
        goto CLEANUP;
    }

    ds = BoundedArrayInt32_toGList(xs);

    for (i=0; i<ysl; i++)
    {
        elem = ys.array[i];
        if (!g_list_find(ds, GINT_TO_POINTER(elem)))
        {
            ret = FALSE;
            goto CLEANUP;
        }
        else
        {
            ds = g_list_remove(ds, GINT_TO_POINTER(elem));
        }
    }

    ret = TRUE;

CLEANUP:
    g_list_free(ds);
    return ret;
}

gboolean numberInList(int x, GList *list)
{
    return (gboolean)g_list_find(list, GINT_TO_POINTER(x));
}

GList * listOfPentagonals()
{
    GList *list = NULL;
    int i, p2;
    gint64 p;

    for (i=1; i<MAX_PENTAGONAL; i++)
    {
        p = (i * ((3 * i) - 1)) / 2;
        assert(p < INT_MAX);
        p2 = (int)p;
        list = g_list_prepend(list, GINT_TO_POINTER(p2));
    }

    list = g_list_reverse(list);
    return list;
}

int pentagonal(int x)
{
    double n = (sqrt((24.0 * (double)x)+1.0)+1.0) / 6.0;
    if (fmod(n, 1.0) == 0.0)
    {
        return floor(n);
    }
    else
    {
        return -1;
    }
}

gint64 totient(gint64 x)
{
    gint64 ret = 0,
           i;

    for (i=1; i<x; i++)
    {
        if (gcd(x, i) == 1)
        {
            ret++;
        }
    }
    return ret;
}

gint64 gcd(gint64 a, gint64 b)
{
    while (b != 0)
    {
        gint64 temp = b;
        b = a % temp;
        a = temp;
    }

    return a;
}

gint intCompare(gconstpointer a, gconstpointer b)
{
    gint ai = GPOINTER_TO_INT(a);
    gint bi = GPOINTER_TO_INT(b);
    return ai - bi;
}

GList *g_list_remove_duplicates(GList *l)
{
    GList *currElem, *nextElem;
    l = g_list_sort(l, intCompare);

    currElem = l;
    while (currElem != NULL)
    {
        nextElem = g_list_next(currElem);
        if (nextElem == NULL)
        {
            break;
        }

        if (currElem->data == nextElem->data)
        {
            currElem = g_list_remove_link(currElem, nextElem);
            g_list_free(nextElem);
        }

        currElem = g_list_next(currElem);
    }

    return l;
}

BoundedArrayInt64 integer_permutations(gint64 x)
{
    return integer_permutations_condition(x, NULL, NULL);
}

BoundedArrayInt64 integer_permutations_condition(gint64 x, BoundedArrayInt64 *range, gboolean *condition(gint64, BoundedArrayInt64))
{
#define MAX_PERMUTATIONS 10000000
    static gint64 rettemp[MAX_PERMUTATIONS];
    int count = 0;
    GList *ds, *elem;
    BoundedArrayInt32 dsa, rgds, lengthCheckDs;
    int n, d, i;
    gint64 p;
    BoundedArrayInt64 ret;

    dsa = digits(x);
    ds = BoundedArrayInt32_toGList(dsa);
    BoundedArrayInt32_free(dsa);
    ds = g_list_sort(ds, intCompare);
    n = g_list_length(ds);

    // put ds into an array
    rgds = BoundedArrayInt32_new(n);
    elem = ds;
    for (i=0; i<n; i++)
    {
        d = GPOINTER_TO_INT(elem->data);
        rgds.array[i] = d;
        elem = g_list_next(elem);
    }
    g_list_free(ds);

    p = undigits(rgds);
    lengthCheckDs = digits(p);
    if (lengthCheckDs.length == n)
    {
        if (condition == NULL || (*condition)(p, *range))
        {
            assert(count < MAX_PERMUTATIONS);
            rettemp[count++] = p;
        }
    }
    BoundedArrayInt32_free(lengthCheckDs);

    while (TRUE)
    {
        int i,j;
        gint64 temp;

        i = n - 1;
        while (rgds.array[i-1] >= rgds.array[i])
        {
            i--;
        }

        j = n;
        while (rgds.array[j-1] <= rgds.array[i-1])
        {
            j--;
        }

        if (i-1 < 0)
        {
            // Found the last permutation
            break;
        }

        assert(i-1 < n);
        assert(j-1 < n && j-1 >= 0);
        temp = rgds.array[i-1];
        rgds.array[i-1] = rgds.array[j-1];
        rgds.array[j-1] = temp;

        i++;
        j = n;
        while (i < j)
        {
            assert(i-1 < n && i-1 >= 0);
            assert(j-1 < n && j-1 >= 0);

            temp = rgds.array[i-1];
            rgds.array[i-1] = rgds.array[j-1];
            rgds.array[j-1] = temp;

            i++;
            j--;
        }

        p = undigits(rgds);

        lengthCheckDs = digits(p);
        if (lengthCheckDs.length == n)
        {
            if (condition == NULL || (*condition)(p, *range))
            {
                assert(count < MAX_PERMUTATIONS);
                rettemp[count++] = p;
            }
        }
        BoundedArrayInt32_free(lengthCheckDs);
    }

    ret = BoundedArrayInt64_new(count);
    for (i=0; i<count; i++)
    {
        ret.array[i] = rettemp[count-i-1];
    }
    return ret;
}

int binary_search(gint64 x, BoundedArrayInt64 sortedArray)
{
    int length = sortedArray.length;
    int high = length-1,
        low = 0;

    int idx = -1;
    while (idx == -1)
    {
        int i;

        if (high - low < 10)
        {
            break;
        }
        i = ((high - low)/2) + low;
        if (sortedArray.array[i] > x)
        {
            // go down
            high = i;
        }
        else if (sortedArray.array[i] < x)
        {
            // go up
            low = i;
        }
        else
        {
            idx = i;
        }
    }
    if (idx == -1)
    {
        int i;
        for (i=low; i<=high; i++)
        {
            if (sortedArray.array[i] == x)
            {
                idx = i;
            }
        }
    }

    return idx;
}

