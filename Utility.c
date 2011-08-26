#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <glib.h>

#include "Utility.h"

int max(int x, int y)
{
    return (x > y) ? x : y;
}

BoundedArrayInt32 digits(gint64 x)
{
    const int tempLimit = 15;
    int temp[tempLimit];
    int length = 0;
    
    while (true)
    {
        int d = (int)(x % 10);
        temp[length++] = d;
        assert(length < tempLimit);
        if (x < 10)
        {
            break;
        }
        else
        {
            x = x / 10;
        }
    }

    BoundedArrayInt32 ret = BoundedArrayInt32_new(length);
    for (int i=0; i<length; i++)
    {
        ret.array[length-i-1] = temp[i];
    }

    return ret;
}

gint64 undigits(BoundedArrayInt32 digits)
{
    assert(digits.length > 0); 

    gint64 ret = 0;
    uint len = digits.length;

    for (int i=0; i<len; i++)
    {
        int data = digits.array[i];
        ret += ipow((gint64)10, (gint64)(len-i-1)) * (gint64)data;
    }

    return ret;
}

int isqrt(int x)
{
    return (int)sqrt((double)x);
}

gint64 ipow(gint64 x, gint64 y)
{
    gint64 ret = 1;
    for (int i=0; i<y; i++)
    {
        ret *= x;
    }
    return ret;
}

GArray *primes = NULL;
GArray *primesSet = NULL;
bool prime(int x)
{
    if (primes == NULL)
    {
        primes = g_array_new(false, true, sizeof(bool));
        primesSet = g_array_new(false, true, sizeof(bool));
    }

    bool ret;
    if (x < MAX_PRIMES && primes->len <= x)
    {
        assert(primesSet->len == primes->len);
        int len = primes->len;
        int newlen = max(len * 2, 10);
        //printf("Setting primes length to %d\n", newlen);
        g_array_set_size(primes, newlen);
        g_array_set_size(primesSet, newlen);
    }

    //printf("Looking up %d in the primesSet array\n", x);
    if (x < MAX_PRIMES && ((bool*)(primesSet->data))[x])
    {
        ret = (((bool*)(primes->data))[x]);
    }
    else
    {
        if (x == 2 || x == 3)
        {
            ret = true;
        }
        else if ((x < 2) ||
                (x % 2 == 0))
        {
            ret = false;
        }
        else
        {
            ret = true;
            for (int i=2; i<=isqrt(x); i++)
            {
                int rem = x % i;
                if (rem == 0)
                {
                    ret = false;
                    break;
                }
            }
        }

        if (x < MAX_PRIMES)
        {
            ((bool*)(primesSet->data))[x] = true;
            ((bool*)(primes->data))[x] = ret;
        }
    }

    //printf("Prime %d? %s\n", x, ret ? "true" : "false");

    return ret;
}

GList *listOfPrimes()
{
    GList *ret = NULL;
    for (int i=2; i<1000000; i++)
    {
        if (prime(i))
        {
            ret = g_list_prepend(ret, GINT_TO_POINTER(i));
        }
    }
    ret = g_list_reverse(ret);
    return ret;
}

bool pandigital9(int x)
{
    bool ret;
    BoundedArrayInt32 ds = digits(x);
    GList *unique = NULL;

    for (int i=0; i<ds.length; i++)
    {
        int data = ds.array[i];
        if (!g_list_find(unique, GINT_TO_POINTER(data)))
        {
            if (data == 0)
            {
                ret = false;
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

bool isPermutation(gint64 x, gint64 y)
{
    BoundedArrayInt32 xs = digits(x);
    BoundedArrayInt32 ys = digits(y);
    bool ret;

    int xsl = xs.length;
    int ysl = ys.length;
    if (xsl != ysl)
    {
        ret = false;
        goto CLEANUP;
    }

    GList *ds = BoundedArrayInt32_toGList(xs);

    for (int i=0; i<ysl; i++)
    {
        int elem = ys.array[i];
        if (!g_list_find(ds, GINT_TO_POINTER(elem)))
        {
            ret = false;
            goto CLEANUP;
        }
        else
        {
            ds = g_list_remove(ds, GINT_TO_POINTER(elem));
        }
    }

    ret = true;

CLEANUP:
    g_list_free(ds);
    return ret;
}

bool numberInList(int x, GList *list)
{
    return (bool)g_list_find(list, GINT_TO_POINTER(x));
}

GList * listOfPentagonals()
{
    GList *list = NULL;

    for (int i=1; i<MAX_PENTAGONAL; i++)
    {
        gint64 p = (i * ((3 * i) - 1)) / 2;
        assert(p < INT_MAX);
        int p2 = (int)p;
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

GList *factors(int x)
{
    GList *ret = NULL;
    if (x < 2)
    {
        ret = g_list_prepend(ret, GINT_TO_POINTER(x));
        return ret;
    }

    int n = x;

    for (int d=2; d<=n; d++)
    {
        int rem = n % d;
        if (rem == 0)
        {
            // found a factor
            if (prime(d))
            {
                ret = g_list_prepend(ret, GINT_TO_POINTER(d));
            }

            int result = n / d;
            n = result;
            d = 2;
        }
    }

    ret = g_list_remove_duplicates(ret);
    return ret;
}

gint64 totient(gint64 x)
{
    gint64 ret = 0;
    for (gint64 i=1; i<x; i++)
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
    l = g_list_sort(l, intCompare);

    GList *currElem = l;
    while (currElem != NULL)
    {
        GList *nextElem = g_list_next(currElem);
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
#define MAX_PERMUTATIONS 1000000
    static gint64 rettemp[MAX_PERMUTATIONS];
    int count = 0;

    BoundedArrayInt32 dsa = digits(x);
    GList *ds = BoundedArrayInt32_toGList(dsa);
    BoundedArrayInt32_free(dsa);
    ds = g_list_sort(ds, intCompare);
    int n = g_list_length(ds);

    // put ds into an array
    BoundedArrayInt32 rgds = BoundedArrayInt32_new(n);
    GList *elem = ds;
    for (int i=0; i<n; i++)
    {
        gint d = GPOINTER_TO_INT(elem->data);
        rgds.array[i] = d;
        elem = g_list_next(elem);
    }
    g_list_free(ds);

    gint64 p = undigits(rgds);
    BoundedArrayInt32 lengthCheckDs = digits(p);
    if (lengthCheckDs.length == n)
    {
        assert(count < MAX_PERMUTATIONS);
        rettemp[count++] = p;
    }
    BoundedArrayInt32_free(lengthCheckDs);

    while (true)
    {
        int i = n - 1;
        while (rgds.array[i-1] >= rgds.array[i])
        {
            i--;
        }

        int j = n;
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
        gint64 temp = rgds.array[i-1];
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
            assert(count < MAX_PERMUTATIONS);
            rettemp[count++] = p;
        }
        BoundedArrayInt32_free(lengthCheckDs);
    }

    BoundedArrayInt64 ret = BoundedArrayInt64_new(count);
    for (int i=0; i<count; i++)
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
        if (high - low < 10)
        {
            break;
        }
        int i = ((high - low)/2) + low;
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
        for (int i=low; i<=high; i++)
        {
            if (sortedArray.array[i] == x)
            {
                idx = i;
            }
        }
    }

    return idx;
}

