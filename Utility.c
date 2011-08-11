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

GList * digits(gint64 x)
{
    GList *ret = NULL;

    while (x >= 10)
    {
        int d = (int)(x % 10);
        ret = g_list_prepend(ret, GINT_TO_POINTER(d));
        x = x / 10;
    }
    ret = g_list_prepend(ret, GINT_TO_POINTER((int)x));
    ret = g_list_reverse(ret);

    return ret;
}

gint64 undigits(GList *digits)
{
    GList *elem = digits;
    if (g_list_length(elem) == 0)
    {
        return -1;
    }

    gint64 ret = 0;
    int i = 0;
    uint len = g_list_length(digits);

    while (g_list_length(elem) > 0)
    {
        int data = GPOINTER_TO_INT(elem->data);
        ret += ipow((gint64)10, (gint64)(len-i-1)) * (gint64)data;
        elem = g_list_next(elem);
        i++;
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
    if (primes->len <= x)
    {
        assert(primesSet->len == primes->len);
        int len = primes->len;
        int newlen = max(len * 2, 10);
        //printf("Setting primes length to %d\n", newlen);
        g_array_set_size(primes, newlen);
        g_array_set_size(primesSet, newlen);
    }

    //printf("Looking up %d in the primesSet array\n", x);
    if (((bool*)(primesSet->data))[x])
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
        ((bool*)(primesSet->data))[x] = true;
        ((bool*)(primes->data))[x] = ret;
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
    GList *ds = digits(x);
    GList *unique = NULL;
    GList *elem = ds;

    while (elem != NULL)
    {
        if (!g_list_find(unique, elem->data))
        {
            int data = GPOINTER_TO_INT(elem->data);
            if (data == 0)
            {
                ret = false;
                goto CLEANUP;
            }

            unique = g_list_prepend(unique, elem->data);
        }
        elem = g_list_next(elem);
    }

    ret = g_list_length(unique) == 9;

CLEANUP:
    g_list_free(unique);

    return ret;
}

bool isPermutation(gint64 x, gint64 y)
{
    GList *xs = digits(x);
    GList *ys = digits(y);
    bool ret;

    int xsl = g_list_length(xs);
    int ysl = g_list_length(ys);
    if (xsl != ysl)
    {
        ret = false;
        goto CLEANUP;
    }

    GList *ds = g_list_copy(xs);

    GList *elemys = ys;
    for (int i=0; i<ysl; i++)
    {
        int elem = GPOINTER_TO_INT(elemys->data);
        if (!g_list_find(ds, GINT_TO_POINTER(elem)))
        {
            ret = false;
            goto CLEANUP;
        }
        else
        {
            ds = g_list_remove(ds, GINT_TO_POINTER(elem));
        }

        elemys = g_list_next(elemys);
    }

    ret = true;

CLEANUP:
    g_list_free(xs);
    g_list_free(ys);
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

GList *g_list_remove_duplicates(GList *l)
{
    gint intSort(gconstpointer a, gconstpointer b)
    {
        gint ai = GPOINTER_TO_INT(a);
        gint bi = GPOINTER_TO_INT(b);
        return ai - bi;
    }

    l = g_list_sort(l, intSort);

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

