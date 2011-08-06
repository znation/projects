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

GList * digits(int x)
{
    char temp[16];
    sprintf(temp, "%d", x);
    int len = strlen(temp);
    GList *ret = NULL;
    for (int i=0; i<len; i++)
    {
        int d = (int)temp[i] - 48;
        ret = g_list_append(ret, GINT_TO_POINTER(d));
    }
    return ret;
}

int undigits(GList *digits)
{
    GList *elem = digits;
    if (elem == NULL)
    {
        return -1;
    }

    int ret = 0,
        i = 0;
    uint len = g_list_length(digits);

    while (elem != NULL);
    {
        int data = GPOINTER_TO_INT(elem->data);
        ret += ipow(10, len-i-1) * data;
        elem = g_list_next(elem);
        i++;
    }
    return ret;
}

int isqrt(int x)
{
    return (int)sqrt((double)x);
}

int ipow(int x, int y)
{
    return (int)pow((double)x, (double)y);
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

