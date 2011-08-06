#include "assert.h"
#include "stdio.h"
#include "string.h"
#include "math.h"
#include "stdlib.h"

#include "Utility.h"
#include "DynArray.h"

int digits(int x, int *buf)
{
    char temp[10];
    sprintf(temp, "%d", x);
    int len = strlen(temp);
    for (int i=0; i<len; i++)
    {
        buf[i] = (int)temp[i] - 48;
    }
    return len; 
}

int undigits(int *buf, int len)
{
    int ret = 0;
    for (int i=0; i<len; i++)
    {
        ret += ipow(10, len-i-1) * buf[i];
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

DynArray *primes = NULL;
bool prime(int x)
{
    if (primes == NULL)
    {
        primes = DynArray_new();
    }

    bool ret;
    if (DynArray_contains(primes, x))
    {
        ret = DynArray_get(primes, x);
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

        DynArray_set(primes, x, ret);
    }
    
    //printf("Prime %d? %s\n", x, ret ? "true" : "false");

    return ret;
}

