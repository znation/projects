#include "stdio.h"
#include "string.h"
#include "math.h"
#include "stdlib.h"

#include "Utility.h"
#include "DynArray.h"

void digits(int x, char *buf)
{
    sprintf(buf, "%d", x); 
}

int undigits(char *buf)
{
    if (strlen(buf) == 0)
    {
        return -1;
    }
    return atoi(buf);
}

int isqrt(int x)
{
    return (int)sqrt((double)x);
}

DynArray *primes = NULL;
bool prime(int x)
{
    if (primes == NULL)
    {
        primes = DynArray_new();
    }

    int ret = DynArray_get(primes, x);
    if (ret == -1)
    {
        if ((x < 2) ||
                (x % 2 == 0))
        {
            ret = false;
        }
        else
        {
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

    return (bool)ret;
}

