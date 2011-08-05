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

