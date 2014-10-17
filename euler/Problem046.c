#include <stdio.h>

#include "Utility.h"

int answer()
{
    int ret = 0;

    for (int i=3;;i+=2)
    {
        if (prime(i))
        {
            continue;
        }

        int highestSqrt = isqrt(i);
        bool found = false;
        for (int j=1; j<=highestSqrt; j++)
        {
            int d = i - (2 * ipow(j, 2));
            if (d > 0 && prime(d))
            {
                found = true;
                printf("%d = %d + 2x%d^2\n", i, d, j);
                break;
            }
        }

        if (!found)
        {
            ret = i;
            break;
        }
    }
    
    return ret;
}

