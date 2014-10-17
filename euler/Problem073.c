#include <glib.h>
#include <stdio.h>
#include <math.h>
#include "Utility.h"

gint64 answer()
{
    int d,
        ret = 0;

    for (d=2; d<=12000; d++)
    {
        int n;

        for (n = (int) floor((double)d / 3.0); n <= (int) ceil((double)d / 2.0); n++)
        {
            double val = (double)n / (double)d;
            if (val > (1.0/3.0) && val < (1.0/2.0))
            {
                if (gcd(n, d) == 1)
                {
                    printf("Found one: %d/%d\n", n, d);
                    ret++;
                }
            }
        }
    }

    return ret;
}

