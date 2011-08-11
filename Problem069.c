#include <stdio.h>
#include <glib.h>

#include "Utility.h"

gint64 answer()
{
    gint64 bestn = 0;
    double bestratio = 0.0;

    //printf("n\tt(n)\tn/t(n)\n");

    for (gint64 n=2; n<=1000000; n++)
    {
        double t = totient(n);
        double result = (double)n / t;

        //printf("%lld\t%lf\t%lf\n", n, t, result);

        if (result > bestratio)
        {
            bestn = n;
            bestratio = result;
        }
    }

    return bestn;
}

