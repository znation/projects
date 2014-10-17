#include <stdio.h>
#include <glib.h>

#include "Utility.h"

gint64 answer()
{
    gint64 bestn = 0;

    // Theory: every prime number, multiplied by the total, introduces a better ratio
    // the code below produces best ratios at:
    // 2 (2 * 1)
    // 6 (3 * 2)
    // 30 (5 * 6)
    // 210 (7 * 30)
    // 2310 (11 * 210)
    // 30030 (13 * 2310)
    GList *primes = listOfPrimes();
    GList *currElem = primes;
    gint64 total = 1;
    while (currElem != NULL)
    {
        int p = GPOINTER_TO_INT(currElem->data);
        gint64 next = total * p;
        if (next > 1000000)
        {
            return total;
        }
        total = next;
        currElem = g_list_next(currElem);
    }

    //double bestratio = 0.0;

    //printf("n\tt(n)\tn/t(n)\n");

    //for (gint64 n=2; n<=100000; n++)
    //{
    //    double t = totient(n);
    //    double result = (double)n / t;

        //printf("%lld\t%lf\t%lf\n", n, t, result);

    //    if (result > bestratio)
    //    {
    //        gint64 multiple = bestn == 0 ? 0 : (n/bestn);
    //        printf("Found new best %lld (=%lld * %lld) with ratio %lf,\n", n, multiple, bestn, result);
    //        bestn = n;
    //        bestratio = result;
    //    }
    //}

    return bestn;
}

