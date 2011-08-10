#include <stdio.h>
#include <glib.h>
#include <gmp.h>

#include "Utility.h"

gint64 answer()
{
    gint64 ret = 0;
    
    gint64 numerator = 3;
    gint64 denominator = 2;

    for (int i=1; i<=1000; i++)
    {
        double result = (double)numerator/(double)denominator;
        printf("%lld/%lld = %f\n", numerator, denominator, result);

        GList *dn = digits(numerator);
        GList *dd = digits(denominator);
        if (g_list_length(dn) > g_list_length(dd))
        {
            ret++;
        }
        g_list_free(dn);
        g_list_free(dd);

        gint64 d2 = numerator + denominator;
        gint64 n2 = (2 * d2) - numerator;
        numerator = n2;
        denominator = d2;
    }

    return ret;
}

