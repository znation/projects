#include <glib.h>
#include <stdio.h>

#include "Utility.h"

gint64 answer()
{
    gint64 result = 0;

    for (gint64 i=1000; i<10000; i++)
    {
        for (gint64 j=1; j<3334 && (i+(2*j) < 10000); j++)
        {
            gint64 a = i;
            if (!prime(a))
            {
                continue;
            }

            gint64 b = a + j;
            if (!prime(b) || !isPermutation(a, b))
            {
                continue;
            }

            gint64 c = b + j;
            if (!prime(c) || !isPermutation(a, c))
            {
                continue;
            }

            printf("Found one: %lld %lld %lld\n", a, b, c);

            GList *listA = digits(a);
            GList *listB = digits(b);
            GList *listC = digits(c);

            listA = g_list_concat(listA, listB);
            listA = g_list_concat(listA, listC);

            result = undigits(listA);

            g_list_free(listA);
        }
    }

    return result;
}

