#include <glib.h>
#include <stdio.h>

#include "Utility.h"

gint64 answer()
{
    gint64 ret = 0;

    for (gint64 i=1; i<10; i++)
    {
        for (gint64 j=1; j<20; j++)
        {
            gint64 p = ipow(i, j);
            GList *ds = digits(p);
            bool c = g_list_length(ds) == j;
            if (c)
            {
                printf("Found one: %lld^%lld = %lld\n", i, j, p);
                ret++;
            }
        }
    }

    // Note: this program returns 47, but there are actually 49.
    // The last 2 are 9^20 and 9^21, which are too large to be
    // represented by even a 64-bit integer.
    return ret;
}

