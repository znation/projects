#include <glib.h>
#include <stdio.h>
#include <stdlib.h>

#include "Utility.h"

#define MAX_CUBE 1000000

gboolean condition(gint64 p, BoundedArrayInt64 cubes)
{
    gint idx = binary_search(p, cubes);
    return (idx != -1) ? TRUE : FALSE;
}

gint64 answer()
{
    gint64 i, ret;
    gint c;
    BoundedArrayInt64 ps;

    BoundedArrayInt64 cubes = BoundedArrayInt64_new(MAX_CUBE);
    printf("Building cube database... ");
    for (i=0; i<MAX_CUBE; i++)
    {
        cubes.array[i] = ipow(i, 3);
    }
    printf("Done.\n");

    ret = 0;
    for (i=2; i<MAX_CUBE; i++)
    {
        gint64 n = cubes.array[i];
        printf("Testing cube %lld^3 = %lld\n", i, n);
        ps = integer_permutations(n, cubes, condition);
        c = ps.length;
        BoundedArrayInt64_free(ps);
        if (c == 3)
        {
            printf("DEBUG: found one at %lld^3 = %lld\n", i, n);
            return n;
        }
    }
    return ret;
}

