#include <glib.h>
#include <stdio.h>
#include <stdlib.h>

#include "Utility.h"

#define MAX_CUBE 1000000

gint64 answer()
{
    BoundedArrayInt64 cubes = BoundedArrayInt64_new(MAX_CUBE);
    printf("Building cube database... ");
    for (int i=0; i<MAX_CUBE; i++)
    {
        cubes.array[i] = ipow(i, 3);
    }
    printf("Done.\n");

    gint64 ret = 0;
    for (gint64 i=2; i<MAX_CUBE; i++)
    {
        gint64 n = cubes.array[i];
        printf("Testing cube %lld\n", n);
        BoundedArrayInt64 ps = integer_permutations(n);
        int c = 0;
        for (int j=0; j<ps.length; j++)
        {
            gint64 p = ps.array[j];
            int idx = binary_search(p, cubes);
            if (idx != -1)
            {
                printf("\t%d^3 = %lld\n", idx, p);
                c++;
            }
        }
        BoundedArrayInt64_free(ps);
        if (c == 3)
        {
            printf("DEBUG: found one at %lld^3 = %lld\n", i, n);
            return n;
        }
    }
    return ret;
}

