#include <glib.h>
#include <stdio.h>

#include "Utility.h"

#define MAX_CUBE 1290

gint64 answer()
{
    int cubes[MAX_CUBE];
    printf("Building cube database... ");
    for (int i=0; i<MAX_CUBE; i++)
    {
        cubes[i] = ipow(i, 3);
    }
    printf("Done.\n");

    int ret = 0;
    for (int i=2; i<MAX_CUBE; i++)
    {
        int n = cubes[i];
        printf("Testing cube %d\n", n);
        GList *ps = integer_permutations(n);
        int c = 0;
        GList *elem = ps;
        while (elem != NULL)
        {
            int p = GPOINTER_TO_INT(elem->data);
            int idx = binary_search(p, cubes, MAX_CUBE);
            if (idx != -1)
            {
                printf("\t%d^3 = %d\n", idx, p);
                c++;
            }
            elem = g_list_next(elem);
        }
        g_list_free(ps);
        if (c == 5)
        {
            printf("DEBUG: found one at %d^3 = %d\n", i, n);
            return n;
        }
    }
    return ret;
}

