#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

gint64 sums(int x)
{
    assert(x > 0);
    if (x == 1)
    {
        return 0;
    }
    if (x == 2)
    {
        return 1;
    }

    int ret = 0;
    for (int i=1; i<x; i++)
    {
        ret += sums(i) + sums(x-i);
    }

    return ret;
}

gint64 answer()
{
    for (int i=1; i<10; i++)
    {
        printf("%d: %lld\n", i, sums(i));
    }
    return 0;
}

