#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

gint64 sums(int x)
{
    if (x == 1)
    {
        return 0;
    }
    if (x == 2)
    {
        return 1;
    }
    int ret = 1;
    for (int i=1; i<x; i++)
    {
        ret += sums(i);
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

