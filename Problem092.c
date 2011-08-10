#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "Utility.h"

#define LIMIT 1000000

int *squareChainCache = NULL;

int squareChain(int x)
{
    assert(x < LIMIT);

    if (squareChainCache == NULL)
    {
        int len = sizeof(int) * LIMIT;
        printf("Allocating %d bytes... ", len);
        squareChainCache = malloc(len);
        printf("done.\n");
        squareChainCache[1] = 1;
        squareChainCache[89] = 89;
    }

    if (squareChainCache[x])
    {
        return squareChainCache[x];
    }
    else
    {
        //printf("%d -> ", x);
        int result = 0;
        GList *ds = digits(x);
        GList *currElem = ds;
        while (currElem != NULL)
        {
            int curr = GPOINTER_TO_INT(currElem->data);
            result += (curr * curr);
            currElem = g_list_next(currElem);
        }
        g_list_free(ds);
        
        int arrive = squareChain(result);
        squareChainCache[x] = arrive;
        return arrive;
    }
}

gint64 answer()
{
    gint64 ret = 0;
    for (int i=1; i<LIMIT; i++)
    {
        //printf("Testing %d\n", i);
        if (squareChain(i) == 89)
        {
            ret++;
        }
    }
    return ret;
}


