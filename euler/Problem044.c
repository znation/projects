#include <glib.h>
#include <stdio.h>
#include <assert.h>

#include "Utility.h"

gint64 answer()
{
    GList *pentagonals = listOfPentagonals();;
    GList *currElem = pentagonals;
    gint64 limit = G_MAXINT64 / 10;

    for (gint64 i=1;; i++)
    {
        if (currElem == NULL)
        {
            break;
        }

        int current = GPOINTER_TO_INT(currElem->data);
        if (current >= limit)
        {
            break;
        }

        GList *nextElem = g_list_next(currElem);
        if (nextElem == NULL)
        {
            break;
        }

        for (gint64 j=(i+1);; j++)
        {
            if (nextElem == NULL)
            {
                break;
            }
            
            int next = GPOINTER_TO_INT(nextElem->data);
            if (next >= limit)
            {
                break;
            }

            int sum = current + next;
            if (sum >= limit)
            {
                break;
            }
            gint64 sumIdx = pentagonal(sum);
            if (sumIdx == -1)
            {
                goto nextloop;
            }

            //printf("Found a sum possibility with: P%lld + P%lld = %d + %d = %d = P%lld\n",
            //        i,
            //        nextIdx,
            //        current,
            //        next,
            //        sum,
            //        sumIdx);

            int diff = next - current;
            assert(diff > 0);
            gint64 diffIdx = pentagonal(diff);
            if (diffIdx == -1)
            {
                goto nextloop;
            }

            printf("Found a definite solution with: P%lld + P%lld = %d + %d = %d = P%lld\n",
                    i,
                    j,
                    current,
                    next,
                    sum,
                    sumIdx);

            return diff;

nextloop:
            nextElem = g_list_next(nextElem);
        }

        currElem = g_list_next(currElem);
    }
    return 0;
}

