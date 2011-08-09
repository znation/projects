#include <glib.h>
#include <stdio.h>
#include <assert.h>

#include "Utility.h"

gint64 answer()
{
    GList *pentagonals = listOfPentagonals();
    // force instantiation of pentagonalsLength
    printf("%d\n", pentagonal(1));

    GList *currElem = pentagonals;

    for (gint64 i=1;; i++)
    {
        int current = GPOINTER_TO_INT(currElem->data);
        if (current >= pentagonalsLength)
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
            int next = GPOINTER_TO_INT(nextElem->data);
            if (next >= pentagonalsLength)
            {
                break;
            }

            int sum = current + next;
            if (sum >= pentagonalsLength)
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

nextloop:
            nextElem = g_list_next(currElem);
            assert(nextElem != NULL);
            assert(nextElem != currElem);
            currElem = nextElem;
        }
    }
    return 0;
}

