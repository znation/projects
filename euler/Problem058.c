#include <stdio.h>
#include <glib.h>

#include "Utility.h"

gint64 answer()
{
    int numCorners = 1;
    int numPrimeCorners = 0;

    int corners = 0;
    int sideLength = 2;
    int sideCounter = 0;
    int layers = 0;
    for (int i=2;; i++)
    {
        sideCounter++;
        if (sideCounter == sideLength)
        {
            corners++;
            sideCounter = 0;

            numCorners++;
            printf("Corner at %d", i);
            if (prime(i))
            {
                numPrimeCorners++;
                printf(" (prime)");
            }
            printf("\n");
        }
        if (corners == 4)
        {
            sideLength += 2;
            sideCounter = 0;
            corners = 0;
            layers++;

            double ratio = (double)numPrimeCorners/(double)numCorners;
            printf("Ratio for %d layers is %lf\n", layers, ratio);

            if (ratio < 0.1)
            {
                return sideLength - 1;
            }
        }
    }

    return 0;
}

