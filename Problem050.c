#include <assert.h>
#include <glib.h>
#include <stdio.h>

#include "Utility.h"

int answer()
{
    int mostConsecutivePrime = 0;
    int mostConsecutivePrimeLength = 0;
    GList *primeList = listOfPrimes();

    for (int i=6; i<1000; i++)
    {
        for (int k=0; k<g_list_length(primeList)-i; k++)
        {
            GList *curr = g_list_nth(primeList, k);

            // Take i primes between k and k+i
            int result = 0;
            for (int j=0; j<i; j++)
            {
                result += GPOINTER_TO_INT(curr->data);
                curr = g_list_next(curr);
                assert(curr != NULL);
            }

            if (result > 1000000)
            {
                break; // gone too far
            }

            if (prime(result))
            {
                printf("Found consecutive prime of length %d: %d\n", i, result);
                if (i > mostConsecutivePrimeLength)
                {
                    mostConsecutivePrime = result;
                    mostConsecutivePrimeLength = i;
                }
                break; // found one
            }
        }
    }

    return mostConsecutivePrime;
}

