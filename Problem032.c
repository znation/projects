#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include "Utility.h"

int answer()
{
    // Find triplets of the form i * j == k
    // Where (i,j,k) is 1 through 9 pandigital
    // i must be 2 digits and j must be 3 digits ???

    GList *answers = NULL;
    int answer = 0;

    for (int i=0; i<1000; i++)
    {
        int ids[4];
        int idsLength = digits(i, ids);

        bool seen[10];
        memset(seen, false, 10);
        bool valid = true;

        for (int j=0; j<idsLength && valid; j++)
        {
            if (ids[j] == 0)
            {
                valid = false;
                break;
            }

            if (seen[ids[j]])
            {
                valid = false;
                break;
            }

            seen[ids[j]] = true;
        }

        if (valid)
        {
            // Try 3 digit numbers
            for (int k=0; k<1000; k++)
            {
                valid = true;
                int kds[4];
                int kdsLength = digits(k, kds);
                bool seen3[10];
                memcpy(seen3, seen, sizeof(bool)*10);

                for (int l=0; l<kdsLength && valid; l++)
                {
                    if (kds[l] == 0)
                    {
                        valid = false;
                        break;
                    }

                    if (seen3[kds[l]])
                    {
                        valid = false;
                        break;
                    }

                    seen3[kds[l]] = true;
                }

                if (valid)
                {

                    int product = i * k;
                    int pds[10];
                    int pdsLength = digits(product, pds);
                    bool productSeen[10];
                    memcpy(productSeen, seen3, sizeof(bool)*10);

                    if (pdsLength + idsLength + kdsLength == 9)
                    {
                        for (int m=0; m<pdsLength; m++)
                        {
                            if (pds[m] == 0)
                            {
                                valid = false;
                                break;
                            }
                            if (productSeen[pds[m]])
                            {
                                valid = false;
                                break;
                            }
                            productSeen[pds[m]] = true;
                        }

                        if (valid)
                        {

                            GList *found = g_list_find(answers, GINT_TO_POINTER(product));
                            if (!found)
                            {
                                answer += product;
                                printf("Found one! %d * %d = %d\n", i, k, product);
                                answers = g_list_prepend(answers, GINT_TO_POINTER(product));
                            }
                        }
                    }
                }
            } 
        }
    }

    return answer;
}

