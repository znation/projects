#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "Utility.h"

int answer()
{
    int answer = 0;

    for (int i=2; i<1000000; i++)
    {
        char dbuf[1024];
        memset(dbuf, 0, 1024);

        for (int j=1; j<10000; j++)
        {
            int product = i * j;
            char prodbuf[10];
            sprintf(prodbuf, "%d", product);
            strcat(dbuf, prodbuf);
            int result = atoi(dbuf);
            if (result > 987654321)
            {
                break;
            }
            else if (result > answer && pandigital9(result))
            {
                printf("Found possible answer %d\n", result);
                answer = result;
                break;
            }
        }
    } 

    return answer;
}
