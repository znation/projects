#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <stdio.h>

#include "Utility.h"

gint64 answer()
{
    int v = 0;
    GList *input = NULL;
    while (scanf("%d,", &v) != EOF)
    {
        input = g_list_prepend(input, GINT_TO_POINTER(v));
    }
    input = g_list_reverse(input);

    int inputLength = g_list_length(input);
    char *buf = malloc(sizeof(char) * (inputLength+1));
    buf[inputLength] = 0;
    for (int i=0; i<inputLength; i++)
    {
        int temp = GPOINTER_TO_INT(g_list_nth_data(input, i));
        buf[i] = (char)temp;
    }

    char key[3];
    char *decrypted = malloc(sizeof(char) * (inputLength+1));
    decrypted[inputLength] = 0;
    
    char bestKey[3];
    double bestAlpha = 0.0;

    for (char a='a'; a<='z'; a++)
    {
        key[0] = a;
        for (char b='a'; b<='z'; b++)
        {
            key[1] = b;
            for (char c='a'; c<='z'; c++)
            {
                key[2] = c;
                
                int i;
                for (i=0; i<inputLength; i++)
                {
                    decrypted[i] = buf[i] ^ key[i % 3];
                    if (!g_ascii_isprint(decrypted[i]))
                    {
                        break;
                    }
                }

                if (i == inputLength)
                {
                    // Determine whether this has a higher percentage of alphabetic characters
                    int alphaCount = 0;
                    for (i=0; i<inputLength; i++)
                    {
                        if (g_ascii_isalpha(decrypted[i]))
                        {
                            alphaCount++;
                        }
                    }

                    double alpha = (double)alphaCount/(double)inputLength;
                    if (alpha > bestAlpha)
                    {
                        bestAlpha = alpha;
                        for (i=0; i<3; i++)
                        {
                            bestKey[i] = key[i];
                        }
                    }

                }
            }
        }
    }

    printf("Best key was \"%s\" with %f%% alpha characters\n", bestKey, bestAlpha);

    gint64 result = 0;
    for (int i=0; i<inputLength; i++)
    {
        char d = buf[i] ^ bestKey[i % 3];
        result += d;
    }

    return result;
}

