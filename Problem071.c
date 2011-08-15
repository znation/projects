#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <glib.h>
#include <stdlib.h>

#include "Utility.h"

typedef struct {
    int numerator;
    int denominator;
    double dblRepresentation;
} Fraction;

Fraction *fraction(int n, int d)
{
    Fraction *ret = malloc(sizeof(Fraction));
    ret->numerator = n;
    ret->denominator = d;
    ret->dblRepresentation = (double)n/(double)d;
    return ret;
}

int compareFractions(Fraction *a, Fraction *b)
{
    if (a->dblRepresentation > b->dblRepresentation)
    {
        return 1;
    }
    else if (a->dblRepresentation < b->dblRepresentation)
    {
        return -1;
    }
    else
    {
        return 0;
    }
}

gint64 answer()
{
    const int limit = 1000000;
    Fraction *before = fraction(2, 7);
    for (int d=2; d<limit; d++)
    {
        int start = (int) floor((before->dblRepresentation)*(double)d);
        for (int n=start; n<d; n++)
        {
            double dblFrac = (double)n/(double)d;
            if (dblFrac >= (3.0/7.0))
            {
                break;
            }

            if (gcd(n, d) == 1)
            {
                if (dblFrac > before->dblRepresentation)
                {
                    assert(dblFrac < (3.0/7.0));
                    free(before);
                    Fraction *frac = fraction(n, d);
                    before = frac;
                }
            }
        }
    }
    return before->numerator;
}

