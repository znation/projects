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

int compareFractions(gconstpointer ptra, gconstpointer ptrb)
{
    const Fraction *a = ptra;
    const Fraction *b = ptrb;

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
    const int limit = 10000;
    GList *fractions = NULL;
    Fraction *threeOverSeven = NULL;
    Fraction *twoOverSeven = NULL;
    for (int d=2; d<limit; d++)
    {
        for (int n=1; n<d; n++)
        {
            if (gcd(n, d) == 1)
            {
                Fraction *frac = fraction(n, d);

                if (threeOverSeven != NULL &&
                        twoOverSeven != NULL &&
                        (compareFractions(frac, threeOverSeven) != -1 ||
                         compareFractions(frac, twoOverSeven) != 1))
                {
                    free(frac);
                }
                else
                {
                    fractions = g_list_prepend(fractions, frac);
                }

                if (n == 3 && d == 7)
                {
                    threeOverSeven = frac;
                }
                else if (n == 2 && d == 7)
                {
                    twoOverSeven = frac;
                }
            }
        }
    }

    fractions = g_list_sort(fractions, compareFractions);

    assert(threeOverSeven != NULL);
    GList *elem = g_list_find(fractions, threeOverSeven);
    assert(elem != NULL);
    GList *prevElem = g_list_previous(elem);
    assert(prevElem != NULL);

    Fraction *prev = prevElem->data;
    return prev->numerator;

}

