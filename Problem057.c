#include <stdio.h>
#include <glib.h>
#include <string.h>
#include <stdlib.h>
#include <gmp.h>

#include "Utility.h"

gint64 answer()
{
    gint64 ret = 0;
    
    mpz_t numerator, denominator, number2;
    mpz_init_set_si(numerator, 3);
    mpz_init_set_si(denominator, 2);
    mpz_init_set_si(number2, 2);

    for (int i=1; i<=1000; i++)
    {
        double numerator_double = mpz_get_d(numerator);
        double denominator_double = mpz_get_d(denominator);
        double result = numerator_double/denominator_double;

        mpz_out_str(stdout, 10, numerator);
        printf("/");
        mpz_out_str(stdout, 10, denominator);
        printf(" = %f\n", result);

        char *numeratorStr = mpz_get_str(NULL, 10, numerator);
        char *denominatorStr = mpz_get_str(NULL, 10, denominator);
        if (strlen(numeratorStr) > strlen(denominatorStr))
        {
            ret++;
        }
        free(numeratorStr);
        free(denominatorStr);

        mpz_t d2, n2, temp;
        mpz_init(d2);
        mpz_init(n2);
        mpz_init(temp);

        mpz_add(d2, numerator, denominator);
        mpz_mul(temp, number2, d2);
        mpz_sub(n2, temp, numerator);

        mpz_set(numerator, n2);
        mpz_set(denominator, d2);
        
        mpz_clear(n2);
        mpz_clear(d2);
        mpz_clear(temp);
    }

    return ret;
}

