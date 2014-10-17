#include <glib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <gmp.h>
#include <mpfr.h>

#include "Utility.h"

gint64 answer()
{
    int bestBase = 0;
    int bestExp = 0;

    mpfr_rnd_t round = mpfr_get_default_rounding_mode();
    mpfr_t normalizedBestBase;
    mpfr_init_set_d(normalizedBestBase, 0.0, round);


    FILE *fp;
    fp = fopen("Problem099_base_exp.txt", "r");
    char buf[128];
    int i = 1;
    while (fgets(buf, 128, fp))
    {
        int base,
            exp;

        sscanf(buf, "%d,%d", &base, &exp);

        // Adjust base/exp to compare to best
        mpfr_t adjBase,
               adjExp,
               factor,
               invFactor;

        mpfr_init(invFactor);
        mpfr_init_set_si(adjBase, base, round);
        mpfr_init_set_si(adjExp, exp, round);

        mpfr_init_set_d(factor, 1000.0, round);
        mpfr_div(factor, factor, adjExp, round);

        mpfr_mul(adjExp, adjExp, factor, round);

        mpfr_d_div(invFactor, 1.0, factor, round);

        mpfr_pow(adjBase, adjBase, invFactor, round);

        if (mpfr_cmp(adjBase, normalizedBestBase) > 0)
        {
            bestExp = exp;
            bestBase = base;
            mpfr_set(normalizedBestBase, adjBase, round);
            printf("Candidate: %d ^ %d (adjusted ", base, exp);
            
            mpfr_out_str(stdout, 10, 10, adjBase, round);

            printf(" ^ ");
            
            mpfr_out_str(stdout, 10, 10, adjExp, round);

            printf(") (line %d)\n", i);
        }

        i++;
    }

    return 0;
}

