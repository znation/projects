#include <stdio.h>
#include "Utility.h"

int main()
{
    int i;

    // precompute primes below 1000000
    printf("#define MAX_PRIME 1000000\n");
    printf("int computedPrimes[MAX_PRIME] = {");
    for (i=0; i<1000000; i++)
    {
        printf("%d,", uncachedPrime(i) ? 1 : 0);
    }
    printf("};\n");

    return 0;
}

