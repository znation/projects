#include "Answer.h"

int truncateLeft(int x)
{
    char ds[10];
    digits(x, ds);
    int ret = undigits(&(ds[1]));
    return ret;
}

int truncateRight(int x)
{
    char ds[10];
    digits(x, ds);
    int l = strlen(ds);
    ds[l-1] = 0;
    int ret = undigits(ds);
    return ret;
}

bool truncatable(int x, bool skipFirst, bool left)
{
    if (x == -1)
    {
        return true;
    }

    if (!skipFirst && !prime(x))
    {
        return false;
    }

    int y;
    if (left)
    {
        y = truncateLeft(x);
    }
    else
    {
        y = truncateRight(x);
    }

    return truncatable(y, false, left);
}

int answer()
{
    int sum = 0;
    int count = 0;
    for (int i=10; i<INT_MAX && count<11; i++)
    {
        if (prime(i) &&
            truncatable(i, true, true) &&
            truncatable(i, true, false))
        {
            printf("Found truncatable prime %d\n", i);
            sum += i;
            count++;
        }
    }
    return sum;
}

