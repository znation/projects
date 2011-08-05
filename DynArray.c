#include "stdlib.h"
#include "string.h"
#include "stdio.h"
#include "stdbool.h"
#include "DynArray.h"

int DynArray_initSize = 10;

DynArray *DynArray_new()
{
    DynArray *da = malloc(sizeof(DynArray));
    da->length = DynArray_initSize;
    da->assigned = malloc(sizeof(bool)*da->length);
    da->elements = malloc(sizeof(int)*da->length);

    memset(da->assigned, 0, da->length);
    return da;
}

int DynArray_get(DynArray *da, int idx)
{
    while (idx >= da->length)
    {
        DynArray_grow(da);
    }

    if (da->assigned[idx])
    {
        return da->elements[idx];
    }
    
    return -1;
}

void DynArray_set(DynArray *da, int idx, int val)
{
    while (idx >= da->length)
    {
        DynArray_grow(da);
    }

    da->assigned[idx] = true;
    da->elements[idx] = val;
}

void DynArray_grow(DynArray *da)
{
    int oldlength = da->length;
    da->length = oldlength * 2;
    //printf("Dynamically growing array to %d\n", da->length);
    int *oldelements = da->elements;
    da->elements = malloc(sizeof(int)*da->length);

    free(da->assigned);
    da->assigned = malloc(sizeof(bool)*da->length);

    memset(da->assigned, 0, da->length);
    memcpy(da->elements, oldelements, oldlength);
    free(oldelements);
}

