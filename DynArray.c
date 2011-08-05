#include "stdlib.h"
#include "string.h"
#include "stdio.h"
#include "stdbool.h"
#include "assert.h"
#include "DynArray.h"

int DynArray_initSize = 10;

DynArray *DynArray_new()
{
    DynArray *da = malloc(sizeof(DynArray));
    da->length = DynArray_initSize;
    da->assigned = malloc(sizeof(bool)*da->length);
    da->elements = malloc(sizeof(int)*da->length);

    memset(da->assigned, 0, da->length);
    memset(da->elements, 0, da->length);
    return da;
}

int DynArray_get(DynArray *da, int idx)
{
    assert(DynArray_contains(da, idx));
    return da->elements[idx];
}

bool DynArray_contains(DynArray *da, int idx)
{
    if (idx >= da->length)
    {
        return false;
    }
    return da->assigned[idx];
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
    int newlength = oldlength * 2;
    da->length = newlength;
    //printf("Dynamically growing array to %d\n", da->length);
    bool *oldassigned = da->assigned;
    int *oldelements = da->elements;
    da->elements = malloc(sizeof(int)*newlength);
    da->assigned = malloc(sizeof(bool)*newlength);
    memset(da->assigned, 0, da->length);
    memset(da->elements, 0, da->length);
    memcpy(da->assigned, oldassigned, sizeof(bool)*oldlength);
    memcpy(da->elements, oldelements, sizeof(int)*oldlength);
    free(oldassigned);
    free(oldelements);
}

