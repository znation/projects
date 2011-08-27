#include <stdlib.h>
#include <string.h>
#include "BoundedArray.h"

#define BOUNDED_ARRAY_IMPL(NAME, TYPE) NAME NAME##_copy(NAME in) \
{ \
    NAME ret; \
    ret.array = in.array; \
    ret.length = in.length; \
    return ret; \
} \
NAME NAME##_new(int length) \
{ \
    NAME ret; \
    ret.length = length; \
    ret.array = malloc(sizeof(TYPE) * length); \
    memset(ret.array, 0, (sizeof(TYPE) / sizeof(char)) * length); \
    return ret; \
} \
void NAME##_free(NAME in) \
{ \
    free(in.array); \
} \


BOUNDED_ARRAY_IMPL(BoundedArrayInt32, gint32)
BOUNDED_ARRAY_IMPL(BoundedArrayInt64, gint64)
BOUNDED_ARRAY_IMPL(BoundedArrayBool, bool)

GList * BoundedArrayInt32_toGList(BoundedArrayInt32 in) 
{ 
    GList *ret = NULL; 
    for (int i=0; i<in.length; i++) 
    { 
        ret = g_list_prepend(ret, GINT_TO_POINTER(in.array[i])); 
    } 
    ret = g_list_reverse(ret); 
    return ret; 
}

