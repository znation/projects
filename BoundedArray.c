#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <assert.h>
#include "BoundedArray.h"

#define BOUNDED_ARRAY_IMPL(NAME, TYPE) NAME NAME##_new(size_t length) \
{ \
    int bytes; \
    NAME ret; \
    bytes = sizeof(TYPE) * length; \
    ret.initialized = TRUE; \
    ret.length = length; \
    ret.array = malloc(bytes); \
    memset(ret.array, 0, bytes); \
    return ret; \
} \
void NAME##_free(NAME in) \
{ \
    assert(in.initialized); \
    free(in.array); \
    in.initialized = FALSE; \
} \


BOUNDED_ARRAY_IMPL(BoundedArrayInt32, gint32)
BOUNDED_ARRAY_IMPL(BoundedArrayInt64, gint64)
BOUNDED_ARRAY_IMPL(BoundedArrayBool, gboolean)

GList * BoundedArrayInt32_toGList(BoundedArrayInt32 in) 
{ 
    GList *ret = NULL;
    int i;

    for (i=0; i<in.length; i++) 
    { 
        ret = g_list_prepend(ret, GINT_TO_POINTER(in.array[i])); 
    } 
    ret = g_list_reverse(ret); 
    return ret; 
}

