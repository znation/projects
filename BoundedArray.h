#include <stdbool.h>
#include <glib.h>

#define BOUNDED_ARRAY_DEF(NAME, TYPE) typedef struct { \
    TYPE *array; \
    size_t length; \
} NAME; \
NAME NAME##_new(int length); \
NAME NAME##_copy(NAME in); \
void NAME##_free(NAME in); \


BOUNDED_ARRAY_DEF(BoundedArrayInt32, gint32)
BOUNDED_ARRAY_DEF(BoundedArrayInt64, gint64)
BOUNDED_ARRAY_DEF(BoundedArrayBool, bool)

GList * BoundedArrayInt32_toGList(BoundedArrayInt32 in);

