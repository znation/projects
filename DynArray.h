
typedef struct DynArray {
    int *elements;
    bool *assigned;
    int length;
} DynArray;

DynArray *DynArray_new();
int DynArray_get(DynArray *da, int idx);
void DynArray_set(DynArray *da, int idx, int val);
void DynArray_grow(DynArray *da);

