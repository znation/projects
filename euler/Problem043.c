#include <stdio.h>
#include <glib.h>
#include <limits.h>

#include "Utility.h"

void printDigits(GList *ds)
{
    GList *elem = ds;
    while (g_list_length(elem) > 0)
    {
        printf("%d", GPOINTER_TO_INT(elem->data));
        elem = g_list_next(elem);
    }
    printf("\n");
}

bool checkCondition(GList *ds, int idx, int mod)
{
    GList *elem = g_list_nth(ds, idx);
    int result = undigits(elem);
    if (result % mod != 0)
    {
        return false;
    }
    return true;
}

gint64 makePandigital(GList *ds)
{
    int len = g_list_length(ds);
    switch (len)
    {
        case 4:
            if (!checkCondition(ds, 1, 2))
            {
                return -1;
            }
            break;
        case 5:
            if (!checkCondition(ds, 2, 3))
            {
                return -1;
            }
            break;
        case 6:
            if (!checkCondition(ds, 3, 5))
            {
                return -1;
            }
            break;
        case 7:
            if (!checkCondition(ds, 4, 7))
            {
                return -1;
            }
            break;
        case 8:
            if (!checkCondition(ds, 5, 11))
            {
                return -1;
            }
            break;
        case 9:
            if (!checkCondition(ds, 6, 13))
            {
                return -1;
            }
            break;
        case 10:
            if (!checkCondition(ds, 7, 17))
            {
                return -1;
            }
            else
            {
                gint64 result = undigits(ds);
                printf("Found one, %lld\n", result);
                printDigits(ds);
                return result;
            }
    }

    gint64 result = 0;
    for (int i=0; i<10; i++)
    {
        if (!g_list_find(ds, GINT_TO_POINTER(i)))
        {
            GList *temp = g_list_copy(ds);
            temp = g_list_append(temp, GINT_TO_POINTER(i));
            gint64 n = makePandigital(temp);
            if (n != -1)
            {
                result += n;
            }
        }
    }
    return result;
}

gint64 answer()
{
    return makePandigital(NULL);
}



