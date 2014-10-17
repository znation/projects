#include <assert.h>
#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

const int pathLimit = 100;
const int matrixSize = 80;

typedef struct {
    gint64 runningTotal;
    int x,y;
} Path;

bool finished(GList *paths)
{
    GList *currElem = paths;
    while (currElem != NULL)
    {
        Path *currPath = currElem->data;
        if (currPath->x < (matrixSize - 1) ||
            currPath->y < (matrixSize - 1))
        {
            return false;
        }
        assert(currPath->x == matrixSize - 1);
        assert(currPath->y == matrixSize - 1);
        currElem = g_list_next(currElem);
    }
    return true;
}

void printPaths(GList *paths)
{
    GList *currElem = paths;
    while (currElem != NULL)
    {
        Path *currPath = currElem->data;
        printf("(%d, %d)\t%lld\n", currPath->x, currPath->y, currPath->runningTotal);
        currElem = g_list_next(currElem);
    }
}

GList * advance(GList *paths, int matrix[matrixSize][matrixSize])
{
    GList *currElem = paths;
    while (currElem != NULL)
    {
        Path *currPath = currElem->data;
        currElem = g_list_next(currElem);

        // move right (create new path and prepend
        if (currPath->x < matrixSize - 1)
        {
            Path *newPath = malloc(sizeof(Path));
            int x = newPath->x = currPath->x + 1;
            int y = newPath->y = currPath->y;
            newPath->runningTotal = currPath->runningTotal + matrix[x][y];
            paths = g_list_prepend(paths, newPath);
        }
        
        // move down (use existing path)
        if (currPath->y < matrixSize - 1)
        {
            currPath->y += 1;
            currPath->runningTotal += matrix[currPath->x][currPath->y];
        }
        else if (currPath->x != matrixSize - 1)
        {
            // done advancing but not finished, remove
            paths = g_list_remove(paths, currPath);
        }

    }

    return paths;
}

int pathSort(gconstpointer a, gconstpointer b)
{
    const Path *pathA = a;
    const Path *pathB = b;
    if (pathA->runningTotal > pathB->runningTotal)
    {
        return 1;
    }
    else if (pathA->runningTotal < pathB->runningTotal)
    {
        return -1;
    }
    else
    {
        return 0;
    }
}

GList * cull(GList *paths)
{
    paths = g_list_sort(paths, pathSort);

    // remove duplicates
    bool seen[matrixSize][matrixSize];
    memset(seen, false, sizeof(bool)*matrixSize*matrixSize);
    GList *currElem = paths;
    while (currElem != NULL)
    {
        Path *currPath = currElem->data;

        if (seen[currPath->x][currPath->y])
        {
            paths = g_list_remove(paths, currPath);
            memset(seen, false, sizeof(bool)*matrixSize*matrixSize);
            currElem = paths;
        }
        else
        {
            seen[currPath->x][currPath->y] = true;
            currElem = g_list_next(currElem);
        }
        
    }

    // cull down to pathLimit
    if (g_list_length(paths) > pathLimit)
    {
        paths = g_list_reverse(paths);
        while (g_list_length(paths) > pathLimit)
        {
            paths = g_list_remove(paths, paths->data);
        }
        paths = g_list_reverse(paths);
    }

    return paths;
}

gint64 answer()
{
    int matrix[matrixSize][matrixSize];

    // Read in matrix file
    FILE *fp;
    fp = fopen("Problem081_matrix.txt", "r");
    int bufSize = 16384;
    char buf[bufSize];
    memset(buf, 0, sizeof(char)*bufSize);
    int i = 0;
    while (fgets(buf, bufSize, fp))
    {
        int j = 0;
        char *tok = NULL;
        tok = strtok(buf, ",");
        while (tok != NULL)
        {
            int t;
            if (sscanf(tok, "%d", &t))
            {
                matrix[i][j] = t;
                tok = strtok(NULL, ",");
                j++;
            }
        }

        i++;
    }
    fclose(fp);

    GList *paths = NULL;

    Path *startingPath = malloc(sizeof(Path));
    startingPath->x = 0;
    startingPath->y = 0;
    startingPath->runningTotal = matrix[0][0];
    paths = g_list_prepend(paths, startingPath);

    i = 0;
    while (!finished(paths))
    {
        printf("Generation %d\n", i+1);
        printf("Advancing...\n");
        paths = advance(paths, matrix);
        printPaths(paths);
        printf("Culling...\n");
        paths = cull(paths);
        printPaths(paths);
        i++;
    }

    Path *bestPath = paths->data;
    return bestPath->runningTotal;
}

