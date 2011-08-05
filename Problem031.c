typedef struct Currency {
    int amounts[8];
} Currency;

int countCurrency(Currency curr, int idx);
void printCurrency(Currency curr);
int valueCurrency(Currency curr);

int valueCount = 8;
int values[] = {200, 100, 50, 20, 10, 5, 2, 1};

int answer()
{
    Currency curr;
    for (int i=0; i<valueCount; i++)
    {
        curr.amounts[i] = 0;
    }
    printf("Counting currency...\n"); 
    int result = countCurrency(curr, 0);
    printf("Done.\n");
    printf("%d\n", result);

    return 0;
}

int countCurrency(Currency curr, int idx)
{
    if (valueCurrency(curr) == 200)
    {
        printCurrency(curr);
        return 1;
    }

    int count = 0;
    for (int i=idx; i<valueCount; i++)
    {
        int val = valueCurrency(curr);
        int rem = 200 - val;
        Currency next = curr;
        if (rem >= values[i])
        {
            (next.amounts)[i]++;
            count += countCurrency(next, i);
        }
    }
    return count;
}

int valueCurrency(Currency curr)
{
    int amount = 0;
    for (int i=0; i<valueCount; i++)
    {
        amount += (curr.amounts)[i] * values[i];
    }
    return amount;
}

void printCurrency(Currency curr)
{
    for (int i=0; i<valueCount; i++)
    {
        int amount = (curr.amounts)[i] * values[i];
        printf("%d", amount);
        if (i != valueCount - 1)
        {
            printf(" ");
        }
    }
    printf("\n");
}

