#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

typedef enum {
    TWO,
    THREE,
    FOUR,
    FIVE,
    SIX,
    SEVEN,
    EIGHT,
    NINE,
    TEN,
    JACK,
    QUEEN,
    KING,
    ACE,
    VALUECOUNT
} Value;

typedef enum {
    HEARTS,
    DIAMONDS,
    CLUBS,
    SPADES,
    SUITCOUNT
} Suit;

typedef struct {
    Suit suit;
    Value value;
} Card;

typedef enum {
    HIGHCARD,
    ONEPAIR,
    TWOPAIR,
    THREEOFAKIND,
    STRAIGHT,
    FLUSH,
    FULLHOUSE,
    FOUROFAKIND,
    STRAIGHTFLUSH,
    ROYALFLUSH,
} Rank;

typedef struct {
    Card cards[5];
    Rank rank;
    Card highcard;
} Hand;


char valueToChar(Value v)
{
    switch (v)
    {
        case TWO:
            return '2';
        case THREE:
            return '3';
        case FOUR:
            return '4';
        case FIVE:
            return '5';
        case SIX:
            return '6';
        case SEVEN:
            return '7';
        case EIGHT:
            return '8';
        case NINE:
            return '9';
        case TEN:
            return 'T';
        case JACK:
            return 'J';
        case QUEEN:
            return 'Q';
        case KING:
            return 'K';
        case ACE:
            return 'A';
        default:
            assert(false);
            return 'X';
    }
}

Value getValue(char c)
{
    switch (c)
    {
        case '2':
            return TWO;
        case '3':
            return THREE;
        case '4':
            return FOUR;
        case '5':
            return FIVE;
        case '6':
            return SIX;
        case '7':
            return SEVEN;
        case '8':
            return EIGHT;
        case '9':
            return NINE;
        case 'T':
            return TEN;
        case 'J':
            return JACK;
        case 'Q':
            return QUEEN;
        case 'K':
            return KING;
        case 'A':
            return ACE;
    }

    assert(false);
    return 0;
}

Suit getSuit(char c)
{
    switch (c)
    {
        case 'D':
            return DIAMONDS;
        case 'S':
            return SPADES;
        case 'C':
            return CLUBS;
        case 'H':
            return HEARTS;
    }

    assert(false);
    return 0;
}

bool flush(Hand *h)
{
    Suit suit = h->cards[0].suit;
    for (int i=1; i<5; i++)
    {
        if (h->cards[i].suit != suit)
        {
            return false;
        }
    }
    h->highcard = h->cards[0];
    return true;
}

bool straight(Hand *h)
{
    Value value = h->cards[0].value;
    for (int i=1; i<5; i++)
    {
        if (h->cards[i].value != value - i)
        {
            return false;
        }
    }

    h->highcard = h->cards[0];
    return true;
}

bool ofAKind(Hand *h, int n)
{
    for (int i=0; i<5; i++)
    {
        int count = 1;
        Card highcard = h->cards[i];
        for (int j=i+1; j<5; j++)
        {
            if (h->cards[i].value == h->cards[j].value)
            {
                count++;
            }
        }
        if (count >= n)
        {
            h->highcard = highcard;
            return true;
        }
    }
    return false;
}

bool fullHouse(Hand *h)
{
    Value value1, value2;
    Card card2;
    value1 = h->cards[0].value;
    for (int i=1; i<5; i++)
    {
        if (h->cards[i].value != value1)
        {
            value2 = h->cards[i].value;
            card2 = h->cards[i];
            break;
        }
    }

    int count1 = 0,
        count2 = 0;
    for (int i=0; i<5; i++)
    {
        if (h->cards[i].value == value1)
        {
            count1++;
        }
        else if (h->cards[i].value == value2)
        {
            count2++;
        }
    }

    if ((count1 == 2 && count2 == 3) || (count1 == 3 && count2 == 2))
    {
        h->highcard = (value1 > value2) ? h->cards[0] : card2;
        return true;
    }
    else
    {
        return false;
    }
}

bool twoPair(Hand *h)
{
    printf("Calculating two pair.\n");

    int set = 0;
    Value values[5];
    Card cards[5];
    int count[5];
    for (int i=0; i<5; i++)
    {
        printf("Card %d: ", i+1);

        bool found = false;
        for (int j=0; j<set; j++)
        {
            if (values[j] == h->cards[i].value)
            {
                found = true;
                count[j]++;
                printf(" matched value %d\n", j+1);
                break;
            }
        }
        if (!found)
        {
            values[set] = h->cards[i].value;
            cards[set] = h->cards[i];
            count[set] = 1;
            set++;
            printf(" set value %d with %c\n", set, valueToChar(h->cards[i].value));
        }
    }

    int pairs = 0;
    for (int i=0; i<set; i++)
    {
        printf("Count %d is %d\n", i, count[i]);
        if (count[i] == 2)
        {
            pairs++;
        }
    }

    return (pairs == 2);
}

int compare(Card a, Card b)
{
    if (a.value == b.value)
    {
        return 0;
    }
    else if (a.value > b.value)
    {
        return 1;
    }
    else
    {
        return -1;
    }
}

void sort(Hand *h)
{
    for (int i=0; i<4; i++)
    {
        for (int j=i+1; j<5; j++)
        {
            if (compare(h->cards[i], h->cards[j]) < 0)
            {
                Card temp = h->cards[i];
                h->cards[i] = h->cards[j];
                h->cards[j] = temp;
            }
        }
    }
}

void setRank(Hand *h)
{
    sort(h);

    if (straight(h) &&
            flush(h))
    {
        if (h->cards[0].value == 'A')
        {
            h->rank = ROYALFLUSH;
        }
        else
        {
            h->rank = STRAIGHTFLUSH;
        }
    }
    else if (ofAKind(h, 4))
    {
        h->rank = FOUROFAKIND;
    }
    else if (fullHouse(h))
    {
        h->rank = FULLHOUSE;
    }
    else if (flush(h))
    {
        h->rank = FLUSH;
    }
    else if (straight(h))
    {
        h->rank = STRAIGHT;
    }
    else if (ofAKind(h, 3))
    {
        h->rank = THREEOFAKIND;
    }
    else if (twoPair(h))
    {
        h->rank = TWOPAIR;
    }
    else if (ofAKind(h, 2))
    {
        h->rank = ONEPAIR;
    }
    else
    {
        h->rank = HIGHCARD;
        h->highcard = h->cards[0];
    }
}

int winner(Hand a, Hand b)
{
    if (a.rank == b.rank)
    {
        if (a.highcard.value == b.highcard.value)
        {
            // compare by cards in the hand
            for (int i=0; i<5; i++)
            {
                Card cardA = a.cards[i];
                Card cardB = b.cards[i];
                if (cardA.value > cardB.value)
                {
                    return 0;
                }
                else if (cardA.value < cardB.value)
                {
                    return 1;
                }
            }
        }
        else
        {
            return (a.highcard.value > b.highcard.value) ? 0 : 1;
        }
    }
    else
    {
        return (a.rank > b.rank) ? 0 : 1;
    }

    assert(false);
    return -1; // logic error?
}

char suitToChar(Suit s)
{
    switch (s)
    {
        case HEARTS:
            return 'H';
        case CLUBS:
            return 'C';
        case SPADES:
            return 'S';
        case DIAMONDS:
            return 'D';
        default:
            assert(false);
            return 'X';
    }
}

void printHand(Hand h, bool printRank)
{
    for (int i=0; i<5; i++)
    {
        char v = valueToChar(h.cards[i].value);
        char s = suitToChar(h.cards[i].suit);
        printf("%c%c ", v, s);
    }

    if (printRank)
    {
        char *buf;
        char v = valueToChar(h.highcard.value);
        char s = suitToChar(h.highcard.suit);
        switch (h.rank)
        {
            case ROYALFLUSH:
                buf = "Royal Flush";
                break;
            case STRAIGHTFLUSH:
                buf = "Straight Flush";
                break;
            case FOUROFAKIND:
                buf = "Four of a kind";
                break;
            case FLUSH:
                buf = "Flush";
                break;
            case STRAIGHT:
                buf = "Straight";
                break;
            case FULLHOUSE:
                buf = "Full House";
                break;
            case THREEOFAKIND:
                buf = "Three of a kind";
                break;
            case TWOPAIR:
                buf = "Two Pair";
                break;
            case ONEPAIR:
                buf = "One Pair";
                break;
            case HIGHCARD:
                buf = "Nothing";
                break;
            default:
                assert(false);
                break;
        }
        printf("(%s with high card %c%c)", buf, v, s);
    }
    printf("\n");
}

gint64 answer()
{
    Hand players[2];
    gint64 ret = 0;
    FILE *fp;
    fp = fopen("Problem054_poker.txt", "r");
    int bufSize = 16384;
    char buf[bufSize];
    memset(buf, 0, sizeof(char)*bufSize);
    
    int i = 0;
    while (fgets(buf, bufSize, fp))
    {
        char *tok = NULL;
        tok = strtok(buf, " ");
        int j = 0;
        while (tok != NULL)
        {
            char value, suit;
            if (sscanf(tok, "%c%c", &value, &suit))
            {
                int player = j / 5;
                Card card;
                card.suit = getSuit(suit);
                card.value = getValue(value);
                players[player].cards[j % 5] = card;
                tok = strtok(NULL, " ");
            }
            j++;
        }

        // Compare hands
        setRank(&(players[0]));
        setRank(&(players[1]));
        int w = winner(players[0], players[1]);
        for (int i=0; i<2; i++)
        {
            printf("Player %d: ", i == 0 ? 1 : 2);
            printHand(players[i], true);
        }
        printf("Winner: Player %d\n\n", w == 0 ? 1 : 2);
        if (w == 0)
        {
            ret++;
        }

        i++;
    }
    fclose(fp);

    return ret;
}

