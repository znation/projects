#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <array>
#include <algorithm>

using namespace std;

typedef unsigned long ulong;

bool nextCombination(
	ulong n,
	ulong k,
	ulong *prevCombination /*array of k elements*/)
{
	long incrementIdx = 0;
	for (long i=k-1, j = 1; i>=0; i--, j++)
	{
		if (prevCombination[i] != n - j)
		{
			incrementIdx = i;
			break;
		}
	}

	if (incrementIdx == 0 && prevCombination[0] == n - k)
	{
		return false;
	}

	// increment at the idx
	prevCombination[incrementIdx]++;

	for (size_t i=incrementIdx+1; i<k; i++)
	{
		prevCombination[i] = prevCombination[i-1]+1;
	}
	return true;
}

int main(int argc, char **argv)
{
	ifstream input("CardGame_example_input.txt", ifstream::in);
	
	// get input line count
	ulong lineCount;
       	input >> lineCount;

	for (ulong i=0; i<lineCount; i++)
	{
		ulong N, K;
	       	input >> N;
		input >> K;
		
		ulong *a = new ulong[N];
		
		ulong *combination = new ulong[K];
		for (ulong j=0; j<K; j++)
		{
			combination[j] = j;
		}

		ulong total = 0;

		for (ulong j=0; j<N; j++)
		{
			input >> a[j];
		}

		do
		{
			// look at indices in current combination
			ulong largest = 0;
			for (ulong m=0; m<K; m++)
			{
				largest = max(largest,
					a[combination[m]]);
			}
			total += largest;
			total = total % 1000000007ul;
		}
		while (nextCombination(N, K, combination));

		cout << "Case #" << (i+1) << ": " << total << endl;
	}
}
