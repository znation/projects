#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <array>
#include <algorithm>

using namespace std;

int main(int argc, char **argv)
{
	const int BUF_SIZE = 501;
	char buf[BUF_SIZE];
	ifstream input("BeautifulStrings_input.txt", ifstream::in);
	
	// get input line count
	input.getline(buf, BUF_SIZE);
	int lineCount = atoi(buf);

	for (int i=0; i<lineCount; i++)
	{
		int result = 0;
		input.getline(buf, BUF_SIZE);

		// now we have the input for 1 line

		std::array<int, 26> count;
		for (size_t j=0; j<26; j++)
		{
			count[j] = 0;
		}

		// build up a table of char counts
		for (size_t j=0; j<strlen(buf); j++)
		{
			char c = buf[j];
			if (islower(c))
			{
				c = toupper(c);
			}
			if (isupper(c))
			{
				int idx = (c - 65);
				count[idx]++;
			}
		}

		std::sort(count.begin(), count.end());

		for (size_t j=26; j>0; j--)
		{
			size_t idx = j-1;
			//cout << "Adding " << count[idx] << " * " << j << " to result " << result << endl;
			result += (count[idx] * j);
		}

		cout << "Case #" << (i+1) << ": " << result << endl;
	}
}
