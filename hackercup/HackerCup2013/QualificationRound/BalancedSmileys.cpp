#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <array>
#include <algorithm>

using namespace std;

bool interpret(const char *buf, int parens, bool isBeginning, size_t length, int realityNumber)
{
	//cout << "DEBUG: reality " << realityNumber << ", string is \"" << buf << "\"" << endl;
	bool ret = false;
	for (size_t j=0; j<length; j++)
	{
		if (buf[j] == '(')
		{
			if (!isBeginning && buf[j-1] == ':')
			{
				ret = ret | interpret(buf+j+1, parens, false, length-(j+1), realityNumber+1);
			}

			//cout << "DEBUG: incrementing parens in reality " << realityNumber << endl;
			parens++;
		}
		else if (buf[j] == ')')
		{
			if (isBeginning || buf[j-1] != ':')
			{
				if (parens > 0)
				{
					//cout << "DEBUG: decrementing parens in reality " << realityNumber << endl;
					parens--;
				}
				else
				{
					// mismatch -- close without an open
					//cout << "DEBUG: unmatched close-parens in reality " << realityNumber << endl;
					parens = -1;
					break;
				}
			}
			else 
			{
				if (!isBeginning && buf[j-1] == ':')
				{
					ret = ret | interpret(buf+(j+1), parens, false, length-(j+1), realityNumber+1);
				}

				if (parens > 0)
				{
					//cout << "DEBUG: decrementing parens in reality " << realityNumber << endl;
					parens--;
				}
				else
				{
					// mismatch -- close without an open
					//cout << "DEBUG: unmatched close-parens in reality " << realityNumber << endl;
					parens = -1;
					break;
				}
			}
		}

		isBeginning = false;
	}

	ret = ret || (parens == 0);
	return ret;
}

int main(int argc, char **argv)
{
	const int BUF_SIZE = 101;
	char buf[BUF_SIZE];
	ifstream input("BalancedSmileys_input.txt", ifstream::in);
	
	// get input line count
	input.getline(buf, BUF_SIZE);
	int lineCount = atoi(buf);

	for (int i=0; i<lineCount; i++)
	{
		int parens = 0;
		input.getline(buf, BUF_SIZE);

		// now we have the input for 1 line
		bool result = interpret(buf, 0, true, strlen(buf), 1);

		//cout << "DEBUG: line was " << buf << endl;
		const char * resultText = result ? "YES" : "NO";
		cout << "Case #" << (i+1) << ": " << resultText << endl;
	}
}
