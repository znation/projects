#include <fstream>
#include <sstream>
#include <string>
#include <iostream>
#include <vector>

using namespace std;

std::vector<long> nextm(const std::vector<long> m, long k)
{
	std::vector<bool> gaps((2 * k) + 1);
	std::vector<long> ret(k);

	for (long j=1; j<=(2*k); j++)
	{
		gaps[j] = true;
	}

	for (long j=0; j<k; j++)
	{
		if (m[j] <= (2 * k))
		{
			//cout << "\tDEBUG -- m contains " << m[j] << endl;
			gaps[m[j]] = false;
		}
	}

	long ret_count = 0;
	for (long j=1; j<=(2 * k); j++)
	{
		if (gaps[j])
		{
			//cout << "\tDEBUG -- adding to ret " << j << endl;
			ret[ret_count] = j;
			ret_count++;
			if (ret_count == k)
			{
				//cout << "\tDEBUG -- stopping after " << ret_count << endl;
				break;
			}
		}
	}

	return ret;
}

long next_in_m(std::vector<long> values)
{
	long ret = 1;
	for (auto it = values.begin(); it != values.end(); it++)
	{
		if (*it == ret)
		{
			ret++;
		}
		else
		{
			break;
		}
	}
	return ret;
}

int main(int argc, char **argv)
{
	string line;
	ifstream input("FindTheMin_example_input.txt", ifstream::in);

	
	// get input line count
	getline(input, line);
	istringstream iss(line);
	int lineCount;
	iss >> lineCount;

	for (int i=0; i<lineCount; i++)
	{
		long n, k, a, b, c, r;
		getline(input, line);
		iss = istringstream(line);
		iss >> n;
		iss >> k;
		getline(input, line);
		iss = istringstream(line);
		iss >> a;
		iss >> b;
		iss >> c;
		iss >> r;

		//printf("\tDEBUG -- n=%d, k=%d\n", n, k);

		std::vector<long> m(k);
		m[0] = a;

		for (long j=1; j<k; j++)
		{
			m[j] = (b * m[j-1] + c) % r;
			
		}

		/*
		for (long j=0; j<k; j++)
		{
			printf("\tDEBUG -- %d\n", m[j]);
		}
		*/

		/*
		while (n > k)
		{
			m = nextm(m, k);
			n -= k;

			for (long j=0; j<k; j++)
			{
				printf("\tDEBUG -- %d\n", m[j]);
			}
		}
		*/

		std::vector<long> afterk = nextm(m, k);
		n -= k;

		long nextval;
		while (n > 0)
		{
			// now afterk is sorted (lowest first)
			nextval = next_in_m(afterk);
			afterk[nextval-1] = nextval;
			n--;
		}
		
		//cout << "\tDEBUG -- lastmIdx is " << lastmIdx << endl;

		cout << "Case #" << (i+1) << ": " << nextval << endl;
	}
}
