using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HackerCup2012
{

    /*
Alfredo Spaghetti really likes soup, especially when it contains alphabet pasta. Every day he constructs a sentence from letters, places the letters into a bowl of broth and enjoys delicious alphabet soup.

Today, after constructing the sentence, Alfredo remembered that the Facebook Hacker Cup starts today! Thus, he decided to construct the phrase "HACKERCUP". As he already added the letters to the broth, he is stuck with the letters he originally selected. Help Alfredo determine how many times he can place the word "HACKERCUP" side-by-side using the letters in his soup.

Input
The first line of the input file contains a single integer T: the number of test cases. T lines follow, each representing a single test case with a sequence of upper-case letters and spaces: the original sentence Alfredo constructed.

Output
Output T lines, one for each test case. For each case, output "Case #t: n", where t is the test case number (starting from 1) and n is the number of times the word "HACKERCUP" can be placed side-by-side using the letters from the sentence.

Constraints
1 < T ≤ 20
Sentences contain only the upper-case letters A-Z and the space character
Each sentence contains at least one letter, and contains at most 1000 characters, including spaces
     */

    class AlphabetSoup
    {
        internal static void Run()
        {
            string inputLengthStr = Console.ReadLine();
            int T = Int32.Parse(inputLengthStr);
            for (int i = 0; i < T; i++)
            {
                string line = Console.ReadLine();
                string[] parts = line.Split(' ');
                Dictionary<char, int> letterCounts = new Dictionary<char, int>();
                foreach (char c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                {
                    letterCounts[c] = 0;
                }

                foreach (string part in parts)
                {
                    foreach (char c in part)
                    {
                        letterCounts[c]++;
                    }
                }

                int hackerCupCount = int.MaxValue;
                foreach (char c in "HACKERCUP")
                {
                    hackerCupCount = Math.Min(hackerCupCount, letterCounts[c]);
                }

                Console.WriteLine("Case #{0}: {1}", i + 1, hackerCupCount);
            }
        }
    }
}
