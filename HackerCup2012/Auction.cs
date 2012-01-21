using System;
using System.Collections.Generic;
using System.Collections;

namespace HackerCup2012
{
    /*
You have encountered a new fancy online auction that offers lots of products. You are only interested in their price and weight. We shall say that product A is strictly preferred over product B if A costs less than B and is not heavier (they may be of equal weight) or if A weighs less and is not more expensive (they can have equal price).

We shall call a product A a bargain if there is no product B such that B is better than A. Similarly, we shall call a product C a terrible deal if there exists no product D such that C is better than D. Note that according to our definitions, the same product may be both a bargain and a terrible deal! Only wacky auctioneers sell such products though.

One day you wonder how many terrible deals and bargains are offered. The number of products, N, is too large for your human-sized brain though. Fortunately, you discovered that the auction manager is terribly lazy and decided to sell the products based on a very simple pseudo-random number generator.

If product i has price Pi and weight Wi, then the following holds for product i+1:

Pi = ((A*Pi-1 + B) mod M) + 1 (for all i = 2..N)
Wi = ((C*Wi-1 + D) mod K) + 1 (for all i = 2..N)
You carefully calculated the parameters for the generator (P1, W1, M, K, A, B, C and D). Now you want to calculate the number of terrible deals and bargains on the site.

Input
The first line of the input file contains a single integer T: the number of test cases. T lines follow, each representing a single test case with 9 space-separated integers: N, P1, W1, M, K, A, B, C and D.

Output
Output T lines, one for each test case. For each case, output "Case #t: a b", where t is the test case number (starting from 1), a is the number of terrible deals and b is the number of bargains.

Constraints
1 ≤ T ≤ 20
1 ≤ N ≤ 10^18
1 ≤ M, K ≤ 107
1 ≤ P1 ≤ M
1 ≤ W_1 ≤ K
0 ≤ A,B,C,D ≤ 109
     */

    class Auction
    {
        class Product
        {
            public Product(int p, int w)
            {
                P = p;
                W = w;
            }

            public int P { get; set; }
            public int W { get; set; }
            public bool? IsBargain { get; set; }
            public bool? IsTerribleDeal { get; set; }

            public static bool Preferred(Product A, Product B)
            {
                return ((A.P < B.P && A.W <= B.W) ||
                        (A.W < B.W && A.P <= B.P));
            }

            public bool IsTerribleDealFn(List<Product> products)
            {
                foreach (Product D in products)
                {
                    if (Preferred(this, D))
                    {
                        return false;
                    }
                }
                return true;
            }

            public void SetTerribleDeal(List<Product> products)
            {
                Product C = this;

                foreach (Product D in products)
                {
                    if (C != D && Preferred(C, D))
                    {
                        C.IsTerribleDeal = false;
                        D.IsBargain = false;
                    }
                }

                if (C.IsTerribleDeal == null)
                {
                    C.IsTerribleDeal = true;
                }
            }

            public bool IsBargainFn(List<Product> products)
            {
                foreach (Product B in products)
                {
                    if (Preferred(B, this))
                    {
                        return false;
                    }
                }
                return true;
            }

            public void SetBargain(List<Product> products)
            {
                Product A = this;

                foreach (Product B in products)
                {
                    if (A != B && Preferred(B, A))
                    {
                        A.IsBargain = false;
                        B.IsTerribleDeal = false;
                    }
                }

                if (A.IsBargain == null)
                {
                    A.IsBargain = true;
                }
            }
        }

        internal static void Run()
        {
            string inputLengthStr = Console.ReadLine();
            int T = Int32.Parse(inputLengthStr);
            for (int i = 0; i < T; i++)
            {
                string line = Console.ReadLine();
                string[] parts = line.Split(' ');
                Int64 N = Int64.Parse(parts[0]); // number of products
                int P1 = Int32.Parse(parts[1]); // initial price
                int W1 = Int32.Parse(parts[2]); // initial weight
                int M = Int32.Parse(parts[3]);
                int K = Int32.Parse(parts[4]);
                int A = Int32.Parse(parts[5]);
                int B = Int32.Parse(parts[6]);
                int C = Int32.Parse(parts[7]);
                int D = Int32.Parse(parts[8]);

                List<Product> products = new List<Product>();
                products.Add(new Product(P1, W1));

                int prevP = P1;
                int prevW = W1;

                // Add to sorted list
                for (Int64 j = 1; j < N; j++)
                {
                    int P = ((A * prevP + B) % M) + 1;
                    int W = ((C * prevW + D) % K) + 1;

                    Product prodNext = new Product(P, W);
                    if (prodNext.IsBargainFn(products) || prodNext.IsTerribleDealFn(products))
                    {
                        products.Add(new Product(P, W));
                    }

                    prevP = P;
                    prevW = W;
                }

                foreach (Product p in products)
                {
                    if (p.IsBargain == null)
                    {
                        p.SetBargain(products);
                    }

                    if (p.IsTerribleDeal == null)
                    {
                        p.SetTerribleDeal(products);
                    }
                }

                int bargains = 0;
                int terribleDeals = 0;
                foreach (Product p in products)
                {
                    if (p.IsBargain.Value)
                    {
                        bargains++;
                    }

                    if (p.IsTerribleDeal.Value)
                    {
                        terribleDeals++;
                    }
                }

                Console.WriteLine("Case #{0}: {1} {2}", i + 1, terribleDeals, bargains);
            }
        }
    }
}
