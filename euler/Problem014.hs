module Problem014 where

import Data.List

answer :: Integer
answer = snd (last (sort (map collatzChain [1..999999])))

-- produces a pair of (idx, lengthOfChain)
collatzChain :: Integer -> (Int, Integer)
collatzChain x =    let chain = collatz x
                    in  ((length chain), x)
                    
collatz :: Integer -> [Integer]
collatz x = if      x == 1
            then    [1]
            else    if      x `mod` 2 == 0
                    then    x:(collatz (x `div` 2))
                    else    x:(collatz ((3 * x) + 1))
