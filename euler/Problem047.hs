module Problem047 where

import Data.List
import Utility

x :: Int
x = 4

answer :: [Int]
answer = findConsecutive 0 (filter distinctPrimeFactors [1..])

distinctPrimeFactors :: Int -> Bool
distinctPrimeFactors n = length (nub (factors n)) == x

findConsecutive :: Int -> [Int] -> [Int]
findConsecutive n xs =  let possible = take x (drop n xs)
                        in  if      consecutive possible
                            then    possible
                            else    findConsecutive (n+1) xs
