module Problem031 where

-- TODO -- brute force is not working
-- needs a smarter algorithm.
-- are combinations like factors?

import Utility

answer :: Int
answer = combinations

nothing :: Maybe [Int] -> Bool
nothing x = x == Nothing

result :: Int
result = 200

denominations :: [Int]
denominations = reverse [1, 2, 5, 10, 20, 50, 100, 200]

combinations :: Int
combinations = sum (map firstCombine denominations)

firstCombine :: Int -> Int
firstCombine x =    let y = x `debug` "Starting with"
                    in  combine 0 y

-- returns combinations that add up to 2 pounds
combine :: Int -> Int -> Int
combine acc x = let total = acc + x
                    diff = result - total
                    ds = filter (<= diff) denominations
                in  if      diff == 0
                    then    1
                    else    if      diff < 0
                            then    0
                            else    sum (map (combine total) ds)
