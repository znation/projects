module Problem031 where

-- TODO -- this is doing permutations, not combinations

import qualified Data.MemoCombinators as Memo
import Utility

answer :: Integer
answer = combinations

result :: Integer
result = 200

denominations :: [Integer]
denominations = reverse [1, 2, 5, 10, 20, 50, 100, 200]

combinations :: Integer
combinations = sum (map firstCombine denominations)

firstCombine :: Integer -> Integer
firstCombine x =    let y = x `debug` "Starting with"
                    in  combine 0 y

combine :: Integer -> Integer -> Integer
combine = Memo.memo2 Memo.integral Memo.integral combine'
                    
-- returns combinations that add up to 2 pounds
combine' :: Integer -> Integer -> Integer
combine' acc x =    let total = acc + x
                        diff = result - total
                        ds = filter (<= diff) denominations
                    in  if      diff == 0
                        then    1
                        else    if      diff < 0
                                then    0
                                else    sum (map (combine total) ds)
