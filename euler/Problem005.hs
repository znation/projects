module Problem005 where

import Data.List

answer :: Integer
answer = product (nub (map adjust (filter prime [2..20])))

factors :: Integer -> [Integer]
factors = factors' 2 []

factors' :: Integer -> [Integer] -> Integer -> [Integer]
factors' d acc x =  if      x == d
                    then    d:acc
                    else    let result = x `div` d
                                remainder = x `mod` d
                            in  if      remainder == 0 -- factor
                                then    let a = factors result
                                            b = factors d
                                        in  a ++ b
                                else    factors' (d+1) acc x

prime :: Integer -> Bool
prime x = (length (factors x)) == 1

-- Adjusts the primes to be their largest multiple <= 20
adjust :: Integer -> Integer
adjust = adjust' 2

adjust' :: Integer -> Integer -> Integer
adjust' m x =   if      (x * m) > 20
                then    x * (m - 1)
                else    adjust' (m + 1) x
