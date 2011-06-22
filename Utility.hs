module Utility where

import Debug.Trace

debug :: Show a => String -> a -> a
debug msg x = trace (msg ++ ": " ++ (show x)) x

divisors :: Integer -> [Integer]
divisors x = divisors' x 1

divisors' :: Integer -> Integer -> [Integer]
divisors' x n = if      n > x `div` 2
                then    [x]
                else    let xs = divisors' x (n+1)
                        in  if      x `mod` n == 0
                            then    x:xs
                            else    xs

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
