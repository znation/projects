module Problem034 where

import Utility

answer :: Integer
answer = sum (filter equalFactorialSum [10..1000000])

equalFactorialSum :: Integer -> Bool
equalFactorialSum x = (factorialSum x) == x

factorialSum :: Integer -> Integer
factorialSum x =    let ds = digits x
                    in  sum (map factorial ds)
