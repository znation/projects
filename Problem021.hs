module Problem021 where

import Utility

answer :: Integer
answer = sum (filter amicableNumber [1..10000])

amicableNumber :: Integer -> Bool
amicableNumber x =  let d y = (sum (divisors y)) - y -- The "proper" divisors don't include the number itself
                    in  (d x /= x) && (d (d x) == x)
