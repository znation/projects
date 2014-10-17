module Problem012 where

import Utility

answer :: Integer
answer = head (filter over500divisors (triangleNumbers' 1))

over500divisors :: Integer -> Bool
over500divisors x = (countDivisors x) > 500

triangleNumber :: Integer -> Integer
triangleNumber x = sum [1..x]

-- Generates triangle numbers from an index
triangleNumbers' :: Integer -> [Integer]
triangleNumbers' x = (triangleNumber x):(triangleNumbers' (x+1))
