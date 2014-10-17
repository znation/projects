module Problem041 where

import Data.List
import Utility

answer :: Integer
answer = last (filter isPrime input)

input :: [Integer]
input = sort (map undigits (concat (map permutations ranges)))

ranges :: [[Integer]]
ranges = map range [1..9]

range :: Integer -> [Integer]
range x = [1..x]
