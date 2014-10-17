module Problem056 where

import Utility

answer :: Integer
answer = maximum (map (sum . digits) input)

input :: [Integer]
input = [(a^b) | a <- ([1..99] :: [Integer]), b <- ([1..99] :: [Integer])]
