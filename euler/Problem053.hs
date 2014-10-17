module Problem053 where

import Utility

answer :: Int
answer = length (filter (> 1000000) nCr)

nCr :: [Integer]
nCr = concat (map nCr' [1..100])

nCr' :: Integer -> [Integer]
nCr' n = map (nCr'' n) [1..n]

nCr'' :: Integer -> Integer -> Integer
nCr'' n r = (factorial n) `div` ((factorial r) * (factorial (n-r)))
