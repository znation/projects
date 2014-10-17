module Problem023 where

import Data.Array
import Utility

upperBound :: Int
upperBound = 28123

answer :: Int
answer = sum (filter (not . abundantSum) [1..upperBound])

abundant :: Int -> Bool
abundant x = (sum (properDivisors x) > x) `debug` ("Abundant " ++ (show x))

abundants :: Array Int Bool
abundants = listArray (0, upperBound) (map abundant [0..])

isAbundant :: Int -> Bool
isAbundant x = abundants ! x

pairs :: Int -> [(Int,Int)]
pairs = let pairs' :: Int -> Int -> [(Int,Int)]
            pairs' x tot =  if     x == tot
                            then   []
                            else   (x,(tot-x)):(pairs' (x+1) tot)
        in  pairs' 1

abundantPair :: (Int,Int) -> Bool
abundantPair (x,y) = isAbundant x && isAbundant y

abundantSum :: Int -> Bool
abundantSum x = let p = take 1 (filter abundantPair (pairs x))
                in  length p > 0
