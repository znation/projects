module Problem050 where

import Data.Array
import Utility

consecutiveLength :: Int
consecutiveLength = 1000

lim :: Int
lim = 1000000

answer :: (Int,[Int])
answer =    let s = reverse [1..consecutiveLength]
                p = map consecutivePrimes s
                p' = zip s p
                p'' = map lessThanLimit p'
                p''' = filter nonNull p''
            in  head p'''

nonNull :: (Int,[Int]) -> Bool
nonNull (_,xs) = not (null xs)
            
lessThanLimit :: (Int,[Int]) -> (Int,[Int])
lessThanLimit (i,xs) = (i, (filter (<lim) xs))

primeNumbers :: Array Int Int
primeNumbers = listArray (0,lim) (filter prime [0..lim])

consecutivePrimes :: Int -> [Int]
consecutivePrimes = let consecutivePrimes' :: Int -> Int -> [Int]
                        consecutivePrimes' idx x
                            | idx == x = []
                            | otherwise =   let s = sum (arraySlice primeNumbers idx (idx+x))
                                                xs = consecutivePrimes' (idx+1) x
                                            in  if      prime s
                                                then    s:xs
                                                else    xs
                    in  consecutivePrimes' 0
