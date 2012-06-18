module Problem076 where

import qualified Data.List as List
import Sum

answer :: Int
answer =    countUniqueSums 6

unpair :: (a,a) -> [a]
unpair (x,y) = [x,y]

countUniqueSums :: Integer -> Int
countUniqueSums x = (length (uniqueSums x))

uniqueSums :: Integer -> [Sum]
uniqueSums x = List.nub (sums x)

sums :: Integer -> [Sum]
sums x =    let a :: Integer
                a = x - 1
                b :: [Integer]
                b = [1..a]
                c :: [Integer]
                c = reverse b
                d :: [(Integer, Integer)]
                d = zip b c
                e :: [[Integer]]
                e = map unpair d
                f = map toSum e
            in  f ++ (concat (map sums' e))

sums' :: [Integer] -> [Sum]
sums' s =   let a = s !! 0
                b = s !! 1
            in  (map (Sum.add a) (sums b))
