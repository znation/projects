module Problem052 where

import Data.List
import Utility

answer :: Integer
answer = head (containsSameDigits [1..6] [1..])

containsSameDigits :: [Integer] -> [Integer] -> [Integer]
containsSameDigits multipliers xs = filter (containsSameDigits' multipliers) xs

containsSameDigits' :: [Integer] -> Integer -> Bool
containsSameDigits' multipliers n = let ns = map (* n) multipliers
                                    in  sameDigits ns (head ns)
                                    
sameDigits :: [Integer] -> Integer -> Bool
sameDigits [] _ = True
sameDigits (x:xs) n = (sameDigits' x n) && (sameDigits xs n)

sameDigits' :: Integer -> Integer -> Bool
sameDigits' x y =   let dx = sort (digits x)
                        dy = sort (digits y)
                    in  dx == dy
