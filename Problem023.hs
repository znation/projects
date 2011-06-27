module Problem023 where

import Data.List
import Data.MemoCombinators
import Utility

answer :: Integer
answer =    let x = [1..100]
            in  sum (x \\ (abundantSums x))

abundantSums :: [Integer] -> [Integer]
abundantSums x =    let abundants = filter abundant x
                        pairs = handshake abundants
                    in  nub (map sumPair pairs)
                    
abundant :: Integer -> Bool
abundant =  let abundant' x = debug ("Abundant " ++ (show x)) (sum (properDivisors x) > x)
            in integral abundant'

sumPair :: (Integer,Integer) -> Integer
sumPair (x,y) = x + y