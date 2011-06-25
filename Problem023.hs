module Problem023 where

import Data.List
import Utility

answer :: a -> Integer
answer _ =  let x = [1..3]
            in  sum (x \\ (abundantSums x))

abundantSums :: [Integer] -> [Integer]
abundantSums x =    let pairs = nub (map (take 2) (permutations x))
                    in  nub (map sum (filter (all abundant) pairs))
                    
abundant :: Integer -> Bool
abundant x = debug ("Abundant " ++ (show x)) (sum (properDivisors x) > x)

-- TODO -- memoize abundant using the recursive technique