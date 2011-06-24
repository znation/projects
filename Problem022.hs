module Problem022 where

import Data.Char
import Data.List
import Utility

answer :: [String] -> Integer
answer input = value (sort input)

value :: [String] -> Integer
value = value' 1

value' :: Integer -> [String] -> Integer
value' _ [] = 0
value' idx (x:xs) = let v = (value'' (debug (x ++ " index") idx) x)
                    in  (debug x v) + (value' (idx+1) xs)

value'' :: Integer -> String -> Integer
value'' idx str = (value''' str) * idx

value''' :: [Char] -> Integer
value''' [] = 0
value''' (x:xs) = toInteger (((ord x) - (ord 'A')) + 1) + (value''' xs)
