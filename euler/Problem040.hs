module Problem040 where

import Data.Char

answer :: Integer
answer = product (map (d !!) [1,10,100,1000,10000,100000,1000000])

d :: [Integer]
d = let integers :: [Integer]
        integers = [0..]
        str :: String
        str = concat (map show integers)
    in  map (toInteger . digitToInt) str
