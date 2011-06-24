module Problem016 where

import Data.Char

answer :: Integer
answer = sum (digits (2^1000))

digits :: Integer -> [Integer]
digits x =  let digitStr = show x
            in  map (toInteger . digitToInt) digitStr
