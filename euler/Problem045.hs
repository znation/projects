module Problem045 where

import Utility

answer :: Integer
answer =    let condition :: Integer -> Bool
                condition x = (pentagonalNumber x) `debug` ("Testing " ++ (show x))
            in  head (drop 2 (filter condition hexagonalNumbers))
            
-- Don't need to test triangle numbers, since every hexagonal number is also a triangle number
