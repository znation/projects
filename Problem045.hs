module Problem045 where

import Utility

answer :: Integer
answer =    let condition :: Integer -> Bool
                condition x = (triangleNumber x) && (hexagonalNumber x) && (pentagonalNumber x) `debug` ("Testing " ++ (show x))
            in  head (filter condition [40756..])
