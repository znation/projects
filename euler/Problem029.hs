module Problem029 where

import Data.List
import Utility

answer :: Int
answer = length (nub (exponentialCombinations [2..100]))

exponentialCombinations :: [Integer] -> [Integer]
exponentialCombinations xs =    let pairs = handshake xs
                                    exponentiate (x,y) = x ^ y
                                in  map exponentiate pairs
                                