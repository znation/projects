module Problem049 where

import Data.List
import Utility

answer :: [Integer]
answer = concat (map digits (increasingSequence 1 1000))

increasingSequence :: Integer -> Integer -> [Integer]
increasingSequence i x =    let y = x + i
                                z = y + i
                                l = (x:y:z:[]) --`debug` "Trying sequence"
                            in  if      z > 9999
                                then    increasingSequence (i+1) 1000
                                else    if      condition l
                                        then    l
                                        else    increasingSequence i (x+1)

condition :: [Integer] -> Bool
condition (x:y:z:_) =   let p = permutations (digits x)
                        in  (all prime (x:y:z:[])) &&
                            (digits y) `elem` p &&
                            (digits z) `elem` p
condition xs = error ("Invalid input to condition: " ++ (show xs))
