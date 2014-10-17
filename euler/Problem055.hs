module Problem055 where

import Utility

answer :: Int
answer = length (filter lychrel [1..9999])

lychrel :: Integer -> Bool
lychrel =   let lychrel' :: Int -> Integer -> Bool
                lychrel' 50 _ = True
                lychrel' i x =  let x' = reverseDigits x
                                    r = x + x'
                                in  if      palindromic r 10
                                    then    False
                                    else    lychrel' (i+1) r
            in  lychrel' 0
