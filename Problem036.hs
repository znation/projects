module Problem036 where

import Data.Char
import Numeric

answer :: Integer
answer = sum (filter (palindromicInBases [2,10]) [0..999999])

palindromic :: Integer -> Integer -> Bool
palindromic x base = palindromic' (showAtBase base x) 0

palindromic' :: [Char] -> Int -> Bool
palindromic' l idx =    if      idx > ((length l) `div` 2)
                        then    True
                        else    if      (l !! ((length l) - idx - 1)) == (l !! idx)
                                then    palindromic' l (idx+1)
                                else    False

palindromicInBases :: [Integer] -> Integer -> Bool
palindromicInBases bases x = and (map (palindromic x) bases)

showAtBase :: Integer -> Integer -> String
showAtBase base x = showIntAtBase base intToDigit x ""
