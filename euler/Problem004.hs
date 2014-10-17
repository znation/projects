module Problem004 where

answer :: Integer
answer = maximum (filter palindromic (threeDigitMultiples [100..999]))

threeDigitMultiples :: [Integer] -> [Integer]
threeDigitMultiples l = concat (map (multiply l) l)

multiply :: [Integer] -> Integer -> [Integer]
multiply l x = map (*x) l

palindromic :: Integer -> Bool
palindromic x = palindromic' (show x) 0

palindromic' :: [Char] -> Int -> Bool
palindromic' l idx =    if      idx > ((length l) `div` 2)
                        then    True
                        else    if      (l !! ((length l) - idx - 1)) == (l !! idx)
                                then    palindromic' l (idx+1)
                                else    False
