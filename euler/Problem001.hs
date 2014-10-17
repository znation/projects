module Problem001 where

answer :: Int
answer = sum (filter condition [0,1..1000])

condition :: Int -> Bool
condition x = (x < 1000) && ((x `mod` 3 == 0) || (x `mod` 5 == 0))
