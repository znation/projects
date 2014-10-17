module Problem006 where

answer :: Integer
answer = abs ((sum (map square [1..100])) - (square (sum [1..100])))

square :: Integer -> Integer
square x = x * x
