module Problem048 where

answer :: String
answer = reverse (take 10 (reverse (show (sum problem48series))))

problem48series :: [Integer]
problem48series = map problem48series' [1..1000]

problem48series' :: Integer -> Integer
problem48series' x = x ^ x
