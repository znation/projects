module Problem002 where

answer :: Int
answer = sum (filter (\x -> x `mod` 2 == 0) (takeWhile (\x -> x < 4000000) (map fib [0,1..])))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))
