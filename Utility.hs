module Utility where

import Data.Char
import Data.List
import Debug.Trace

debug :: Show a => a -> String -> a
debug x msg = trace (msg ++ ": " ++ (show x)) x

properDivisors :: (Integral a, Num a) => a -> [a]
properDivisors x =  let fs = filter (\y -> y /= x) (nub (factors x))
                    in  nub (1:(sort ((map (div x) fs) ++ fs)))

divisors :: (Integral a, Num a) => a -> [a]
divisors x = divisors' x 1

divisors' :: (Integral a, Num a) => a -> a -> [a]
divisors' x n = if      n > x `div` 2
                then    [x]
                else    let xs = divisors' x (n+1)
                        in  if      x `mod` n == 0
                            then    n:xs
                            else    xs   
                         
-- Follows http://www.ehow.com/how_5169234_calculate-number-divisors.html
countDivisors :: Integer -> Int
countDivisors x =   let primeFactors = factors x
                        duplicatesRemoved = nub primeFactors
                        countOccurrences n = length (filter (\m -> m == n) primeFactors)
                        exponents = map countOccurrences duplicatesRemoved
                    in  product (map (+1) exponents)

factors :: (Integral a) => a -> [a]
factors 1 = [1]
factors n = factors' 2 [] n

factors' :: (Integral a) => a -> [a] -> a -> [a]
factors' d acc x =  if      x == d
                    then    d:acc
                    else    let result = x `div` d
                                remainder = x `mod` d
                            in  if      remainder == 0 -- factor
                                then    let a = factors result
                                            b = factors d
                                        in  a ++ b
                                else    factors' (d+1) acc x
                                
prime :: Integer -> Bool
prime x = (length (factors x)) == 1

triangleNumber :: Integer -> Integer
triangleNumber x = sum [1..x]

digits :: Integer -> [Integer]
digits x =  let digitStr = show x
            in  map (toInteger . digitToInt) digitStr

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

handshake :: [a] -> [(a, a)]
handshake xs = concat (map (handshake' xs) xs)

handshake' :: [a] -> a -> [(a, a)]
handshake' xs x = map (handshake'' x) xs

handshake'' :: a -> a -> (a, a)
handshake'' x y = (x,y)
