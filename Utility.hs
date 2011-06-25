module Utility where

import Data.Char
import Data.List
import Debug.Trace

debug :: Show a => String -> a -> a
debug msg x = trace (msg ++ ": " ++ (show x)) x

properDivisors :: Integer -> [Integer]
properDivisors = init . divisors -- For proper divisors, remove the last element, which is the input

divisors :: Integer -> [Integer]
divisors x = divisors' x 1

divisors' :: Integer -> Integer -> [Integer]
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

factors :: Integer -> [Integer]
factors 1 = [1]
factors n = factors' 2 [] n

factors' :: Integer -> [Integer] -> Integer -> [Integer]
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
