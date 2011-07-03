module Utility where

import Data.Char
import Data.List
import Debug.Trace

import qualified Data.MemoCombinators as Memo

debug :: Show a => a -> String -> a
debug x msg = trace (msg ++ ": " ++ (show x)) x

properDivisors :: Integer -> [Integer]
properDivisors x =  let fs = filter (\y -> y /= x) (nub (factors x))
                    in  nub (1:(sort ((map (div x) fs) ++ fs)))

divisors :: Integer -> [Integer]
divisors x = (properDivisors x) ++ [x]
                         
-- Follows http://www.ehow.com/how_5169234_calculate-number-divisors.html
countDivisors :: Integer -> Int
countDivisors x =   let primeFactors = factors x
                        duplicatesRemoved = nub primeFactors
                        countOccurrences n = length (filter (\m -> m == n) primeFactors)
                        exponents = map countOccurrences duplicatesRemoved
                    in  product (map (+1) exponents)

factors :: Integer -> [Integer]
factors n = factors' 2 [] n

factors' :: Integer -> [Integer] -> Integer -> [Integer]
factors' _ _ 0 = [0]
factors' _ _ 1 = [1]
factors' d acc x =  if      d > (isqrt x)
                    then    x:acc
                    else    let (result,remainder) = x `divMod` d
                            in  if      remainder == 0 -- factor
                                then    let a = factors result
                                            b = factors d
                                        in  a ++ b
                                else    factors' (d+1) acc x
                        

isqrt :: Integer -> Integer
isqrt x =   let f :: Double
                f = fromInteger x
            in  floor (sqrt f)
            
circularPrime :: Integer -> Bool
circularPrime x = and (map prime (rotations x))

prime :: Integer -> Bool
prime x = primes !! (fromInteger x)

primes :: [Bool]
primes = map isPrime [0..]

isPrime :: Integer -> Bool
isPrime x = isPrime' 2 x

isPrime' :: Integer -> Integer -> Bool
isPrime' _ 0 = False
isPrime' _ 1 = False
isPrime' d x =   if      d > (isqrt x)
                then    True
                else    let remainder = x `mod` d
                        in  if      remainder == 0 -- factor
                            then    False
                            else    isPrime' (d+1) x

triangleNumbers :: (Integral a) => [a]
triangleNumbers = map makeTriangleNumber [1..]

makeTriangleNumber :: (Integral a) => a -> a
makeTriangleNumber x = sum [1..x]

triangleNumber :: (Integral a) => a -> Bool
triangleNumber x =  let subset = takeWhile (\y -> y <= x) triangleNumbers
                    in  elem x subset

digits :: Integer -> [Integer]
digits x =  let digitStr = show x
            in  map (toInteger . digitToInt) digitStr

undigits :: [Integer] -> Integer
undigits ds =   let undigits' :: Integer -> [Integer] -> Integer
                    undigits' _ [] = 0
                    undigits' tens (x:xs) = (x * (10 ^ tens)) + (undigits' (tens+1) xs)
                in  undigits' 0 (reverse ds)
            
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

handshake :: [a] -> [(a, a)]
handshake xs = concat (map (handshake' xs) xs)

handshake' :: [a] -> a -> [(a, a)]
handshake' xs x = map (handshake'' x) xs

handshake'' :: a -> a -> (a, a)
handshake'' x y = (x,y)

fibonacci :: [Integer]
fibonacci = map fibonacci' [0..]

fibonacci' :: Integer -> Integer
fibonacci' =  Memo.integral fibonacci''

fibonacci'' :: Integer -> Integer
fibonacci'' 0 = 0
fibonacci'' 1 = 1
fibonacci'' 2 = 1
fibonacci'' x = (fibonacci' (x-1)) + (fibonacci' (x-2))

rotate :: [a] -> Int -> [a]
rotate l 0 = l
rotate l n = (drop n l) ++ (take n l)

rotations :: Integer -> [Integer]
rotations x =   let ds :: [Integer]
                    ds = digits x
                    l :: Int
                    l = length ds
                    rs :: [[Integer]]
                    rs = map (rotate ds) [0..(l-1)]
                in  map undigits rs
