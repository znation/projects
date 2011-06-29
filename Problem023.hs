module Problem023 where

import Utility

answer :: Integer
answer =    let upperBound = 28123
                xs = [1..upperBound]
            in  (total xs upperBound) - (abundantTotal xs upperBound)

abundant :: Integer -> Bool
abundant x = sum (properDivisors x) > x

abundants :: [Bool]
abundants = map abundant [1..]

isAbundant :: Integer -> Bool
isAbundant x = (abundants !! (fromInteger (x-1))) --`debug` ("Abundant " ++ (show x))

pairs :: Integer -> [(Integer,Integer)]
pairs = let pairs' :: Integer -> Integer -> [(Integer,Integer)]
            pairs' x tot =  if     x == tot
                            then   []
                            else   (x,(tot-x)):(pairs' (x+1) tot)
        in  pairs' 1

abundantSum :: (Integer,Integer) -> Bool
abundantSum (x,y) = (isAbundant x) && (isAbundant y)

sumPair :: (Integer,Integer) -> Integer
sumPair (x,y) = x + y
                    
abundantTotal :: [Integer] -> Integer -> Integer
abundantTotal xs upperBound = sum (filter (<upperBound) (map sumPair (handshake (filter isAbundant xs))))

total :: [Integer] -> Integer -> Integer
total xs upperBound = sum (filter (<upperBound) (map sumPair (handshake xs)))
