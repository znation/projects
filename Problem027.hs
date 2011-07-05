module Problem027 where

import Utility

data CoefficientPair = CoefficientPair Integer Integer
    deriving (Show, Eq)

answer :: Integer
answer = coefficientProduct (bestResult coefficientPairs)

coefficientPairs :: [CoefficientPair]
coefficientPairs =  let tuples :: [(Integer, Integer)]
                        tuples = handshake [-999..999]
                        makeCoefficientPair :: (Integer, Integer) -> CoefficientPair
                        makeCoefficientPair (x,y) = CoefficientPair x y
                    in  map makeCoefficientPair tuples

coefficientProduct :: CoefficientPair -> Integer
coefficientProduct (CoefficientPair a b) = a * b

bestResult :: [CoefficientPair] -> CoefficientPair
bestResult = bestResult' (CoefficientPair 0 0) 0

bestResult' :: CoefficientPair -> Int -> [CoefficientPair] -> CoefficientPair
bestResult' p _ [] = p
bestResult' p pr (x:xs) =   let r = result x
                            in  if      r > pr
                                then    bestResult' x r xs
                                else    bestResult' p pr xs

-- Determines the number of consecutive primes produced by the quadric equation with these coefficients
result :: CoefficientPair -> Int
result p =  let eq :: Integer -> Integer
                eq = quadraticEquation p
                l :: [Integer]
                l = map eq [0..]
            in  length (takeWhile prime l)

quadraticEquation :: CoefficientPair -> Integer -> Integer
quadraticEquation (CoefficientPair a b) n = (n^2) + (a * n) + b


