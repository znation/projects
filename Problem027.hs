module Problem027 where

import Data.List
import Utility

data CoefficientPair = CoefficientPair Integer Integer
    deriving (Show, Eq)

answer :: Integer
answer = coefficientProduct (head (sort coefficientPairs))

coefficientPairs :: [CoefficientPair]
coefficientPairs =  let tuples :: [(Integer, Integer)]
                        tuples = handshake [-999..999]
                        makeCoefficientPair :: (Integer, Integer) -> CoefficientPair
                        makeCoefficientPair (x,y) = CoefficientPair x y
                    in  map makeCoefficientPair tuples

coefficientProduct :: CoefficientPair -> Integer
coefficientProduct (CoefficientPair a b) = a * b

instance Ord CoefficientPair where
    (<=) x y = (result x) <= (result y)
    
-- Determines the number of consecutive primes produced by the quadric equation with these coefficients
result :: CoefficientPair -> Int
result (CoefficientPair a b) =  let eq :: Integer -> Integer
                                    eq n = (n^2) + (a * n) + b
                                    l :: [Integer]
                                    l = map eq [0..]
                                in  length (takeWhile prime l)



