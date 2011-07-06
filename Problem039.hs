module Problem039 where

import Utility

answer :: Integer
answer = maximumSolutions 0 0 1

type Solution = (Integer, Integer, Integer)

maximumSolutions :: Int -> Integer -> Integer -> Integer
maximumSolutions _ n 1000 = n
maximumSolutions m n p =    let s = (length (solutions p) `debug` ("Solutions for " ++ (show p)))
                            in  if      s > m
                                then    maximumSolutions s p (p+1)
                                else    maximumSolutions m n (p+1)
                                
                                
solutions :: Integer -> [Solution]
solutions x = filter pythagoreanEquation (makeSolutions x)

pythagoreanEquation :: Solution -> Bool
pythagoreanEquation (a,b,c) = (a^2) + (b^2) == (c^2)

makeSolutions :: Integer -> [Solution]
makeSolutions x = filter (validSolution x) (concat (map (makeSolutions' x) [1..x]))

validSolution :: Integer -> Solution -> Bool
validSolution x (a,b,c) = a + b + c == x

makeSolutions' :: Integer -> Integer -> [Solution]
makeSolutions' x a = concat (map (makeSolutions'' x a) [1..x])

makeSolutions'' :: Integer -> Integer -> Integer -> [Solution]
makeSolutions'' x a b = map (makeSolution a b) [1..x]

makeSolution :: Integer -> Integer -> Integer -> Solution
makeSolution a b c = (a,b,c)
