module Problem039 where

import Data.List
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
solutions x = unique (filter pythagoreanEquation (makeSolutions x))

pythagoreanEquation :: Solution -> Bool
pythagoreanEquation (a,b,c) = (square a) + (square b) == (square c)

makeSolutions :: Integer -> [Solution]
makeSolutions x = [(a,b,c) | a <- [1..(x-2)], b <- [1..(x-a-1)], c <- [(x-a-b)]]
                    
solution :: [Integer] -> Solution
solution (a:b:c:[]) = (a,b,c)
solution _ = error "Bad input for solution"
                    
sortSolution :: Solution -> Solution
sortSolution (a,b,c) =  let xs = [a,b,c]
                            sorted = sort xs
                        in  solution sorted
                    
unique :: [Solution] -> [Solution]
unique xs = let sorted = map sortSolution xs
            in  nub sorted
