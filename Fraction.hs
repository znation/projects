module Fraction where

import qualified Data.List as List
import Utility

-- TODO -- instance Num so Fractions can be used like numbers
type Fraction = (Integer, Integer)

reduce :: Fraction -> Fraction
reduce (n,d) =  let fn = factors n
                    fd = factors d
                    i = fn `List.intersect` fd
                    n' = List.product (fn List.\\ i)
                    d' = List.product (fd List.\\ i)
                in  (n',d')

curious :: Fraction -> Bool
curious (n,d) = let dn = digits n
                    dd = digits d
                    di = dn `List.intersect` dd
                    rn = (dn List.\\ di)
                    rd = (dd List.\\ di)
                    rn' = head rn
                    rd' = head rd
                in  if      length di /= 1
                    then    False -- no intersection or too much
                    else    if      head rd == 0
                            then    False
                            else    (reduce (rn', rd')) == (reduce (n, d))
                                    
product :: [Fraction] -> Fraction
product [] = (1,1)
product (x:xs) = product' x (Fraction.product xs)

product' :: Fraction -> Fraction -> Fraction
product' (nx,dx) (ny,dy) = (nx * ny, dx * dy)

lessThanOne :: Fraction -> Bool
lessThanOne (n,d) = n `div` d == 0

twoDigitFractions :: [Fraction]
twoDigitFractions = concat (map twoDigitFractions' [10..99])

twoDigitFractions' :: Integer -> [Fraction]
twoDigitFractions' x = map (twoDigitFractions'' x) [10..99]

twoDigitFractions'' :: Integer -> Integer -> Fraction
twoDigitFractions'' x y = (x,y)

trivial :: Fraction -> Bool
trivial (n,d) = if      n == d
                then    True
                else    if      n `mod` 10 == 0 && d `mod` 10 == 0
                        then    True
                        else    False
