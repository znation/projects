module Problem009 where

answer :: Integer
answer = tripletProduct (head (filter pythagoreanTriplet (filter addsTo1000 triplets)))

type Triplet = (Integer, Integer, Integer)

pythagoreanTriplet :: Triplet -> Bool
pythagoreanTriplet (a,b,c) = (square a) + (square b) == (square c)

addsTo1000 :: Triplet -> Bool
addsTo1000 (a,b,c) = (a + b + c) == 1000

triplets :: [Triplet]
triplets = triplets' 1 []

triplets' :: Integer -> [Triplet] -> [Triplet]
triplets' 1000 acc = acc
triplets' a acc = (triplets'' a 1 acc) ++ (triplets' (a+1) acc)

triplets'' :: Integer -> Integer -> [Triplet] -> [Triplet]
triplets'' _ 1000 acc = acc
triplets'' a b acc = (triplets''' a b 1 acc) ++ (triplets'' a (b+1) acc)

triplets''' :: Integer -> Integer -> Integer -> [Triplet] -> [Triplet]
triplets''' _ _ 1000 acc = acc
triplets''' a b c acc = (a,b,c):(triplets''' a b (c+1) acc)

length3 :: [Integer] -> Bool
length3 l = length l == 3

square :: Integer -> Integer
square x = x * x

tripletProduct :: Triplet -> Integer
tripletProduct (a,b,c) = a * b * c
