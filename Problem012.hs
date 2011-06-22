module Problem012 where

import Utility

answer :: Integer
answer = binarySearch

over500divisors :: Integer -> Bool
over500divisors x = (length (divisors x)) > 500

notOver500divisors :: (Integer, Integer) -> Bool
notOver500divisors x = (length (divisors (snd x))) <= 500

triangleNumber :: Integer -> Integer
triangleNumber x = sum [1..x]

-- Generates triangle numbers from an index
triangleNumbers' :: Integer -> [Integer]
triangleNumbers' x = (triangleNumber x):(triangleNumbers' (x+1))

-- Returns a list of pairs of (index, triangleNumber)
binarySearchTriangleNumbers :: [(Integer, Integer)]
binarySearchTriangleNumbers = binarySearchTriangleNumbers' 1

binarySearchTriangleNumbers' :: Integer -> [(Integer, Integer)]
binarySearchTriangleNumbers' x = (x, (triangleNumber x)):(binarySearchTriangleNumbers' (x * 2))

-- Does a binary search until we find one over 500 divisors, then binary searches from
-- the previous index to that one, until we arrive at the correct index
binarySearch :: Integer
binarySearch = binarySearch' 0 1

binarySearch' :: Integer -> Integer -> Integer
binarySearch' offset idx =  let n = debug ("triangleNumber " ++ (show (offset + idx))) (triangleNumber (offset + idx))
                                prev = triangleNumber (offset + idx - 1)
                            in  if      (over500divisors n)
                                then    if      (over500divisors prev) -- overshot
                                        then    binarySearch' (offset + (idx `div` 2)) 1
                                        else    n
                                else    binarySearch' offset (idx * 2)
                                