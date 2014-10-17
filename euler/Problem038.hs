module Problem038 where

import Utility

answer :: Integer
answer = maximum (filter pandigital concatenatedProducts)

concatenatedProducts :: [Integer]
concatenatedProducts = takeWhile (< 987654322) (map concatenatedProduct [2..])

concatenatedProduct :: Integer -> Integer
concatenatedProduct =   let concatenatedProduct' :: Integer -> Integer -> Integer
                            concatenatedProduct' x y = if (x > 987654322
