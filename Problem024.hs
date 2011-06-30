module Problem024 where

import Data.List

answer :: [Int]
answer = (permute [0..9]) !! 999999

permute :: (Eq a) => [a] -> [[a]]
permute [] = [[]]
permute str = do
    x  <- str
    xs <- permute (delete x str)
    return (x:xs)
