module Problem024 where

import Data.List

answer :: String
answer = (lexicographicPermutations [0..9]) !! 1000000

lexicographicPermutations :: [Int] -> [String]
lexicographicPermutations xs = map (concat . (map show)) (sort (permutations xs))

