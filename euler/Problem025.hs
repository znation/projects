module Problem025 where

import Utility

answer :: Int
answer = length (takeWhile containsLessThan1000digits fibonacci)

containsLessThan1000digits :: Integer -> Bool
containsLessThan1000digits x = length (show x) < 1000
