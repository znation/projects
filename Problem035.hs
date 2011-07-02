module Problem035 where

import Utility

answer :: Int
answer = length (filter circularPrime [1..9999])

