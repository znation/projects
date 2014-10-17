module Problem036 where

import Data.Char
import Numeric
import Utility

answer :: Integer
answer = sum (filter (palindromicInBases [2,10]) [0..999999])



