module Problem020 where

import Utility

answer :: Integer
answer = sum (digits (factorial 100))
