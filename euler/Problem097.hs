module Problem097 where

import Utility

answer :: [Integer]
answer =    let a :: Integer
                a = 28433
                b :: Integer
                b = 2
                c :: Integer
                c = 7830457
            in  reverse (take 10 (reverse (digits ((a * (b^c)) + 1))))
