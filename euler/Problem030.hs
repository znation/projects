module Problem030 where

import Utility

answer :: Integer
answer = sum (powerNumbers 5)

powerNumbers :: Integer -> [Integer]
powerNumbers x = filter (powerNumber x) [2..9999999]

powerNumber :: Integer -> Integer -> Bool
powerNumber p x =   let ds = digits x
                        dsum = sum (map (^p) ds)
                    in  dsum == x
