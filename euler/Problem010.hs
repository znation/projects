module Problem010 where

answer :: Integer
answer = sum (filter prime [2..2000000])

prime :: Integer -> Bool
prime = prime' 2

prime' :: Integer -> Integer -> Bool
prime' m x =    if      ((fromInteger m) :: Double) > ((sqrt (fromInteger x)) :: Double)
                then    True
                else    if      (x `mod` m) == 0
                        then    False
                        else    prime' (m+1) x
