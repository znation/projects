module Problem003 where

answer :: Integer
answer = maximum (factors 600851475143)

factors :: Integer -> [Integer]
factors = factors' 2 []

factors' :: Integer -> [Integer] -> Integer -> [Integer]
factors' d acc x =  if      x == d
                    then    d:acc
                    else    let result = x `div` d
                                remainder = x `mod` d
                            in  if      remainder == 0 -- factor
                                then    let a = factors result
                                            b = factors d
                                        in  a ++ b
                                else    factors' (d+1) acc x
                                
prime :: Integer -> Bool
prime x = (length (factors x)) == 1
