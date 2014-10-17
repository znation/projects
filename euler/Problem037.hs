module Problem037 where

import Utility

answer :: Integer
answer = sum (take 11 truncatablePrimes)

truncatablePrimes :: [Integer]
truncatablePrimes = filter truncatablePrime [8..]

truncatablePrime :: Integer -> Bool
truncatablePrime x = (prime x) && (truncatablePrimeLeft x) && (truncatablePrimeRight x)

truncatablePrimeLeft :: Integer -> Bool
truncatablePrimeLeft x = truncatablePrime' truncatablePrimeLeft (tail (show x))
                            
truncatablePrimeRight :: Integer -> Bool
truncatablePrimeRight x = truncatablePrime' truncatablePrimeRight (init (show x))

truncatablePrime' :: (Integer -> Bool) -> String -> Bool
truncatablePrime' _ [] = True
truncatablePrime' callback s =  let y :: Integer
                                    y = read s
                                in  if      prime y
                                    then    callback y
                                    else    False
