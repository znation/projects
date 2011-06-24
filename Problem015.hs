module Problem015 where

answer :: Integer
answer = routes 20 20

routes :: Integer -> Integer -> Integer
routes n 1 = 1 + n
routes 1 n = 1 + n
routes x y =    if      x == y
                then    2 * (routes x (x-1))
                else    (routes y (x-1)) + (routes x (y-1))
