module Problem015 where

answer :: Integer
answer = routes 20 20

routes :: Integer -> Integer -> Integer
routes 0 _ = 1
routes _ 0 = 1
routes x y = (routes (x-1) y) + (routes x (y-1))
