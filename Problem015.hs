module Problem015 where

import Data.MemoCombinators (memo2, integral)

answer :: Integer
answer = memoizedRoutes 20 20

memoizedRoutes :: Integer -> Integer -> Integer
memoizedRoutes = memo2 integral integral routes

routes :: Integer -> Integer -> Integer
routes n 1 = 1 + n
routes 1 n = 1 + n
routes x y = (memoizedRoutes y (x-1)) + (memoizedRoutes x (y-1))

