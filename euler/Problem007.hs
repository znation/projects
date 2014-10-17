module Problem007 where

answer :: Integer
answer = (filter prime [2..]) !! 10000 -- 0-indexed, so 10001th element is index 10000
