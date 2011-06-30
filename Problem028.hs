module Problem028 where

answer :: Integer
answer = sum (diagonalNumbers (1001*1001))

diagonalNumbers :: Integer -> [Integer]
diagonalNumbers count = diagonalNumbers' count 1 1 2

diagonalNumbers' :: Integer -> Integer -> Integer -> Integer -> [Integer]
diagonalNumbers' count cornerIdx idx increment =    if      idx == count
                                                    then    [count]
                                                    else    if      cornerIdx == 4
                                                            then    idx:(diagonalNumbers' count 1 (idx+increment) (increment+2))
                                                            else    idx:(diagonalNumbers' count (cornerIdx + 1) (idx + increment) increment)
