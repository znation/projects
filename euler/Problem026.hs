module Problem026 where

answer :: Int
answer =    let xs = [2..1000]
                ls = map cycleLength xs
                pairs = zip xs ls
            in  longestCycle (0, 0) pairs
            
longestCycle :: (Int, Int) -> [(Int, Int)] -> Int
longestCycle (i,_) [] = i
longestCycle (i,l) ((xi,xl):xs) =   if      xl > l
                                    then    longestCycle (xi,xl) xs
                                    else    longestCycle (i,l) xs

cycleLength :: Int -> Int
cycleLength n = let n' = removePowers 2 n
                    n'' = removePowers 5 n'
                in  if      n'' > 1
                    then    cycleLength' n'' (10 `mod` n'') 1
                    else    0

cycleLength' :: Int -> Int -> Int -> Int
cycleLength' n a l =    if      a == 1
                        then    l
                        else    let a' = (a * 10) `mod` n
                                in  cycleLength' n a' (l+1)

removePowers :: Int -> Int -> Int
removePowers p n
    | n `mod` p == 0 = removePowers p (n `div` p)
    | otherwise = n
